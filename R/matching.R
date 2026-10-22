# Propensity score matching ---------------------------------------------------
#
# The matched data compares LAB clients with methadone and oral buprenorphine
# clients who look like them. Each population of the summary data (clients in
# treatment in a year, new presentations, completions) is matched within
# itself, so a matched rate is the summary data's rate with comparators made
# to resemble the LAB clients. The estimand is the average treatment effect
# in the treated (ATT): every LAB client is kept where a comparator can be
# found.
#
# - Groups: OST type in the period (add_ost_type()), as in the summary data.
#   Clients with another OST type (mixed oral OST, not recorded, no
#   prescribing) are not matched.
# - Propensity model: logistic regression of LAB on MATCHING_COVARIATES
#   (R/covariates.R), leaving out any covariate with one value in the
#   population.
# - Matching: nearest neighbour on the logit of the propensity score, without
#   replacement, PSM_RATIO comparators per LAB client (from the plan: 1:3 for
#   methadone, 1:2 for oral buprenorphine), capped where fewer are available,
#   within a caliper of 0.2 standard deviations of the logit. LAB clients
#   with no comparator inside the caliper are dropped and counted
#   (match_summary()).
# - Balance: standardised mean differences after matching for every
#   covariate level (cobalt::bal.tab()), against a threshold of 0.1. If any
#   region is out of balance, the match is redone with exact matching on
#   region, as the plan says, and the summary records it.
# - Weights: MatchIt's matching weights. LAB clients have weight 1; a
#   comparator's weight is smaller where its LAB client found fewer than the
#   full ratio inside the caliper.

PSM_RATIO <- c(methadone = 3, oral_buprenorphine = 2)
PSM_CALIPER <- 0.2
PSM_SMD_THRESHOLD <- 0.1

# Matching --------------------------------------------------------------------

#' Nearest-neighbour propensity score matching of LAB to one comparator
#' @param people tibble. One row per client, from add_covariates(), with
#'   ost_type
#' @param comparator character(1). "methadone" or "oral_buprenorphine"
#' @param exact_region logical(1). Match exactly on region as well
#' @return list. matched (tibble: the matched clients' rows of people, with
#'   lab (integer 0/1) and weights), balance (from match_balance()) and
#'   summary (from match_summary())
match_clients <- function(people, comparator, exact_region = FALSE) {
  clients <- people |>
    dplyr::filter(ost_type %in% c("lab", comparator)) |>
    dplyr::mutate(lab = as.integer(ost_type == "lab")) |>
    # Unused factor levels would give an empty column in the propensity model
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), droplevels))

  # A covariate with one value (time in treatment among new presentations)
  # can't enter the propensity model
  covariates <- purrr::keep(
    MATCHING_COVARIATES,
    \(covariate) dplyr::n_distinct(clients[[covariate]]) > 1
  )

  available <- floor(sum(clients$lab == 0) / sum(clients$lab == 1))
  ratio <- max(1, min(PSM_RATIO[[comparator]], available))

  match <- MatchIt::matchit(
    stats::reformulate(covariates, response = "lab"),
    data = as.data.frame(clients),
    method = "nearest",
    distance = "glm",
    link = "linear.logit",
    estimand = "ATT",
    ratio = ratio,
    replace = FALSE,
    caliper = PSM_CALIPER,
    std.caliper = TRUE,
    exact = if (exact_region) ~region else NULL
  )

  matched <- clients |>
    dplyr::mutate(weights = match$weights) |>
    dplyr::filter(weights > 0)
  balance <- match_balance(match)

  list(
    matched = matched,
    balance = balance,
    summary = match_summary(clients, matched, balance, comparator, ratio)
  )
}

#' Standardised mean differences before and after matching, every covariate
#' level
#' @param match matchit. From MatchIt::matchit()
#' @return tibble. covariate (character), smd_before, smd_after (numeric),
#'   balanced (logical, |smd_after| < PSM_SMD_THRESHOLD)
match_balance <- function(match) {
  balance <- cobalt::bal.tab(
    match,
    un = TRUE,
    binary = "std",
    continuous = "std"
  )$Balance

  tibble::tibble(
    covariate = rownames(balance),
    smd_before = balance$Diff.Un,
    smd_after = balance$Diff.Adj
  ) |>
    dplyr::filter(covariate != "distance") |>
    dplyr::mutate(balanced = abs(smd_after) < PSM_SMD_THRESHOLD)
}

#' How many clients were matched, and how well
#' @param clients tibble. LAB and comparator clients, with lab
#' @param matched tibble. The matched clients
#' @param balance tibble. From match_balance()
#' @param comparator character(1)
#' @param ratio integer(1)
#' @return tibble. comparator, ratio, lab, lab_matched, lab_unmatched,
#'   comparator_available, comparator_matched, max_smd_after
match_summary <- function(clients, matched, balance, comparator, ratio) {
  tibble::tibble(
    comparator = comparator,
    ratio = ratio,
    lab = sum(clients$lab == 1),
    lab_matched = sum(matched$lab == 1),
    lab_unmatched = lab - lab_matched,
    comparator_available = sum(clients$lab == 0),
    comparator_matched = sum(matched$lab == 0),
    max_smd_after = max(abs(balance$smd_after))
  )
}

#' Matching, with the exact-region fallback
#' @param people tibble. As match_clients()
#' @param comparator character(1)
#' @return list. From match_clients(), with exact_region (logical) in its
#'   summary
match_with_fallback <- function(people, comparator) {
  match <- match_clients(people, comparator)
  region_rows <- grepl("^region", match$balance$covariate)
  exact_region <- !all(match$balance$balanced[region_rows])
  if (exact_region) {
    match <- match_clients(people, comparator, exact_region = TRUE)
  }
  match$summary$exact_region <- exact_region
  match
}

# Matched populations ---------------------------------------------------------
#
# A population is matched twice, LAB against each comparator. Nearly every
# LAB client is in both matches; the LAB group shown is the LAB side of the
# methadone match.

#' Matches a summary-data population's LAB clients to each comparator
#' @param people tibble. One row per client, from a population in
#'   R/summary_data.R, with ost_type
#' @param covariates tibble. From journey_covariates()
#' @return list. people (tibble: the matched clients' rows of people, with
#'   its columns unchanged, ost_type lab, methadone or oral_buprenorphine, and
#'   weights; no rows if the population has no LAB clients) and summary
#'   (tibble: one row per comparator, from match_with_fallback(); no rows if
#'   no LAB clients)
match_population <- function(people, covariates) {
  if (!any(people$ost_type == "lab")) {
    return(list(
      people = dplyr::mutate(people[0, ], weights = numeric()),
      summary = tibble::tibble()
    ))
  }

  with_covariates <- add_covariates(people, covariates)
  methadone <- match_with_fallback(with_covariates, "methadone")
  oral_buprenorphine <- match_with_fallback(
    with_covariates,
    "oral_buprenorphine"
  )

  # The covariates are dropped again, so the matched clients have the same
  # columns as the population (and as the summary data's clients)
  list(
    people = dplyr::bind_rows(
      dplyr::filter(methadone$matched, lab == 1),
      dplyr::filter(methadone$matched, lab == 0),
      dplyr::filter(oral_buprenorphine$matched, lab == 0)
    ) |>
      dplyr::select(dplyr::all_of(names(people)), weights),
    summary = dplyr::bind_rows(methadone$summary, oral_buprenorphine$summary)
  )
}
