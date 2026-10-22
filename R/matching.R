
PSM_RATIO <- c(methadone = 3, oral_buprenorphine = 2)
PSM_CALIPER <- 0.2
PSM_SMD_THRESHOLD <- 0.1

# Matching --------------------------------------------------------------------

#' One row per journey for matching: treated (LAB, including switchers) or the
#' comparator, with its time-zero covariates
#' @param data tibble. A model dataset; may have several rows per journey
#'   (prepare_completion_tte_data())
#' @param comparator character(1). "methadone" or "oral_buprenorphine"
#' @return tibble. client_random_id, n_jy, lab (integer 0/1), region and
#'   BASELINE_COVARIATES
matching_journeys <- function(data, comparator) {
  data |>
    one_row_per_journey() |>
    dplyr::mutate(lab = as.integer(exposure_group == "lab")) |>
    dplyr::filter(lab == 1 | exposure_group == comparator) |>
    dplyr::select(
      client_random_id, n_jy, lab, region, dplyr::all_of(BASELINE_COVARIATES)
    ) |>
    # Unused factor levels would give an empty column in the propensity model
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), droplevels))
}

#' Nearest-neighbour propensity score matching of LAB to one comparator
#' @param data tibble. A model dataset (see matching_journeys())
#' @param comparator character(1). "methadone" or "oral_buprenorphine"
#' @param exact_region logical(1). Match exactly on region as well
#' @return list. matched (tibble: client_random_id, n_jy, lab, weights,
#'   subclass), balance (from match_balance()), summary (from
#'   match_summary()), comparator
match_cohort <- function(data, comparator, exact_region = FALSE) {
  journeys <- matching_journeys(data, comparator)
  available <- floor(sum(journeys$lab == 0) / sum(journeys$lab == 1))
  ratio <- max(1, min(PSM_RATIO[[comparator]], available))

  match <- MatchIt::matchit(
    stats::reformulate(c(BASELINE_COVARIATES, "region"), response = "lab"),
    data = as.data.frame(journeys),
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

  matched <- MatchIt::match.data(match, drop.unmatched = TRUE) |>
    tibble::as_tibble() |>
    dplyr::select(client_random_id, n_jy, lab, weights, subclass)

  list(
    matched = matched,
    balance = match_balance(match),
    summary = match_summary(journeys, matched, comparator, ratio),
    comparator = comparator
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

#' How many journeys were matched
#' @param journeys tibble. From matching_journeys()
#' @param matched tibble. Matched journeys
#' @param comparator character(1)
#' @param ratio integer(1)
#' @return tibble. comparator, ratio, lab, lab_matched, lab_unmatched,
#'   comparator_available, comparator_matched
match_summary <- function(journeys, matched, comparator, ratio) {
  tibble::tibble(
    comparator = comparator,
    ratio = ratio,
    lab = sum(journeys$lab == 1),
    lab_matched = sum(matched$lab == 1),
    lab_unmatched = lab - lab_matched,
    comparator_available = sum(journeys$lab == 0),
    comparator_matched = sum(matched$lab == 0)
  )
}

# Matches ---------------------------------------------------------------------
#
# One match per comparator and dataset. If any region is out of balance after
# matching, the match is redone with exact matching on region, as the plan
# says, and the summary records it. In the pipeline each match is a target,
# match_<comparator>_<dataset>.

#' Matches, with the exact-region fallback
#' @param data tibble. A model dataset
#' @param comparator character(1)
#' @param dataset character(1). The dataset's name, kept with the match to
#'   label its balance and summary
#' @return list. From match_cohort(), with exact_region (logical) in its
#'   summary, and dataset
match_with_fallback <- function(data, comparator, dataset) {
  match <- match_cohort(data, comparator)
  region_rows <- grepl("^region", match$balance$covariate)
  exact_region <- !all(match$balance$balanced[region_rows])
  if (exact_region) {
    match <- match_cohort(data, comparator, exact_region = TRUE)
  }
  match$summary$exact_region <- exact_region
  match$dataset <- dataset
  match
}
