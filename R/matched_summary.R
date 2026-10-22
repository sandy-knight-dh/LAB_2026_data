# Matched-cohort summary data -------------------------------------------------
#
# The summary data's counts and rates with propensity score matched
# comparators, written to output/ as CSV files (R/outputs.R). Each
# indicator's population in each financial year is the summary data's
# (R/summary_data.R), and its LAB clients are matched within it to methadone
# and oral buprenorphine clients (match_population(), R/matching.R). So
# every definition is the summary data's: clients counted once by their
# latest journey, opiate clients only, OST type in the period, the same
# financial-year windows.
#
# - LAB: nearly every LAB client finds a match, so the LAB figures are the
#   summary data's. The comparator figures change: they are for methadone and
#   oral buprenorphine clients who resemble the LAB clients, including in how
#   long they have been in treatment.
# - Populations, each matched separately in each year:
#   - In treatment (in_treatment()): counts, completions and deaths.
#   - New presentations 3 months before the year (early_exit_cohort()): early
#     unplanned exits.
#   - Successful completions with the follow-up in the data
#     (completions_with_followup()): re-presentation at 6 and 12 months.
#   - In treatment over the whole period (WHOLE_PERIOD): the characteristics
#     table.
# - Weights: counts and rates are sums of the matching weights.
# - The summary data's caveats carry over unchanged. Early unplanned exits
#   favour LAB in the earlier years, when LAB was mostly started months into
#   a journey and so recorded too late for an early exit to count as LAB.

# Matching each year ----------------------------------------------------------

#' A summary-data population matched in each financial year
#' @param journeys tibble. From summary_journeys()
#' @param covariates tibble. From journey_covariates()
#' @param population function(journeys, period_start, period_end, ...). A
#'   population from R/summary_data.R, e.g. in_treatment
#' @param ... Passed to population, e.g. months for
#'   completions_with_followup()
#' @return list. people (tibble: year, period_start, period_end, then the
#'   matched clients' rows from match_population()) and summary (tibble:
#'   year, then one row per comparator from match_population())
matched_by_year <- function(journeys, covariates, population, ...) {
  matches <- purrr::pmap(
    FINANCIAL_YEARS,
    \(year, period_start, period_end) {
      match <- journeys |>
        population(period_start, period_end, ...) |>
        match_population(covariates)

      list(
        people = dplyr::mutate(
          match$people,
          year = year,
          period_start = period_start,
          period_end = period_end,
          .before = 1
        ),
        summary = dplyr::mutate(match$summary, year = year, .before = 1)
      )
    }
  )

  list(
    people = purrr::list_rbind(purrr::map(matches, "people")),
    summary = purrr::list_rbind(purrr::map(matches, "summary"))
  )
}

#' The match summaries of every matched population: how many were matched
#' and how well
#' @param ... list. Named matched populations, from matched_by_year() or
#'   match_population()
#' @return tibble. population, year (NA for the whole period), then the
#'   columns of match_summary() and exact_region
match_summaries <- function(...) {
  list(...) |>
    purrr::map(\(match) match$summary) |>
    purrr::list_rbind(names_to = "population")
}

# Counting --------------------------------------------------------------------

#' Weighted count by year and group
#' @param people tibble. Matched clients, with year, ost_type and weights
#' @return tibble. year, ost_type (factor, OST_TYPES), n (numeric, sum of
#'   weights)
weighted_count_by_year <- function(people) {
  people |>
    dplyr::summarise(n = sum(weights), .by = c(year, ost_type)) |>
    dplyr::mutate(ost_type = factor(ost_type, OST_TYPES)) |>
    tidyr::complete(year, ost_type, fill = list(n = 0)) |>
    dplyr::arrange(year, ost_type)
}

#' Weighted rate by year and group
#' @param people tibble. Matched clients (the denominator), with year,
#'   ost_type and weights
#' @param event logical column in people. The numerator; NA counts as FALSE
#' @return tibble. year, ost_type (factor, OST_TYPES), numerator, denominator
#'   (numeric, sums of weights), rate (%)
weighted_rate_by_year <- function(people, event) {
  people |>
    dplyr::mutate(event = dplyr::coalesce({{ event }}, FALSE)) |>
    dplyr::summarise(
      numerator = sum(weights * event),
      denominator = sum(weights),
      .by = c(year, ost_type)
    ) |>
    dplyr::mutate(ost_type = factor(ost_type, OST_TYPES)) |>
    tidyr::complete(
      year,
      ost_type,
      fill = list(numerator = 0, denominator = 0)
    ) |>
    dplyr::mutate(rate = 100 * numerator / denominator) |>
    dplyr::arrange(year, ost_type)
}

# Indicators ------------------------------------------------------------------
#
# Each mirrors the summary indicator of the same name in R/summary_data.R, on
# the matched clients.

#' Matched clients in treatment in each financial year
#' @param matched list. From matched_by_year() with in_treatment()
#' @return tibble. year, ost_type, n
matched_in_treatment_counts <- function(matched) {
  weighted_count_by_year(matched$people)
}

#' Successful completions in each financial year among matched clients in
#' treatment, as in successful_completions()
#' @param matched list. From matched_by_year() with in_treatment()
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_successful_completions <- function(matched) {
  matched$people |>
    dplyr::mutate(
      completed_in_period = successful_completion &
        dplyr::between(disd_jy, period_start, period_end)
    ) |>
    weighted_rate_by_year(completed_in_period)
}

#' Deaths in treatment in each financial year among matched clients in
#' treatment, as in deaths_in_treatment()
#' @param matched list. From matched_by_year() with in_treatment()
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_deaths <- function(matched) {
  matched$people |>
    dplyr::mutate(
      died_in_period = died &
        dplyr::between(disd_jy, period_start, period_end)
    ) |>
    weighted_rate_by_year(died_in_period)
}

#' Early unplanned exits among matched new presentations, as in
#' early_unplanned_exits()
#' @param matched list. From matched_by_year() with early_exit_cohort()
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_early_unplanned_exits <- function(matched) {
  weighted_rate_by_year(matched$people, early_unplanned_exit)
}

#' Re-presentation among matched successful completions, as in
#' re_presentations(). Years with no completions with the follow-up have no
#' rows
#' @param matched list. From matched_by_year() with
#'   completions_with_followup()
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_re_presentations <- function(matched) {
  weighted_rate_by_year(matched$people, re_presented)
}

# Characteristics -------------------------------------------------------------

#' Table 1 of the matched clients in treatment over the whole period, as
#' summary_characteristics(). Unweighted: the matching weights are almost all
#' 1
#' @param matched list. From match_population() on the whole period's
#'   in_treatment()
#' @param df data.frame. Linked dataset
#' @return tibble. From make_table1(); age at the period start or journey
#'   start, whichever is later
matched_characteristics <- function(matched, df) {
  summary_characteristics(matched$people, df)
}
