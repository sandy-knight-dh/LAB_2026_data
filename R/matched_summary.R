# Matched-cohort summary data -------------------------------------------------
#
# The summary deck's counts and rates, computed on the propensity score
# matched journeys instead of all adult opiate clients, for
# write_matched_summary_deck(). The matches are made in R/matching.R.
#
# - Journeys: the time-to-event matches (match_<comparator>_completion_tte),
#   which cover every journey with a time zero from April 2022, open or
#   closed, with switchers as LAB. Re-presentation uses its own matches
#   (match_<comparator>_representation), of journeys that ended in successful
#   completion.
# - Groups: LAB, matched methadone and matched oral buprenorphine. LAB is
#   matched twice, once against each comparator, and nearly every LAB journey
#   is in both. The LAB group is the LAB side of the methadone match.
# - Definitions: each matched journey is joined to the summary journey table
#   (summary_journeys()), and each indicator uses the summary deck's
#   definition and financial-year window, so a matched rate is the summary
#   deck's rate restricted to matched journeys. Two differences remain:
#   - The unit is the journey, not the client: a client with two matched
#     journeys open in a year counts twice.
#   - The group is the journey's matched exposure (OST at any point in the
#     journey), not OST in the period.
# - Weights: counts and rates are weighted by the matching weights (sums of
#   weights). They are almost all 1 in the time-to-event matches; in the
#   re-presentation matches, where there are only about two comparators per
#   LAB journey, they vary more.
# - Early unplanned exits are biased in LAB's favour: a journey is LAB only
#   once LAB is recorded, a median of 84 days after prescribing starts, so
#   few LAB journeys can exit within 12 weeks.
# - The matched journeys start in April 2022, so 2022/23 has no journeys that
#   started earlier and its in-treatment counts are low. With the 3-month
#   delay, early unplanned exits for 2022/23 cover April to December 2022.

# Matched journeys ------------------------------------------------------------

#' One side of a match, labelled with its group
#' @param match list. From match_with_fallback()
#' @param side integer(1). 1 for the LAB side, 0 for the comparator side
#' @param group character(1). Value for ost_type
#' @return tibble. client_random_id, n_jy, ost_type, weights
matched_side <- function(match, side, group) {
  match$matched |>
    dplyr::filter(lab == side) |>
    dplyr::transmute(client_random_id, n_jy, ost_type = group, weights)
}

#' The matched journeys of one dataset, with their group and weight
#' @param match_methadone list. From match_with_fallback(), LAB against
#'   methadone
#' @param match_oral_buprenorphine list. From match_with_fallback(), LAB
#'   against oral buprenorphine, on the same dataset
#' @return tibble. client_random_id, n_jy, ost_type (character: lab,
#'   methadone or oral_buprenorphine), weights (numeric). LAB journeys come
#'   from the methadone match
matched_groups <- function(match_methadone, match_oral_buprenorphine) {
  dplyr::bind_rows(
    matched_side(match_methadone, side = 1, group = "lab"),
    matched_side(match_methadone, side = 0, group = "methadone"),
    matched_side(
      match_oral_buprenorphine,
      side = 0,
      group = "oral_buprenorphine"
    )
  )
}

#' @param journeys tibble. From summary_journeys()
#' @param groups tibble. From matched_groups()
#' @return tibble. The matched journeys' rows of journeys, with ost_type and
#'   weights
join_matched_groups <- function(journeys, groups) {
  dplyr::inner_join(journeys, groups, by = c("client_random_id", "n_jy"))
}

# Counting --------------------------------------------------------------------

#' Weighted count by group
#' @param data tibble. Matched journeys, with ost_type and weights
#' @return tibble. ost_type (factor, OST_TYPES), n (numeric, sum of weights)
weighted_count_by_group <- function(data) {
  data |>
    dplyr::summarise(n = sum(weights), .by = ost_type) |>
    dplyr::mutate(ost_type = factor(ost_type, OST_TYPES)) |>
    tidyr::complete(ost_type, fill = list(n = 0)) |>
    dplyr::arrange(ost_type)
}

#' Weighted rate by group
#' @param data tibble. Matched journeys (the denominator), with ost_type and
#'   weights
#' @param event logical column in data. The numerator; NA counts as FALSE
#' @return tibble. ost_type (factor, OST_TYPES), numerator, denominator
#'   (numeric, sums of weights), rate (%)
weighted_rate_by_group <- function(data, event) {
  data |>
    dplyr::mutate(event = dplyr::coalesce({{ event }}, FALSE)) |>
    dplyr::summarise(
      numerator = sum(weights * event),
      denominator = sum(weights),
      .by = ost_type
    ) |>
    dplyr::mutate(ost_type = factor(ost_type, OST_TYPES)) |>
    tidyr::complete(ost_type, fill = list(numerator = 0, denominator = 0)) |>
    dplyr::mutate(rate = 100 * numerator / denominator) |>
    dplyr::arrange(ost_type)
}

# Indicators ------------------------------------------------------------------
#
# Each mirrors the summary indicator of the same name in R/summary_data.R.

#' Matched journeys open at any time in each financial year
#' @param matched tibble. From join_matched_groups(), time-to-event matches
#' @return tibble. year, ost_type, n
matched_in_treatment_counts <- function(matched) {
  for_each_year(\(period_start, period_end) {
    matched |>
      journeys_in_period(period_start, period_end) |>
      weighted_count_by_group()
  })
}

#' Early unplanned exits among matched journeys starting in each financial
#' year, delayed by 3 months as in early_unplanned_exits()
#' @param matched tibble. From join_matched_groups(), time-to-event matches
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_early_unplanned_exits <- function(matched) {
  for_each_year(\(period_start, period_end) {
    matched |>
      dplyr::filter(
        dplyr::between(
          triaged_jy,
          period_start - EARLY_EXIT_DELAY,
          period_end - EARLY_EXIT_DELAY
        ),
        age_jy_start >= 18
      ) |>
      weighted_rate_by_group(early_unplanned_exit)
  })
}

#' Successful completions in each financial year among matched journeys open
#' in it, as in successful_completions()
#' @param matched tibble. From join_matched_groups(), time-to-event matches
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_successful_completions <- function(matched) {
  for_each_year(\(period_start, period_end) {
    matched |>
      journeys_in_period(period_start, period_end) |>
      dplyr::mutate(
        completed_in_period = successful_completion &
          dplyr::between(disd_jy, period_start, period_end)
      ) |>
      weighted_rate_by_group(completed_in_period)
  })
}

#' Deaths in treatment in each financial year among matched journeys open in
#' it, as in deaths_in_treatment()
#' @param matched tibble. From join_matched_groups(), time-to-event matches
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_deaths <- function(matched) {
  for_each_year(\(period_start, period_end) {
    matched |>
      journeys_in_period(period_start, period_end) |>
      dplyr::mutate(
        died_in_period = died &
          dplyr::between(disd_jy, period_start, period_end)
      ) |>
      weighted_rate_by_group(died_in_period)
  })
}

#' Re-presentation within 6 or 12 months of a successful completion in each
#' financial year, among matched completions with the full follow-up, as in
#' re_presentations(). Years with no such completions are dropped
#' @param matched tibble. From join_matched_groups(), re-presentation matches
#' @param months 6 or 12
#' @return tibble. year, ost_type, numerator, denominator, rate
matched_re_presentations <- function(matched, months) {
  followup <- paste0("followup_available_", months, "m")
  repres <- paste0("repres_", months, "m")

  for_each_year(\(period_start, period_end) {
    matched |>
      journeys_in_period(period_start, period_end) |>
      dplyr::filter(
        successful_completion,
        dplyr::between(disd_jy, period_start, period_end),
        .data[[followup]] == 1
      ) |>
      dplyr::mutate(re_presented = .data[[repres]] == 1) |>
      weighted_rate_by_group(re_presented)
  }) |>
    dplyr::filter(sum(denominator) > 0, .by = year)
}

# Characteristics -------------------------------------------------------------

#' Table 1 of the matched journeys, by group. Unweighted: the time-to-event
#' matching weights are almost all 1
#' @param data tibble. The time-to-event model dataset (data_completion_tte)
#' @param groups tibble. From matched_groups(), time-to-event matches
#' @param df data.frame. Linked dataset
#' @return tibble. From model_table1(); age at time zero
matched_characteristics <- function(data, groups, df) {
  data |>
    dplyr::select(-exposure_group) |>
    dplyr::inner_join(
      dplyr::select(groups, client_random_id, n_jy, exposure_group = ost_type),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::mutate(exposure_group = factor(exposure_group, OST_TYPES)) |>
    model_table1(df)
}
