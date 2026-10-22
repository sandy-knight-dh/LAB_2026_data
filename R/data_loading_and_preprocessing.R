# Journeys --------------------------------------------------------------------
#
# The linked dataset (DATA_PATH, read with arrow::read_feather()) has one row
# per journey x episode x modality, so it is collapsed to one row per journey
# (client_random_id + n_jy) first.
#
# collapse_to_journeys() works in data.table through dtplyr: dtplyr::lazy_dt()
# translates the dplyr verbs and tibble::as_tibble() runs them. dtplyr is used
# only here and in the other functions that summarise the raw dataset or group
# by client, where plain dplyr is many times slower (timings in each
# function's comment). Everything on the journey table is plain dplyr.
#
# - Pre-2022 journeys must be kept: many people in treatment in recent periods
#   are on journeys that started before 2022.
# - All clients are treated as opiate clients; drug.grp_sml is not filtered.

JOURNEY_COLS <- c(
  "client_random_id",
  "n_jy",
  "triaged_jy",
  "disd_jy",
  "age_jy_start",
  "ever_lab",
  "first_lab_date",
  "last_lab_date",
  "ever_methadone",
  "ever_oral_buprenorphine"
)

#' @param df data.frame. Linked dataset
#' @param group_cols character vector. Extra columns to keep, e.g. "region".
#'   Must be constant within a journey
#' @return tibble. One row per journey (client_random_id + n_jy)
collapse_to_journeys <- function(df, group_cols = NULL) {
  journeys <- dtplyr::lazy_dt(df) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(JOURNEY_COLS, group_cols)))
    ) |>
    tibble::as_tibble()

  # Counting 700,000 journeys by client takes 6 s in plain dplyr, under 1 s
  # in dtplyr
  n_split <- dtplyr::lazy_dt(journeys) |>
    dplyr::count(client_random_id, n_jy) |>
    dplyr::filter(n > 1) |>
    tibble::as_tibble() |>
    nrow()

  if (n_split > 0) {
    stop(
      "group_cols vary within ", n_split, " journeys: ",
      paste(group_cols, collapse = ", "),
      ". Use journey-level columns such as region, utla_final, dat or sex."
    )
  }

  journeys
}

# In treatment ----------------------------------------------------------------
#
# In treatment follows the dashboard definition: journey start (triaged_jy) on
# or before the period end, and journey end (disd_jy) on or after the period
# start or ongoing (NA). Adults are 18+ at the period start or journey start,
# whichever is later. There is no date of birth, so age is approximated as
# age_jy_start plus whole years elapsed since triage. Used by both decks.

#' @param journeys tibble of journeys from collapse_to_journeys()
#' @param period_start,period_end Date(1)
#' @return tibble. Adults' journeys open in the period, with ref_date (Date)
#'   and age_at_ref (numeric) added
journeys_in_period <- function(journeys, period_start, period_end) {
  journeys |>
    dplyr::filter(
      triaged_jy <= period_end,
      is.na(disd_jy) | disd_jy >= period_start
    ) |>
    dplyr::mutate(
      ref_date = pmax(period_start, triaged_jy),
      age_at_ref = age_jy_start +
        floor(as.numeric(ref_date - triaged_jy, units = "days") / 365.25)
    ) |>
    dplyr::filter(age_at_ref >= 18)
}
