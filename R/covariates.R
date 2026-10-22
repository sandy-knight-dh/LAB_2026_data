# Covariates ------------------------------------------------------------------
#
# What the matched deck matches LAB clients to comparators on
# (R/matching.R). Each summary-deck population is matched within itself, so
# the covariates are added to its clients (one row per client, their latest
# journey in the period) rather than to a model cohort.
#
# MATCHING_COVARIATES, as model columns (reference level first):
# - age: at the period start or journey start, whichever is later, as the
#   population gives it (numeric, linear).
# - sex: M, F.
# - region: the journey's region, with Missing for the few journeys without
#   one.
# - housing: housing group at the journey's first assessment
#   (baseline_housing()), Rented home as reference, with Missing: housing was
#   not collected before April 2022, so most clients whose journey started
#   earlier have none.
# - crack, benzo: use at assessment (drug_a.crack, drug_a.benzo), No/Yes.
# - prev_journeys: previous treatment journeys, None (treatment-naive), 1,
#   2-4, 5+.
# - time_out: time out of treatment before this journey, Under 6 months, 6-12
#   months, 1-2 years, 2 years or more. Overlapping journeys (negative time
#   out) count as under 6 months.
# - prior_treatment: total time in treatment on earlier journeys, Under 1
#   year, 1-3 years, 3-6 years, 6 years or more.
# - time_in_treatment: time on this journey by the reference date (ref_date:
#   the period start, or the journey start if later), Started in the period,
#   Under 1 year, 1-2 years, 2-5 years, 5 years or more. This makes the
#   comparators as long in treatment as the LAB clients; without it the
#   comparators would include the long-term maintenance clients who rarely
#   complete. Where everyone started in the period (new presentations) it has
#   one level and is left out of the match.
# Treatment-naive journeys have no time out or prior time in treatment. They
# are put in the reference level of time_out and prior_treatment, and picked
# out by prev_journeys == "None"; giving them their own level in all three
# would make the three variables collinear.
#
# Not available in the extract, so not matched on: ethnicity, age at first
# use (length of opioid use), TOP use frequency, injecting at baseline,
# supervised consumption.

MATCHING_COVARIATES <- c(
  "age",
  "sex",
  "region",
  "housing",
  "crack",
  "benzo",
  "prev_journeys",
  "time_out",
  "prior_treatment",
  "time_in_treatment"
)

# Journey-level columns for the covariates, on top of JOURNEY_COLS; the
# summary journey table has the rest (sex, region, drug_a.crack, triaged_jy)
COVARIATE_COLS <- c(
  "drug_a.benzo",
  "n_prev_journeys",
  "previous_time_out_of_tx_triage",
  "los.days"
)

# Journey covariates ----------------------------------------------------------

#' The covariates each journey needs that the summary journey table doesn't
#' have
#' @param df data.frame. Linked dataset
#' @return tibble. One row per journey: client_random_id, n_jy, drug_a.benzo,
#'   n_prev_journeys, previous_time_out_of_tx_triage, prior_treatment_days
#'   (numeric) and housing_baseline (character, NA where not recorded)
journey_covariates <- function(df) {
  journeys <- collapse_to_journeys(df, group_cols = COVARIATE_COLS)

  journeys |>
    dplyr::left_join(
      treatment_history(journeys),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::left_join(
      baseline_housing(df),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::select(
      client_random_id,
      n_jy,
      drug_a.benzo,
      n_prev_journeys,
      previous_time_out_of_tx_triage,
      prior_treatment_days,
      housing_baseline
    )
}

# Treatment history -----------------------------------------------------------

#' @param journeys tibble. One row per journey with client_random_id, n_jy,
#'   n_prev_journeys and los.days (difftime), from collapse_to_journeys()
#' @return tibble. client_random_id, n_jy, prior_treatment_days (numeric, sum
#'   of los.days over the client's earlier journeys; 0 if none) and
#'   treatment_naive (logical, n_prev_journeys == 0)
treatment_history <- function(journeys) {
  dtplyr::lazy_dt(journeys) |>
    dplyr::mutate(
      los = dplyr::coalesce(as.numeric(los.days, units = "days"), 0)
    ) |>
    dplyr::arrange(client_random_id, n_jy) |>
    dplyr::mutate(
      prior_treatment_days = cumsum(los) - los,
      .by = client_random_id
    ) |>
    dplyr::transmute(
      client_random_id,
      n_jy,
      prior_treatment_days,
      treatment_naive = n_prev_journeys == 0
    ) |>
    tibble::as_tibble()
}

# Model columns ---------------------------------------------------------------

#' Adds the matching covariates to a summary-deck population
#' @param people tibble. One row per client, from a population in
#'   R/summary_data.R (in_treatment(), new_presentations() and so on), with
#'   age, sex, region, drug_a.crack, triaged_jy and ref_date
#' @param covariates tibble. From journey_covariates()
#' @return people with the MATCHING_COVARIATES columns added (factors, except
#'   age)
add_covariates <- function(people, covariates) {
  people |>
    dplyr::left_join(covariates, by = c("client_random_id", "n_jy")) |>
    dplyr::mutate(
      sex = factor(sex, c("M", "F")),
      region = factor(dplyr::coalesce(region, "Missing")),
      housing = factor(
        dplyr::coalesce(housing_baseline, "Missing"),
        c(
          "Rented home", "Home owner", "Other home", "NFA", "Rough sleeping",
          "Missing"
        )
      ),
      # "No" first, as the reference level
      crack = factor(
        dplyr::if_else(drug_a.crack == 1, "Yes", "No"),
        levels = c("No", "Yes")
      ),
      benzo = factor(
        dplyr::if_else(drug_a.benzo == 1, "Yes", "No"),
        levels = c("No", "Yes")
      ),
      prev_journeys = dplyr::case_when(
        n_prev_journeys == 0 ~ "None",
        n_prev_journeys == 1 ~ "1",
        n_prev_journeys <= 4 ~ "2-4",
        .default = "5+"
      ) |>
        factor(c("None", "1", "2-4", "5+")),
      time_out_days = as.numeric(
        previous_time_out_of_tx_triage,
        units = "days"
      ),
      time_out = dplyr::case_when(
        n_prev_journeys == 0 | time_out_days < 183 ~ "Under 6 months",
        time_out_days < 365 ~ "6-12 months",
        time_out_days < 730 ~ "1-2 years",
        .default = "2 years or more"
      ) |>
        factor(c(
          "Under 6 months", "6-12 months", "1-2 years", "2 years or more"
        )),
      prior_treatment = dplyr::case_when(
        prior_treatment_days < 365.25 ~ "Under 1 year",
        prior_treatment_days < 3 * 365.25 ~ "1-3 years",
        prior_treatment_days < 6 * 365.25 ~ "3-6 years",
        .default = "6 years or more"
      ) |>
        factor(c("Under 1 year", "1-3 years", "3-6 years", "6 years or more")),
      days_in_treatment = as.numeric(ref_date - triaged_jy, units = "days"),
      time_in_treatment = dplyr::case_when(
        days_in_treatment <= 0 ~ "Started in the period",
        days_in_treatment < 365 ~ "Under 1 year",
        days_in_treatment < 730 ~ "1-2 years",
        days_in_treatment < 1826 ~ "2-5 years",
        .default = "5 years or more"
      ) |>
        factor(c(
          "Started in the period", "Under 1 year", "1-2 years", "2-5 years",
          "5 years or more"
        ))
    ) |>
    dplyr::select(-time_out_days, -days_in_treatment)
}
