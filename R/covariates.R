# Covariates ------------------------------------------------------------------
#
# Baseline covariates for the propensity score, added to both model datasets
# by prepare_cohort(). Baseline means measured at or before the journey's
# start (triage), so before exposure.
#
# BASELINE_COVARIATES, as model columns (reference level first):
# - age: at time zero, years (numeric, linear).
# - sex: M, F.
# - triage_year: year the journey started, 2022 (reference), 2023, 2024,
#   2025+ (2026 has only January to March). LAB availability grew over time.
# - housing: housing group at the journey's first assessment
#   (baseline_housing()), with Rented home as reference. Not recorded at all
#   for journeys triaged before April 2022 (the field wasn't collected), 17%
#   missing in April 2022 and 2 to 5% after that. Journeys without it are
#   dropped (prepare_cohort()), which in practice starts the cohort in April
#   2022, four full financial years; it loses 5.4% of LAB journeys, under the
#   plan's 10% threshold for multiple imputation.
# - crack, benzo: use at assessment (drug_a.crack, drug_a.benzo), No/Yes.
# - prev_journeys: previous treatment journeys, None (treatment-naive), 1,
#   2-4, 5+.
# - time_out: time out of treatment before this journey, Under 6 months, 6-12
#   months, 1-2 years, 2 years or more. Overlapping journeys (negative time
#   out) count as under 6 months.
# - prior_treatment: total time in treatment on earlier journeys, Under 1
#   year, 1-3 years, 3-6 years, 6 years or more.
# Treatment-naive journeys have no time out or prior time in treatment. They
# are put in the reference level of time_out and prior_treatment, and picked
# out by prev_journeys == "None"; giving them their own level in all three
# would make the three variables collinear. Table 1 shows them separately.
#
# Not available in the extract, so not adjusted for: ethnicity, age at first
# use (length of opioid use), TOP use frequency, injecting at baseline,
# supervised consumption.

BASELINE_COVARIATES <- c(
  "age",
  "sex",
  "triage_year",
  "housing",
  "crack",
  "benzo",
  "prev_journeys",
  "time_out",
  "prior_treatment"
)

NO_YES <- c("No", "Yes")

# Treatment history -----------------------------------------------------------
#
# All of a client's earlier journeys are in the dataset (back to 1981), so
# prior time in treatment is the sum of their lengths of stay. Journeys that
# overlap are counted in full, so this can exceed calendar time.

#' Runs in dtplyr: the cumulative sum by client takes 19 s in plain dplyr
#' (220,000 clients) and 2 s this way
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

#' Adds the model covariates from the journey columns
#' @param journeys tibble. From prepare_cohort(), with age_at_time_zero, sex,
#'   triaged_jy, housing_baseline, drug_a.crack, drug_a.benzo,
#'   n_prev_journeys, previous_time_out_of_tx_triage and prior_treatment_days
#' @return journeys with the BASELINE_COVARIATES columns added (factors,
#'   except age); housing is NA where it was not recorded
add_covariates <- function(journeys) {
  journeys |>
    dplyr::mutate(
      age = age_at_time_zero,
      sex = factor(sex, c("M", "F")),
      triage_year = dplyr::case_when(
        lubridate::year(triaged_jy) >= 2025 ~ "2025+",
        .default = as.character(lubridate::year(triaged_jy))
      ) |>
        factor(c("2022", "2023", "2024", "2025+")),
      housing = factor(
        housing_baseline,
        c("Rented home", "Home owner", "Other home", "NFA", "Rough sleeping")
      ),
      crack = factor(dplyr::if_else(drug_a.crack == 1, "Yes", "No"), NO_YES),
      benzo = factor(dplyr::if_else(drug_a.benzo == 1, "Yes", "No"), NO_YES),
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
        factor(c("Under 1 year", "1-3 years", "3-6 years", "6 years or more"))
    ) |>
    dplyr::select(-time_out_days)
}
