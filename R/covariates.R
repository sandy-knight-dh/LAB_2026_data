# Covariates ------------------------------------------------------------------

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
    dplyr::transmute(
      client_random_id,
      n_jy,
      drug_a.benzo,
      n_prev_journeys,
      time_out_weeks = as.numeric(
        previous_time_out_of_tx_triage,
        units = "weeks"
      ),
      prior_treatment_days,
      housing_baseline
    )
}

# Treatment history -----------------------------------------------------------

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
      # Cut points in days (183, 365, 730), as weeks
      time_out = dplyr::case_when(
        n_prev_journeys == 0 | time_out_weeks < 183 / 7 ~ "Under 6 months",
        time_out_weeks < 365 / 7 ~ "6-12 months",
        time_out_weeks < 730 / 7 ~ "1-2 years",
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
    dplyr::select(-days_in_treatment)
}
