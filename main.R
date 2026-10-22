# Source scripts

for (file in list.files("R", full.names = TRUE)) {
  source(file)
}

# Data ------------------------------------------------------------------------

df <- arrow::read_feather(DATA_PATH)
covariates <- journey_covariates(df)

# Summary data ----------------------------------------------------------------
#
# Unadjusted NDTMS-style counts and rates by OST type, each indicator from
# the summary journey table (summary_journeys()). The characteristics tables
# cover the whole period (WHOLE_PERIOD); the last two are split by OST type
# too.

summary_journey_table <- summary_journeys(df)

summary_results <- list(
  new_presentations = new_presentation_counts(summary_journey_table),
  in_treatment = in_treatment_counts(summary_journey_table),
  early_unplanned_exits = early_unplanned_exits(summary_journey_table),
  early_unplanned_exits_by_substance =
    early_unplanned_exits_by_substance(summary_journey_table),
  early_unplanned_exit_reasons =
    early_unplanned_exit_reasons(summary_journey_table),
  successful_completions = successful_completions(summary_journey_table),
  deaths = deaths_in_treatment(summary_journey_table),
  re_presentations_6m = re_presentations(summary_journey_table, 6),
  re_presentations_12m = re_presentations(summary_journey_table, 12),
  characteristics_in_treatment = summary_journey_table |>
    in_treatment(WHOLE_PERIOD$period_start, WHOLE_PERIOD$period_end) |>
    summary_characteristics(df),
  characteristics_new_presentations = summary_journey_table |>
    new_presentations(WHOLE_PERIOD$period_start, WHOLE_PERIOD$period_end) |>
    summary_characteristics(df),
  characteristics_completion =
    completion_characteristics(summary_journey_table, df),
  characteristics_re_presentation_6m =
    re_presentation_characteristics(summary_journey_table, df, months = 6),
  characteristics_completion_by_ost =
    completion_characteristics(summary_journey_table, df, by_ost_type = TRUE),
  characteristics_re_presentation_6m_by_ost =
    re_presentation_characteristics(
      summary_journey_table,
      df,
      months = 6,
      by_ost_type = TRUE
    )
)


# Matched-cohort summary data -------------------------------------------------
#
# The summary data's populations with their LAB clients matched to methadone
# and oral buprenorphine clients, in each financial year (R/matched_summary.R,
# R/matching.R), and the summary data's indicators counted on them. The
# characteristics table is matched over the whole period.

matched_in_treatment <-
  matched_by_year(summary_journey_table, covariates, in_treatment)
matched_early_exit_cohort <-
  matched_by_year(summary_journey_table, covariates, early_exit_cohort)
matched_completions_6m <- matched_by_year(
  summary_journey_table,
  covariates,
  completions_with_followup,
  months = 6
)
matched_completions_12m <- matched_by_year(
  summary_journey_table,
  covariates,
  completions_with_followup,
  months = 12
)
matched_whole_period <- summary_journey_table |>
  in_treatment(WHOLE_PERIOD$period_start, WHOLE_PERIOD$period_end) |>
  match_population(covariates)

matched_summary_results <- list(
  in_treatment = matched_in_treatment_counts(matched_in_treatment),
  early_unplanned_exits =
    matched_early_unplanned_exits(matched_early_exit_cohort),
  successful_completions =
    matched_successful_completions(matched_in_treatment),
  deaths = matched_deaths(matched_in_treatment),
  re_presentations_6m = matched_re_presentations(matched_completions_6m),
  re_presentations_12m = matched_re_presentations(matched_completions_12m),
  characteristics = matched_characteristics(matched_whole_period, df),
  match_summaries = match_summaries(
    in_treatment = matched_in_treatment,
    early_exit_cohort = matched_early_exit_cohort,
    completions_6m = matched_completions_6m,
    completions_12m = matched_completions_12m,
    whole_period = matched_whole_period
  )
)

# Outputs ---------------------------------------------------------------------
#
# Tables as CSV and row-level datasets as feather, in output/ (R/outputs.R)

summary_tables <- write_tables(summary_results, prefix = "summary")
matched_tables <- write_tables(matched_summary_results, prefix = "matched")
output_datasets <- write_datasets(list(
  summary_journey_table = summary_journey_table,
  journey_covariates = covariates,
  matched_in_treatment = matched_in_treatment$people,
  matched_early_exit_cohort = matched_early_exit_cohort$people,
  matched_completions_6m = matched_completions_6m$people,
  matched_completions_12m = matched_completions_12m$people,
  matched_whole_period = matched_whole_period$people
))

cat("Written:", summary_tables, matched_tables, output_datasets, sep = "\n")
