# Source scripts

for (file in list.files("R", full.names = TRUE)) {
  source(file)
}

# Data ------------------------------------------------------------------------

df <- arrow::read_feather(DATA_PATH)
journeys <- model_journeys(df)

# Propensity score matching ---------------------------------------------------


data_completion_tte <- prepare_completion_tte_data(journeys)
data_representation <- prepare_representation_data(journeys)

match_methadone_completion_tte <-
  match_with_fallback(data_completion_tte, "methadone", "completion_tte")
match_oral_buprenorphine_completion_tte <-
  match_with_fallback(data_completion_tte, "oral_buprenorphine", "completion_tte")
match_methadone_representation <-
  match_with_fallback(data_representation, "methadone", "representation")
match_oral_buprenorphine_representation <-
  match_with_fallback(data_representation, "oral_buprenorphine", "representation")

# Summary data deck -----------------------------------------------------------
#
# Unadjusted NDTMS-style counts and rates by OST type, each indicator from
# the summary journey table (summary_journeys()). The characteristics tables
# cover the whole period (WHOLE_PERIOD); the last two are split by OST type
# too, for the workbook only.

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

summary_deck <- write_summary_deck(summary_results)

# Matched-cohort summary deck -------------------------------------------------
#
# The summary deck's counts and rates on the propensity score matched
# journeys (R/matched_summary.R), from the matches joined to the summary
# journey table.

matched_tte_groups <- matched_groups(
  match_methadone_completion_tte,
  match_oral_buprenorphine_completion_tte
)
matched_representation_groups <- matched_groups(
  match_methadone_representation,
  match_oral_buprenorphine_representation
)
matched_tte_journeys <-
  join_matched_groups(summary_journey_table, matched_tte_groups)
matched_representation_journeys <-
  join_matched_groups(summary_journey_table, matched_representation_groups)

matched_summary_results <- list(
  in_treatment = matched_in_treatment_counts(matched_tte_journeys),
  early_unplanned_exits = matched_early_unplanned_exits(matched_tte_journeys),
  successful_completions =
    matched_successful_completions(matched_tte_journeys),
  deaths = matched_deaths(matched_tte_journeys),
  re_presentations_6m =
    matched_re_presentations(matched_representation_journeys, 6),
  re_presentations_12m =
    matched_re_presentations(matched_representation_journeys, 12),
  characteristics =
    matched_characteristics(data_completion_tte, matched_tte_groups, df)
)

matched_summary_deck <- write_matched_summary_deck(matched_summary_results)

cat("Written:", summary_deck, matched_summary_deck, sep = "\n")
