# Pipeline --------------------------------------------------------------------

targets::tar_source()
targets::tar_option_set(format = "qs")

# Data ------------------------------------------------------------------------

data_targets <- list(
  # Create variable data_file from DATA_PATH (Set in ./R/config.R to: ./data/final_linked_dataset_v5.feather)
  targets::tar_target(data_file, DATA_PATH, format = "file"),
  # Read the data file
  targets::tar_target(df, arrow::read_feather(data_file), format = "feather"),
  # The covariates the matching needs that the summary journey table doesn't
  # have (./R/covariates.R)
  targets::tar_target(covariates, journey_covariates(df))
)

# Summary data
#
# Unadjusted NDTMS-style counts and rates by OST type, each indicator from
# the summary journey table (summary_journeys()).

summary_targets <- list(
  targets::tar_target(summary_journey_table, summary_journeys(df)),
  targets::tar_target(
    summary_new_presentations,
    new_presentation_counts(summary_journey_table)
  ),
  targets::tar_target(
    summary_in_treatment,
    in_treatment_counts(summary_journey_table)
  ),
  targets::tar_target(
    summary_early_unplanned_exits,
    early_unplanned_exits(summary_journey_table)
  ),
  targets::tar_target(
    summary_early_unplanned_exits_by_substance,
    early_unplanned_exits_by_substance(summary_journey_table)
  ),
  targets::tar_target(
    summary_early_unplanned_exit_reasons,
    early_unplanned_exit_reasons(summary_journey_table)
  ),
  targets::tar_target(
    summary_successful_completions,
    successful_completions(summary_journey_table)
  ),
  targets::tar_target(
    summary_deaths,
    deaths_in_treatment(summary_journey_table)
  ),
  targets::tar_target(
    summary_re_presentations_6m,
    re_presentations(summary_journey_table, 6)
  ),
  targets::tar_target(
    summary_re_presentations_12m,
    re_presentations(summary_journey_table, 12)
  ),
  # Characteristics tables cover the whole period (WHOLE_PERIOD)
  targets::tar_target(
    summary_characteristics_in_treatment,
    summary_journey_table |>
      in_treatment(WHOLE_PERIOD$period_start, WHOLE_PERIOD$period_end) |>
      summary_characteristics(df)
  ),
  targets::tar_target(
    summary_characteristics_new_presentations,
    summary_journey_table |>
      new_presentations(WHOLE_PERIOD$period_start, WHOLE_PERIOD$period_end) |>
      summary_characteristics(df)
  ),
  targets::tar_target(
    summary_characteristics_completion,
    completion_characteristics(summary_journey_table, df)
  ),
  targets::tar_target(
    summary_characteristics_re_presentation_6m,
    re_presentation_characteristics(summary_journey_table, df, months = 6)
  ),
  # The last two split by OST type too, for the workbook only
  targets::tar_target(
    summary_characteristics_completion_by_ost,
    completion_characteristics(summary_journey_table, df, by_ost_type = TRUE)
  ),
  targets::tar_target(
    summary_characteristics_re_presentation_6m_by_ost,
    re_presentation_characteristics(
      summary_journey_table,
      df,
      months = 6,
      by_ost_type = TRUE
    )
  ),
  targets::tar_target(
    summary_results,
    list(
      new_presentations = summary_new_presentations,
      in_treatment = summary_in_treatment,
      early_unplanned_exits = summary_early_unplanned_exits,
      early_unplanned_exits_by_substance =
        summary_early_unplanned_exits_by_substance,
      early_unplanned_exit_reasons = summary_early_unplanned_exit_reasons,
      successful_completions = summary_successful_completions,
      deaths = summary_deaths,
      re_presentations_6m = summary_re_presentations_6m,
      re_presentations_12m = summary_re_presentations_12m,
      characteristics_in_treatment = summary_characteristics_in_treatment,
      characteristics_new_presentations =
        summary_characteristics_new_presentations,
      characteristics_completion = summary_characteristics_completion,
      characteristics_re_presentation_6m =
        summary_characteristics_re_presentation_6m,
      characteristics_completion_by_ost =
        summary_characteristics_completion_by_ost,
      characteristics_re_presentation_6m_by_ost =
        summary_characteristics_re_presentation_6m_by_ost
    )
  ),
  targets::tar_target(
    summary_deck,
    write_summary_deck(summary_results),
    format = "file"
  )
)


# Matched-cohort summary deck -------------------------------------------------
#
# The summary deck's populations with their LAB clients matched to methadone
# and oral buprenorphine clients, in each financial year (R/matched_summary.R,
# R/matching.R), and the summary deck's indicators counted on them.

matched_summary_targets <- list(
  targets::tar_target(
    matched_in_treatment,
    matched_by_year(summary_journey_table, covariates, in_treatment)
  ),
  targets::tar_target(
    matched_early_exit_cohort,
    matched_by_year(summary_journey_table, covariates, early_exit_cohort)
  ),
  targets::tar_target(
    matched_completions_6m,
    matched_by_year(
      summary_journey_table,
      covariates,
      completions_with_followup,
      months = 6
    )
  ),
  targets::tar_target(
    matched_completions_12m,
    matched_by_year(
      summary_journey_table,
      covariates,
      completions_with_followup,
      months = 12
    )
  ),
  # Over the whole period, for the characteristics table
  targets::tar_target(
    matched_whole_period,
    summary_journey_table |>
      in_treatment(WHOLE_PERIOD$period_start, WHOLE_PERIOD$period_end) |>
      match_population(covariates)
  ),
  targets::tar_target(
    matched_summary_results,
    list(
      in_treatment = matched_in_treatment_counts(matched_in_treatment),
      early_unplanned_exits =
        matched_early_unplanned_exits(matched_early_exit_cohort),
      successful_completions =
        matched_successful_completions(matched_in_treatment),
      deaths = matched_deaths(matched_in_treatment),
      re_presentations_6m = matched_re_presentations(matched_completions_6m),
      re_presentations_12m = matched_re_presentations(matched_completions_12m),
      characteristics = matched_characteristics(matched_whole_period, df),
      matches = match_summaries(
        in_treatment = matched_in_treatment,
        early_exit_cohort = matched_early_exit_cohort,
        completions_6m = matched_completions_6m,
        completions_12m = matched_completions_12m,
        whole_period = matched_whole_period
      )
    )
  ),
  targets::tar_target(
    matched_summary_deck,
    write_matched_summary_deck(matched_summary_results),
    format = "file"
  )
)


# All targets -----------------------------------------------------------------

list(
  data_targets,
  summary_targets,
  matched_summary_targets
)
