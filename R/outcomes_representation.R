# Re-presentation dataset -----------------------------------------------------
#
# The journeys that ended in successful completion, for the matched-cohort
# deck's re-presentation slides: the matches on this dataset pick which
# completions are compared. The re-presentation flags themselves (repres_6m,
# repres_12m and whether the follow-up is in the data) come from the summary
# journey table once the matched journeys are joined to it
# (R/matched_summary.R).
#
# - Cohort: prepare_cohort(), then journeys closed by DATA_END whose discharge
#   reason is successful completion (disrsn_jy.grp == 1).
# - Exposure is the discharged journey's group. Switchers count as LAB, with
#   time zero at the first prescribing start.

#' @param journeys tibble. From model_journeys()
#' @return tibble. One row per completed journey: model_data_cols(), with
#'   exposure_group a factor (reference "methadone"). The exclusion log is
#'   attached as attr(, "exclusions")
prepare_representation_data <- function(journeys) {
  cohort <- prepare_cohort(journeys)
  log <- cohort$log

  journeys <- cohort$journeys |>
    dplyr::filter(!is.na(disd_jy), disd_jy <= DATA_END)
  log <- log_step(journeys, log, "Journey closed by 31 March 2026")

  journeys <- dplyr::filter(journeys, disrsn_jy.grp == 1)
  log <- log_step(journeys, log, "Successful completion")

  journeys <- dplyr::filter(journeys, time_zero <= disd_jy)
  log <- log_step(journeys, log, "Time zero on or before discharge")

  journeys <- dplyr::filter(journeys, !is.na(region), !is.na(utla_final))
  log <- log_step(journeys, log, "Geography recorded")

  representation <- journeys |>
    dplyr::select(dplyr::all_of(model_data_cols())) |>
    dplyr::mutate(exposure_group = exposure_factor(exposure_group))

  attr(representation, "exclusions") <- log
  representation
}
