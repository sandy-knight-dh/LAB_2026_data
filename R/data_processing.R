# Model datasets --------------------------------------------------------------
#
# Builds the journey-level datasets the matches are made on. Each step that
# drops journeys is recorded so exclusions can be reported, as the plan
# requires.
#
# The functions that summarise the raw dataset (prescribing_start_dates(),
# baseline_housing()) or group by client (treatment_history()) run in
# data.table through dtplyr; see R/data_loading_and_preprocessing.R. Inside a
# dtplyr pipeline a function's local variable must be injected with !!, since
# the verbs are translated to data.table code rather than evaluated in place.
# Everything on the journey table is plain dplyr.

# Journey-level columns for the model datasets, on top of JOURNEY_COLS
MODEL_COLS <- c(
  "drug.grp_sml",
  "disrsn_jy",
  "disrsn_jy.grp",
  "sex",
  "region",
  "utla_final",
  "methadone_record_count",
  "oral_buprenorphine_record_count",
  "los.days",
  # Covariates (R/covariates.R)
  "drug_a.crack",
  "drug_a.benzo",
  "n_prev_journeys",
  "previous_time_out_of_tx_triage"
)

# Journey table ---------------------------------------------------------------
#
# One row per journey, every journey in the dataset (all years, all drug
# groups), with what the model datasets need: the MODEL_COLS, prescribing
# start dates (prescribing_start_dates()), housing at the first assessment
# (baseline_housing()) and treatment history (treatment_history()).

#' @param df data.frame. Linked dataset
#' @return tibble. One row per journey: JOURNEY_COLS, MODEL_COLS, rx_start,
#'   lab_rx_start (Date), housing_baseline (character), prior_treatment_days
#'   (numeric) and treatment_naive (logical)
model_journeys <- function(df) {
  journeys <- collapse_to_journeys(df, group_cols = MODEL_COLS)

  journeys |>
    dplyr::mutate(
      methadone_record_count = dplyr::coalesce(methadone_record_count, 0),
      oral_buprenorphine_record_count = dplyr::coalesce(
        oral_buprenorphine_record_count,
        0
      )
    ) |>
    dplyr::left_join(
      prescribing_start_dates(df),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::left_join(
      baseline_housing(df),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::left_join(
      treatment_history(journeys),
      by = c("client_random_id", "n_jy")
    )
}

# Exposure groups -------------------------------------------------------------
#
# - lab: any LAB recorded on a SIR in the journey, whatever other OST is also
#   recorded.
# - methadone / oral_buprenorphine: only that oral OST recorded, no LAB; or
#   both oral drugs recorded and more SIRs recording this one.
# - mixed_oral: both methadone and oral buprenorphine on equally many SIRs,
#   no LAB.
# - no_ost: no OST recorded on any SIR (including journeys with no SIRs).
#
# The plan assigns journeys with both oral drugs by the majority of SIR time.
# Oral OST has no SIR dates in the extract, so the majority is by number of
# SIRs recording each drug (methadone_record_count,
# oral_buprenorphine_record_count), which ignores how long each SIR covers.
#
# lab_and_oral flags LAB journeys that also have oral OST recorded (mostly
# switches into LAB).

#' @param journeys tibble of journeys with ever_lab, ever_methadone and
#'   ever_oral_buprenorphine (integer 0/1), methadone_record_count and
#'   oral_buprenorphine_record_count (numeric, 0 if none)
#' @return journeys with exposure_group (character) and lab_and_oral (logical)
#'   added
add_exposure_group <- function(journeys) {
  journeys |>
    dplyr::mutate(
      both_oral = ever_methadone == 1 & ever_oral_buprenorphine == 1,
      exposure_group = dplyr::case_when(
        ever_lab == 1 ~ "lab",
        both_oral & methadone_record_count > oral_buprenorphine_record_count ~
          "methadone",
        both_oral & oral_buprenorphine_record_count > methadone_record_count ~
          "oral_buprenorphine",
        both_oral ~ "mixed_oral",
        ever_methadone == 1 ~ "methadone",
        ever_oral_buprenorphine == 1 ~ "oral_buprenorphine",
        .default = "no_ost"
      ),
      lab_and_oral = ever_lab == 1 &
        (ever_methadone == 1 | ever_oral_buprenorphine == 1)
    ) |>
    dplyr::select(-both_oral)
}

# Time zero -------------------------------------------------------------------
#
# Time zero is `modst`, the start date of the prescribing intervention, not
# the triage date. The submodality first recorded on a SIR is assumed to have
# started on that date.
#
# - Prescribing interventions are modal == 94.
# - Oral OST, and LAB journeys with oral OST also recorded (switchers): the
#   earliest prescribing modst in the journey.
# - LAB only: the modst of the prescribing intervention in which LAB is first
#   recorded, i.e. the latest prescribing modst on or before first_lab_date.
#   This is the first prescribing intervention in 97.7% of cases.

PRESCRIBING_MODAL <- 94

#' Runs in dtplyr: it summarises the raw dataset by journey
#' @param df data.frame. Linked dataset
#' @return tibble. One row per journey with a prescribing intervention:
#'   client_random_id, n_jy, rx_start (Date), lab_rx_start (Date, NA unless
#'   LAB is recorded within a prescribing intervention)
prescribing_start_dates <- function(df) {
  rx <- dtplyr::lazy_dt(df) |>
    dplyr::filter(modal == PRESCRIBING_MODAL, !is.na(modst)) |>
    dplyr::distinct(client_random_id, n_jy, modst, first_lab_date) |>
    dplyr::compute()

  first_rx <- rx |>
    dplyr::summarise(rx_start = min(modst), .by = c(client_random_id, n_jy))

  lab_rx <- rx |>
    dplyr::filter(modst <= first_lab_date) |>
    dplyr::summarise(
      lab_rx_start = max(modst),
      .by = c(client_random_id, n_jy)
    )

  first_rx |>
    dplyr::left_join(lab_rx, by = c("client_random_id", "n_jy")) |>
    tibble::as_tibble()
}

# Exclusion log ---------------------------------------------------------------
#
# Each dataset's exclusions are logged one step at a time: filter, then
# log_step(). The log is attached to the dataset as attr(, "exclusions").

#' @param data tibble. Journeys after the step
#' @param log tibble. Exclusion log so far, or NULL to start one
#' @param step character(1). Description of the step
#' @return tibble. step (character), n_journeys (integer), n_clients (integer)
log_step <- function(data, log, step) {
  counts <- data |>
    dplyr::summarise(
      n_journeys = dplyr::n(),
      n_clients = dplyr::n_distinct(client_random_id)
    ) |>
    dplyr::mutate(step = step, .before = 1)

  dplyr::bind_rows(log, counts)
}

# Cohort ----------------------------------------------------------------------
#
# Steps shared by both model datasets, from all journeys to the analysis
# cohort with time zero, age at time zero and the baseline covariates
# (add_covariates()). Dataset-specific steps follow in
# prepare_completion_tte_data() and prepare_representation_data().
#
# - Journeys triaged from January 2022 (COHORT_START) and aged 18+ (MIN_AGE)
#   at time zero.
# - Opiate journeys only (drug.grp_sml == 1), as the plan specifies. The
#   dataset also holds these clients' other journeys.
# - Exposure groups: add_exposure_group(). Only LAB, methadone and oral
#   buprenorphine journeys are kept; ties between the oral drugs (mixed_oral)
#   and no OST are dropped.
# - LAB journeys with oral OST also recorded (switchers) are kept, with time
#   zero at the first prescribing start; prepare_completion_tte_data() splits
#   their follow-up at first_lab_date.
# - Missing covariates: only housing at assessment has any. Journeys without
#   it are dropped (complete case), so every dataset uses the same journeys.
#   Housing was not collected before April 2022, so in practice the cohort
#   starts then.

COHORT_START <- as.Date("2022-01-01")
MIN_AGE <- 18

#' @param journeys tibble. From model_journeys()
#' @return list. journeys (tibble, one row per journey, with time_zero,
#'   age_at_time_zero and the covariates) and log (tibble, exclusion log)
prepare_cohort <- function(journeys) {
  journeys <- journeys |>
    add_exposure_group() |>
    dplyr::mutate(
      time_zero = dplyr::if_else(
        exposure_group == "lab" & !lab_and_oral,
        lab_rx_start,
        rx_start
      )
    )
  log <- log_step(journeys, NULL, "All journeys in dataset")

  journeys <- dplyr::filter(journeys, triaged_jy >= COHORT_START)
  log <- log_step(journeys, log, "Triaged on or after 1 January 2022")

  journeys <- dplyr::filter(journeys, drug.grp_sml == 1)
  log <- log_step(journeys, log, "Opiate journey")

  journeys <- dplyr::filter(
    journeys,
    exposure_group %in% c("lab", "methadone", "oral_buprenorphine")
  )
  log <- log_step(
    journeys,
    log,
    "LAB or oral OST (both oral drugs: majority of SIRs, ties excluded)"
  )

  journeys <- dplyr::filter(journeys, !is.na(time_zero))
  log <- log_step(journeys, log, "Time zero identified")

  journeys <- journeys |>
    dplyr::mutate(
      age_at_time_zero = age_jy_start +
        floor(as.numeric(time_zero - triaged_jy, units = "days") / 365.25)
    ) |>
    dplyr::filter(age_at_time_zero >= MIN_AGE)
  log <- log_step(journeys, log, "Aged 18+ at time zero")

  journeys <- journeys |>
    add_covariates() |>
    dplyr::filter(!is.na(housing))
  log <- log_step(
    journeys,
    log,
    "Housing at assessment recorded (not collected before April 2022)"
  )

  list(journeys = journeys, log = log)
}

#' The exposure group factor used by both model datasets, methadone first so
#' it is the reference
#' @param x character. Exposure groups
#' @return factor
exposure_factor <- function(x) {
  factor(x, levels = c("methadone", "oral_buprenorphine", "lab"))
}

#' Columns both model datasets return: identifiers, exposure, time zero,
#' discharge, geography and the covariates. A function rather than a
#' constant, as the covariates are defined in R/covariates.R
#' @return character
model_data_cols <- function() {
  c(
    "client_random_id",
    "n_jy",
    "exposure_group",
    "lab_and_oral",
    "time_zero",
    "triaged_jy",
    "disd_jy",
    "disrsn_jy",
    "disrsn_jy.grp",
    "region",
    "utla_final",
    BASELINE_COVARIATES
  )
}

# Time-to-event dataset -------------------------------------------------------
#
# Follow-up from time zero to the end of the journey, for successful
# completion. People still in treatment are kept and censored at DATA_END,
# and LAB switchers count as their oral OST before switching and as LAB after
# it. The matched-cohort deck matches on this dataset (one row per journey,
# one_row_per_journey()).
#
# - Rows are follow-up periods: tstart and tstop in days from time zero, one
#   exposure group each. Most journeys have one row. A switcher has an oral
#   OST row up to first_lab_date and a LAB row after it. The oral OST before
#   the switch follows the same rule as add_exposure_group(): a switcher with
#   both oral drugs counts as the one recorded on more SIRs. Where there is no
#   single oral drug (ties) the oral row is dropped, so their LAB row starts
#   after day 0 (delayed entry).
# - The event is successful completion: disrsn_jy.grp == 1 (codes 80
#   Treatment Completed - Drug Free, 82 Treatment Completed - Occasional
#   User), recorded on the journey's last row. Any other discharge ends
#   follow-up without the event (censored), so transfers (including into
#   custody) and deaths count as not completed. Journeys still open at
#   DATA_END, or discharged after it, are censored at DATA_END.
# - Half a day is added to every journey's follow-up so that same-day
#   discharges have positive time at risk.
# - first_lab_date comes from a retrospective SIR, so some early LAB time is
#   counted as oral OST.

#' @param journeys tibble. From model_journeys()
#' @return tibble. One row per follow-up period: model_data_cols()
#'   (exposure_group a factor, reference "methadone", for the period), tstart,
#'   tstop (numeric, days from time zero), successful_completion (integer
#'   0/1), closed (logical, journey closed by DATA_END), pyrs (numeric,
#'   person-years) and switcher (logical). The exclusion log (journeys) is
#'   attached as attr(, "exclusions")
prepare_completion_tte_data <- function(journeys) {
  cohort <- prepare_cohort(journeys)
  log <- cohort$log

  journeys <- cohort$journeys |>
    dplyr::mutate(
      closed = !is.na(disd_jy) & disd_jy <= DATA_END,
      end_date = dplyr::if_else(closed, disd_jy, DATA_END),
      event = dplyr::if_else(closed, as.integer(disrsn_jy.grp == 1), 0L)
    ) |>
    dplyr::filter(time_zero <= end_date)
  log <- log_step(journeys, log, "Time zero on or before end of follow-up")

  journeys <- journeys |>
    dplyr::filter(!is.na(region), !is.na(utla_final)) |>
    dplyr::mutate(
      switcher = exposure_group == "lab" & lab_and_oral,
      follow_up = as.numeric(end_date - time_zero, units = "days") + 0.5,
      switch_day = pmin(
        pmax(as.numeric(first_lab_date - time_zero, units = "days"), 0),
        follow_up
      ),
      oral_group = dplyr::case_when(
        ever_methadone == 1 & ever_oral_buprenorphine == 0 ~ "methadone",
        ever_oral_buprenorphine == 1 & ever_methadone == 0 ~
          "oral_buprenorphine",
        methadone_record_count > oral_buprenorphine_record_count ~
          "methadone",
        oral_buprenorphine_record_count > methadone_record_count ~
          "oral_buprenorphine"
      )
    )
  log <- log_step(journeys, log, "Geography recorded")

  single_exposure <- journeys |>
    dplyr::filter(!switcher) |>
    dplyr::mutate(tstart = 0, tstop = follow_up)

  before_switch <- journeys |>
    dplyr::filter(switcher, !is.na(oral_group), switch_day > 0) |>
    dplyr::mutate(
      exposure_group = oral_group,
      tstart = 0,
      tstop = switch_day,
      event = 0L
    )

  after_switch <- journeys |>
    dplyr::filter(switcher, follow_up > switch_day) |>
    dplyr::mutate(tstart = switch_day, tstop = follow_up)

  tte_data <- dplyr::bind_rows(single_exposure, before_switch, after_switch) |>
    dplyr::rename(successful_completion = event) |>
    dplyr::select(dplyr::all_of(c(
      model_data_cols(),
      "tstart", "tstop", "successful_completion", "closed", "switcher"
    ))) |>
    dplyr::mutate(
      exposure_group = exposure_factor(exposure_group),
      pyrs = (tstop - tstart) / 365.25,
      .after = successful_completion
    ) |>
    dplyr::arrange(client_random_id, n_jy, tstart)

  # A switcher with no single oral drug (no row before the switch) and first
  # LAB on the last day of follow-up (no row after it) has no rows
  log <- log_step(
    dplyr::distinct(tte_data, client_random_id, n_jy),
    log,
    "With follow-up time in an exposure group"
  )

  attr(tte_data, "exclusions") <- log
  tte_data
}

#' Each journey once: a switcher's LAB row, otherwise its only row. Used to
#' match and to describe the time-to-event dataset by journey
#' @param data tibble. From prepare_completion_tte_data()
#' @return tibble. One row per journey, the LAB row for switchers
one_row_per_journey <- function(data) {
  data |>
    # arrange() and distinct() are much faster than summarise(.by =) on
    # 100,000 journeys
    dplyr::arrange(dplyr::desc(exposure_group == "lab")) |>
    dplyr::distinct(client_random_id, n_jy, .keep_all = TRUE)
}
