# Summary data ----------------------------------------------------------------
#
# Unadjusted counts and rates by OST type, by financial year, written to
# output/ as CSV files (write_tables(), R/outputs.R). Written for reading
# and checking rather than speed: plain dplyr, one small function per
# indicator. The exception is prescribing_spans(), which summarises the raw
# dataset in dtplyr. In the pipeline each indicator is a target of its own
# (summary_<name>), built from the summary journey table, and summary_results
# collects them for output.
#
# Definitions follow the NDTMS indicators:
# - Counts are of individuals: a client with more than one journey in a period
#   is counted once, using their latest journey in that period.
# - Opiate clients only: those whose latest journey in the period is an opiate
#   journey (drug.grp_sml == 1). The dataset also holds these clients' other
#   journeys. This matches the in-treatment denominator of the NDTMS opiate
#   dashboard (about 134,500 in the 12 months to March 2026).
# - In treatment: adults with a journey open at any time in the period (see
#   journeys_in_period()).
# - New presentations: adults whose journey started in the period.
# - OST type is what the client received in the period, not over the whole
#   journey: see add_ost_type().
# - Substance group: opiates and crack if crack was recorded at the journey's
#   assessment (drug_a.crack), otherwise opiates only. The dataset holds
#   opiate clients only, so this is the nearest to the NDTMS substance groups.

# Financial years: rolling 12 months to the end of March
FINANCIAL_YEARS <- tibble::tibble(
  year = c("2022/23", "2023/24", "2024/25", "2025/26"),
  period_start = as.Date(c(
    "2022-04-01", "2023-04-01", "2024-04-01", "2025-04-01"
  )),
  period_end = as.Date(c(
    "2023-03-31", "2024-03-31", "2025-03-31", "2026-03-31"
  ))
)

# The whole period, for the characteristics tables
WHOLE_PERIOD <- tibble::tibble(
  period_start = as.Date("2022-04-01"),
  period_end = as.Date("2026-03-31")
)

# Early unplanned exits are counted on new presentations this much earlier
# than the period, so that everyone has had 12 weeks to be retained
EARLY_EXIT_DELAY <- lubridate::period(3, "months")

# Journeys --------------------------------------------------------------------

# Journey-level columns used by the indicators, on top of JOURNEY_COLS
SUMMARY_COLS <- c(
  "drug.grp_sml",
  "drug_a.crack",
  "sex",
  "region",
  "utla_final",
  "disrsn_jy",
  "disrsn_jy.grp",
  "effective_tx",
  "repres_6m",
  "repres_12m",
  "followup_available_6m",
  "followup_available_12m"
)

# Prescribing interventions are modal 94
PRESCRIBING_MODAL <- 94

#' First to last prescribing intervention in each journey. Gaps between
#' prescribing interventions are ignored, as they are between LAB records.
#' Runs in dtplyr: summarising 600,000 rows by journey takes about 19 s in
#' plain dplyr and 2 s this way
#' @param df data.frame. Linked dataset
#' @return tibble. One row per journey with a prescribing intervention:
#'   client_random_id, n_jy, rx_start (Date, earliest start), rx_end (Date,
#'   latest end; the journey's discharge if the intervention has no end date,
#'   DATA_END if the journey is still open)
prescribing_spans <- function(df) {
  dtplyr::lazy_dt(df) |>
    dplyr::filter(modal == PRESCRIBING_MODAL, !is.na(modst)) |>
    dplyr::mutate(rx_end = dplyr::coalesce(modend, disd_jy, DATA_END)) |>
    dplyr::summarise(
      rx_start = min(modst),
      rx_end = max(rx_end),
      .by = c(client_random_id, n_jy)
    ) |>
    tibble::as_tibble()
}

#' @param df data.frame. Linked dataset
#' @return tibble. One row per journey, with the prescribing span (rx_start,
#'   rx_end; NA if the journey has no prescribing intervention), the
#'   outcome flags used by the indicators (logical) and substance_group
#'   (character, one of SUBSTANCE_GROUPS)
summary_journeys <- function(df) {
  df |>
    collapse_to_journeys(group_cols = SUMMARY_COLS) |>
    dplyr::left_join(
      prescribing_spans(df),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::mutate(
      # From the NDTMS effective treatment module: 0 is an exit within 12
      # weeks of the journey start that wasn't a successful completion
      early_unplanned_exit = effective_tx == 0,
      # Codes 80 Treatment Completed - Drug Free and 82 Treatment Completed -
      # Occasional User; NA while the journey is open
      successful_completion = disrsn_jy.grp == 1,
      # Incomplete - client died. Deaths after discharge are outside the
      # treatment record
      died = disrsn_jy == 89,
      substance_group = dplyr::if_else(
        drug_a.crack == 1,
        "opiates_and_crack",
        "opiates_only"
      )
    )
}

# Crack recorded at assessment is complete (0 or 1 on every journey), so the
# two groups partition every population
SUBSTANCE_GROUPS <- c("opiates_only", "opiates_and_crack")

# OST type --------------------------------------------------------------------
#
# What the client received in the period, so that LAB first recorded in 2025,
# for example, doesn't count in 2022/23:
# - lab: LAB in the period on any of the client's journeys in the population,
#   meaning the span from the journey's first to last LAB record (SIRs)
#   overlaps the period. The span doesn't imply continuous LAB, and LAB at
#   the end of a period first recorded on a later SIR is missed.
# - methadone / oral_buprenorphine: otherwise, a prescribing intervention open
#   in the period (prescribing_spans()), on a journey with only that oral OST
#   recorded. Oral OST has no dates in the dataset, so the drug comes from the
#   whole journey and the prescribing intervention dates place it in the period.
# - not_recorded: a prescribing intervention in the period but no OST recorded
#   for it. OST is recorded at reviews (SIRs), often months after a journey
#   starts, so this is mostly recent journeys, and 2025/26 has many.
# - mixed_oral (both oral drugs on the journey) and no_prescribing count in All
#   only, so the OST types don't sum to All.

OST_TYPES <- c("methadone", "oral_buprenorphine", "lab")

# Labels for the OST types, used in the column names of the characteristics
# tables split by OST type (ost_outcome_group())
OST_LABELS <- c(
  all = "All",
  methadone = "Methadone",
  oral_buprenorphine = "Oral buprenorphine",
  lab = "LAB",
  not_recorded = "Prescribing, drug not recorded"
)

#' @param journeys tibble. Journeys from summary_journeys(), limited to the
#'   population (e.g. those in treatment in the period)
#' @param period_start,period_end Date(1)
#' @return tibble. journeys with ost_type (character) added: lab, methadone,
#'   oral_buprenorphine, not_recorded, mixed_oral or no_prescribing. Every
#'   journey of a client with LAB in the period is lab
add_ost_type <- function(journeys, period_start, period_end) {
  lab_clients <- journeys |>
    dplyr::filter(
      ever_lab == 1,
      first_lab_date <= period_end,
      last_lab_date >= period_start
    ) |>
    dplyr::pull(client_random_id)

  journeys |>
    dplyr::mutate(
      prescribing_in_period = !is.na(rx_start) &
        rx_start <= period_end &
        rx_end >= period_start,
      ost_type = dplyr::case_when(
        client_random_id %in% lab_clients ~ "lab",
        !prescribing_in_period ~ "no_prescribing",
        ever_methadone == 1 & ever_oral_buprenorphine == 1 ~ "mixed_oral",
        ever_methadone == 1 ~ "methadone",
        ever_oral_buprenorphine == 1 ~ "oral_buprenorphine",
        .default = "not_recorded"
      )
    ) |>
    dplyr::select(-prescribing_in_period)
}

# Populations -----------------------------------------------------------------

#' Each client once, by their latest journey, with their OST type in the
#' period. Clients whose latest journey isn't an opiate journey are left out
#' @param journeys tibble. Journeys from summary_journeys(), limited to the
#'   population
#' @param period_start,period_end Date(1)
#' @return tibble. One row per client, with ost_type
opiate_clients <- function(journeys, period_start, period_end) {
  journeys |>
    add_ost_type(period_start, period_end) |>
    # Latest journey per client. The same as slice_max(n_jy, by =
    # client_random_id), since n_jy is unique within a client, but that takes
    # about 11 s for a year's clients and this takes a fraction of a second
    dplyr::arrange(dplyr::desc(n_jy)) |>
    dplyr::distinct(client_random_id, .keep_all = TRUE) |>
    dplyr::filter(drug.grp_sml == 1)
}

#' Opiate clients in treatment at any time in the period, adults only
#' @param journeys tibble. From summary_journeys()
#' @param period_start,period_end Date(1)
#' @return tibble. One row per client, from opiate_clients(), with age (at the
#'   period start or journey start, whichever is later)
in_treatment <- function(journeys, period_start, period_end) {
  journeys |>
    journeys_in_period(period_start, period_end) |>
    opiate_clients(period_start, period_end) |>
    dplyr::mutate(age = age_at_ref)
}

#' Opiate clients whose journey started in the period, adults only
#' @param journeys tibble. From summary_journeys()
#' @param period_start,period_end Date(1)
#' @return tibble. One row per client, from opiate_clients(), with age (at
#'   journey start) and ref_date (the journey start, as journeys_in_period()
#'   gives it for in_treatment())
new_presentations <- function(journeys, period_start, period_end) {
  journeys |>
    dplyr::filter(
      dplyr::between(triaged_jy, period_start, period_end),
      age_jy_start >= 18
    ) |>
    opiate_clients(period_start, period_end) |>
    dplyr::mutate(age = age_jy_start, ref_date = triaged_jy)
}

# Counting --------------------------------------------------------------------
#
# Every count or rate is given for all clients and then for each OST type.
# with_all_ost_types() stacks a copy of the data labelled "all" on the rows of
# each type, so one count() or summarise() by ost_type gives both.

#' @param data tibble. One row per client, with ost_type (character)
#' @param types character vector. OST types to keep, besides all
#' @return tibble. The rows of data with ost_type "all", then the rows of each
#'   type; ost_type a factor (all, then types)
with_all_ost_types <- function(data, types = OST_TYPES) {
  dplyr::bind_rows(
    dplyr::mutate(data, ost_type = "all"),
    dplyr::filter(data, ost_type %in% types)
  ) |>
    dplyr::mutate(ost_type = factor(ost_type, c("all", types)))
}

#' @param data tibble. One row per client, with ost_type
#' @param types character vector. OST types to count, besides all
#' @return tibble. ost_type (factor: all, then types), n (integer)
count_by_ost_type <- function(data, types = OST_TYPES) {
  data |>
    with_all_ost_types(types) |>
    dplyr::count(ost_type)
}

#' @param data tibble. One row per client (the denominator), with ost_type
#' @param event logical column in data. The numerator; NA counts as FALSE
#' @return tibble. ost_type, numerator, denominator, rate (%)
rate_by_ost_type <- function(data, event) {
  data |>
    with_all_ost_types() |>
    dplyr::summarise(
      numerator = sum({{ event }}, na.rm = TRUE),
      denominator = dplyr::n(),
      .by = ost_type
    ) |>
    dplyr::mutate(rate = 100 * numerator / denominator) |>
    dplyr::arrange(ost_type)
}

#' Runs a calculation for each financial year and stacks the results
#' @param calculation function(period_start, period_end) returning a tibble
#' @return tibble. year (character), then the calculation's columns
for_each_year <- function(calculation) {
  purrr::pmap(FINANCIAL_YEARS, \(year, period_start, period_end) {
    calculation(period_start, period_end) |>
      dplyr::mutate(year = year, .before = 1)
  }) |>
    purrr::list_rbind()
}

# Indicators ------------------------------------------------------------------

# The counts also show prescribing with no OST recorded, so that a fall in the
# OST types from incomplete recording can be told apart from a real fall
COUNT_OST_TYPES <- c(OST_TYPES, "not_recorded")

#' @param journeys tibble. From summary_journeys()
#' @return tibble. year, ost_type, n
new_presentation_counts <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      new_presentations(period_start, period_end) |>
      count_by_ost_type(COUNT_OST_TYPES)
  })
}

#' @param journeys tibble. From summary_journeys()
#' @return tibble. year, ost_type, n
in_treatment_counts <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      in_treatment(period_start, period_end) |>
      count_by_ost_type(COUNT_OST_TYPES)
  })
}

#' Early unplanned exits as a share of new presentations. As in the published
#' indicator, the cohort is delayed by 3 months (EARLY_EXIT_DELAY) so that
#' everyone has had 12 weeks to be retained: 2025/26 is new presentations
#' January to December 2025
#' @param journeys tibble. From summary_journeys()
#' @param group character(1) or NULL. One of SUBSTANCE_GROUPS, to limit the
#'   cohort to it. Applied after each client's latest journey is chosen, so
#'   the groups add up to the whole cohort
#' @return tibble. year, ost_type, numerator, denominator, rate
early_unplanned_exits <- function(journeys, group = NULL) {
  for_each_year(\(period_start, period_end) {
    cohort <- early_exit_cohort(journeys, period_start, period_end)
    if (!is.null(group)) {
      cohort <- dplyr::filter(cohort, substance_group == group)
    }
    rate_by_ost_type(cohort, early_unplanned_exit)
  })
}

#' The denominator of early_unplanned_exits(): new presentations in the
#' period delayed by 3 months. Shared with early_unplanned_exit_reasons(), so
#' the two always count the same clients
#' @param journeys tibble. From summary_journeys()
#' @param period_start,period_end Date(1). The financial year, before the delay
#' @return tibble. One row per client, from new_presentations()
early_exit_cohort <- function(journeys, period_start, period_end) {
  new_presentations(
    journeys,
    period_start - EARLY_EXIT_DELAY,
    period_end - EARLY_EXIT_DELAY
  )
}

#' Early unplanned exits for each substance group in turn
#' @param journeys tibble. From summary_journeys()
#' @return tibble. substance_group, then the columns of early_unplanned_exits()
early_unplanned_exits_by_substance <- function(journeys) {
  purrr::map(SUBSTANCE_GROUPS, \(group) {
    early_unplanned_exits(journeys, group) |>
      dplyr::mutate(substance_group = group, .before = 1)
  }) |>
    purrr::list_rbind()
}

#' Successful completions in the period as a share of those in treatment. As
#' in NDTMS reporting, each client's status is their latest journey in the
#' period, so someone who completes and starts a new journey in the same
#' period is not counted as a completion
#' @param journeys tibble. From summary_journeys()
#' @return tibble. year, ost_type, numerator, denominator, rate
successful_completions <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      in_treatment(period_start, period_end) |>
      dplyr::mutate(
        completed_in_period = successful_completion &
          dplyr::between(disd_jy, period_start, period_end)
      ) |>
      rate_by_ost_type(completed_in_period)
  })
}

#' Deaths in treatment in the period as a share of those in treatment
#' @param journeys tibble. From summary_journeys()
#' @return tibble. year, ost_type, numerator, denominator, rate
deaths_in_treatment <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      in_treatment(period_start, period_end) |>
      dplyr::mutate(
        died_in_period = died &
          dplyr::between(disd_jy, period_start, period_end)
      ) |>
      rate_by_ost_type(died_in_period)
  })
}

#' Re-presentations within 6 or 12 months of a successful completion in the
#' period, as a share of those completions.
#' - The denominator starts from completed journeys, not each client's latest
#'   journey: someone who completes and comes back in the same year has the
#'   new journey as their latest, so would otherwise be missed. A client with
#'   more than one completion in the period counts once, for the latest.
#' - Completions too recent for the full follow-up (followup_available_6m or
#'   _12m is 0) are left out, so 2025/26 has completions to September 2025
#'   only for 6 months and none for 12 months (that year is dropped).
#' @param journeys tibble. From summary_journeys()
#' @param months 6 or 12
#' @return tibble. year, ost_type, numerator, denominator, rate
re_presentations <- function(journeys, months) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      completions_with_followup(period_start, period_end, months) |>
      rate_by_ost_type(re_presented)
  }) |>
    dplyr::filter(denominator > 0)
}

#' Opiate clients who completed successfully in the period, with the full
#' follow-up for re-presentation available. The denominator of
#' re_presentations(), which explains the choices
#' @param journeys tibble. From summary_journeys()
#' @param period_start,period_end Date(1)
#' @param months 6 or 12
#' @return tibble. One row per client, from opiate_clients(), with
#'   re_presented (logical) and age (at the period start or journey start,
#'   whichever is later)
completions_with_followup <- function(
  journeys,
  period_start,
  period_end,
  months
) {
  followup <- paste0("followup_available_", months, "m")
  repres <- paste0("repres_", months, "m")

  journeys |>
    journeys_in_period(period_start, period_end) |>
    dplyr::filter(
      successful_completion,
      dplyr::between(disd_jy, period_start, period_end),
      .data[[followup]] == 1
    ) |>
    opiate_clients(period_start, period_end) |>
    dplyr::mutate(re_presented = .data[[repres]] == 1, age = age_at_ref)
}

# Early unplanned exits by discharge reason -----------------------------------
#
# The numerator of early_unplanned_exits() split by the journey's discharge
# reason (disrsn_jy), so each year's reasons add up to that indicator's
# numerator for each OST type. The early_unplanned_exits() output is
# unchanged, so the tables without reasons don't depend on this.
#
# The NDTMS discharge codes are collapsed into four groups (the user's
# choice, 2026-10-19):
# - Dropped out: incomplete, dropped out (85).
# - Custody: transferred in custody (84) or incomplete, retained in custody
#   (87).
# - Died: incomplete, client died (89).
# - Other: everything else. Mainly transferred not in custody (83), plus
#   referred on and the other transfer codes, treatment commencement declined
#   (88), treatment withdrawn by provider (86), no reason recorded, and a few
#   journeys whose discharge reason is a completion although the effective
#   treatment flag counts them as an early unplanned exit.

EXIT_REASONS <- c("Dropped out", "Custody", "Died", "Other")

#' @param disrsn_jy numeric. Journey discharge reason code
#' @return factor with levels EXIT_REASONS
exit_reason_group <- function(disrsn_jy) {
  dplyr::case_when(
    disrsn_jy == 85 ~ "Dropped out",
    disrsn_jy %in% c(84, 87) ~ "Custody",
    disrsn_jy == 89 ~ "Died",
    .default = "Other"
  ) |>
    factor(levels = EXIT_REASONS)
}

#' Early unplanned exits by discharge reason, for each financial year
#' @param journeys tibble. From summary_journeys()
#' @return tibble. year, ost_type (factor: all, then OST_TYPES), reason
#'   (factor, EXIT_REASONS), n (integer, early unplanned exits), pct (numeric,
#'   share of the year's early unplanned exits for the OST type). Every
#'   combination is present, with n = 0 where there are none
early_unplanned_exit_reasons <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      early_exit_cohort(period_start, period_end) |>
      dplyr::filter(early_unplanned_exit) |>
      dplyr::mutate(reason = exit_reason_group(disrsn_jy)) |>
      with_all_ost_types() |>
      dplyr::count(ost_type, reason) |>
      tidyr::complete(ost_type, reason, fill = list(n = 0L)) |>
      dplyr::mutate(pct = 100 * n / sum(n), .by = ost_type) |>
      dplyr::arrange(ost_type, reason)
  })
}

# Characteristics -------------------------------------------------------------

#' Table 1 for clients in the whole period (April 2022 to March 2026), with a
#' column for each of OST_TYPES
#' @param people tibble. From in_treatment() or new_presentations(), with age
#' @param df data.frame. Linked dataset
#' @return tibble. From make_table1()
summary_characteristics <- function(people, df) {
  people |>
    dplyr::transmute(
      client_random_id,
      n_jy,
      exposure_group = factor(ost_type, OST_TYPES),
      age,
      sex,
      region
    ) |>
    add_table1_variables(df) |>
    make_table1()
}

# Characteristics by outcome --------------------------------------------------
#
# The same Table 1, with a column for each side of an outcome (completed or
# not, re-presented or not) instead of each OST type, plus time in treatment
# on the journey itself (los.days, the NDTMS length of stay: days from the
# journey's triage to its discharge, or to the end of the extract if it is
# still open). The bands are mutually exclusive, with the lower bound
# included: 1-2 years is one year or more and under two.
#
# Each table also comes split by OST type in the period as well as the
# outcome (by_ost_type = TRUE): a column for each of OST_TYPES and each side
# of the outcome, OST type first ("Methadone: Completed", "Methadone: Not
# completed", ...). Clients with another OST type (prescribing with no OST
# recorded, mixed oral OST, no prescribing) count in Overall only, as on the
# other summary tables, so Overall is the same as in the table without the
# split.

TIME_IN_TREATMENT_BREAKS <- c(0, 0.5, 1, 2, 3, 4, 5, 10, Inf)

TIME_IN_TREATMENT_BANDS <- c(
  "Under 6 months",
  "6 months-1 year",
  "1-2 years",
  "2-3 years",
  "3-4 years",
  "4-5 years",
  "5-10 years",
  "10 years or more"
)

TIME_IN_TREATMENT_LABEL <- "Time in treatment on this journey"

#' @param ost_type character. OST type in the period, from add_ost_type()
#' @param outcome factor. The side of the outcome, levels as labelled
#' @return factor. "<OST type>: <outcome>" for each of OST_TYPES (labelled as
#'   OST_LABELS) and each outcome level, OST type first; NA for other OST types
ost_outcome_group <- function(ost_type, outcome) {
  ost_labels <- unname(OST_LABELS[OST_TYPES])
  levels <- paste(
    rep(ost_labels, each = nlevels(outcome)),
    rep(levels(outcome), times = length(ost_labels)),
    sep = ": "
  )

  group <- dplyr::if_else(
    ost_type %in% OST_TYPES,
    paste(OST_LABELS[ost_type], outcome, sep = ": "),
    NA_character_
  )
  factor(group, levels)
}

#' Table 1 split by an outcome, for the whole period
#' @param people tibble. One row per client with age, sex, region and
#'   ost_type, from in_treatment() or completions_with_followup()
#' @param df data.frame. Linked dataset
#' @param event logical column in people. The outcome to split by
#' @param labels character(2). Column names for event TRUE then FALSE
#' @param by_ost_type logical(1). Split each side of the outcome by OST type
#'   too (ost_outcome_group())
#' @return tibble. From make_table1(), with TIME_IN_TREATMENT_LABEL as its
#'   last variable
outcome_characteristics <- function(
  people,
  df,
  event,
  labels,
  by_ost_type = FALSE
) {
  people |>
    dplyr::mutate(
      outcome = dplyr::if_else({{ event }}, labels[1], labels[2]) |>
        factor(labels)
    ) |>
    dplyr::transmute(
      client_random_id,
      n_jy,
      group = if (by_ost_type) {
        ost_outcome_group(ost_type, outcome)
      } else {
        outcome
      },
      age,
      sex,
      region
    ) |>
    add_table1_variables(df) |>
    dplyr::mutate(
      time_in_treatment_band = cut(
        as.numeric(los.days, units = "days") / 365.25,
        breaks = TIME_IN_TREATMENT_BREAKS,
        labels = TIME_IN_TREATMENT_BANDS,
        right = FALSE
      )
    ) |>
    make_table1(
      vars = c(
        TABLE1_VARS,
        rlang::set_names("time_in_treatment_band", TIME_IN_TREATMENT_LABEL)
      ),
      group = "group"
    )
}

#' Everyone in treatment in the period, split by whether their latest journey
#' ended in successful completion in the period (the numerator of
#' successful_completions()). Not completed includes journeys still open
#' @param journeys tibble. From summary_journeys()
#' @param df data.frame. Linked dataset
#' @param period_start,period_end Date(1). Default to the whole period
#' @param by_ost_type logical(1). Split by OST type too
#' @return tibble. From outcome_characteristics()
completion_characteristics <- function(
  journeys,
  df,
  period_start = WHOLE_PERIOD$period_start,
  period_end = WHOLE_PERIOD$period_end,
  by_ost_type = FALSE
) {
  journeys |>
    in_treatment(period_start, period_end) |>
    dplyr::mutate(
      completed_in_period = successful_completion &
        dplyr::between(disd_jy, period_start, period_end)
    ) |>
    outcome_characteristics(
      df,
      completed_in_period,
      c("Completed", "Not completed"),
      by_ost_type = by_ost_type
    )
}

#' Successful completions in the period with 6 months' follow-up, split by
#' re-presentation within 6 months (the cohort of re_presentations())
#' @param journeys tibble. From summary_journeys()
#' @param df data.frame. Linked dataset
#' @param period_start,period_end Date(1). Default to the whole period
#' @param months 6 or 12
#' @param by_ost_type logical(1). Split by OST type too
#' @return tibble. From outcome_characteristics()
re_presentation_characteristics <- function(
  journeys,
  df,
  period_start = WHOLE_PERIOD$period_start,
  period_end = WHOLE_PERIOD$period_end,
  months = 6,
  by_ost_type = FALSE
) {
  journeys |>
    completions_with_followup(period_start, period_end, months) |>
    outcome_characteristics(
      df,
      re_presented,
      c("Re-presented", "Not re-presented"),
      by_ost_type = by_ost_type
    )
}
