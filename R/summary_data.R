# Summary data ----------------------------------------------------------------

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
  # Re-presentation: the client's next journey in any English local
  # authority triaged within 183 or 365 days of discharge; NA for open
  # journeys and when the client was already in treatment elsewhere at
  # discharge (see completions_with_followup())
  "repres_any_la_6m",
  "repres_any_la_12m",
  "followup_available_6m",
  "followup_available_12m",
  # Number of SIRs (reviews) linked to the journey, for the early unplanned
  # exits by every OST type
  "number_sir_records"
)

# Prescribing modalities, as in the standard modules (mod.prescribing_length.R
# and the prescribing modality start in mod.subinterventions.R). Nearly all
# are 94; 98 and 2 are a few hundred records in the whole dataset
PRESCRIBING_MODALS <- c(2, 3, 56, 72, 94, 98)

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
    dplyr::filter(modal %in% PRESCRIBING_MODALS, !is.na(modst)) |>
    dplyr::mutate(rx_end = dplyr::coalesce(modend, disd_jy, DATA_END)) |>
    dplyr::summarise(
      rx_start = min(modst),
      rx_end = max(rx_end),
      .by = c(client_random_id, n_jy)
    ) |>
    tibble::as_tibble()
}

summary_journeys <- function(df) {
  df |>
    collapse_to_journeys(group_cols = SUMMARY_COLS) |>
    dplyr::left_join(
      prescribing_spans(df),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::mutate(
      early_unplanned_exit = effective_tx == 0,
      successful_completion = disrsn_jy.grp == 1,
      died = disrsn_jy == 89,
      substance_group = dplyr::if_else(
        drug_a.crack == 1,
        "opiates_and_crack",
        "opiates_only"
      )
    )
}

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

in_treatment <- function(journeys, period_start, period_end) {
  journeys |>
    journeys_in_period(period_start, period_end) |>
    opiate_clients(period_start, period_end) |>
    dplyr::mutate(age = age_at_ref)
}

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

with_all_ost_types <- function(data, types = OST_TYPES) {
  dplyr::bind_rows(
    dplyr::mutate(data, ost_type = "all"),
    dplyr::filter(data, ost_type %in% types)
  ) |>
    dplyr::mutate(ost_type = factor(ost_type, c("all", types)))
}

count_by_ost_type <- function(data, types = OST_TYPES) {
  data |>
    with_all_ost_types(types) |>
    dplyr::count(ost_type)
}

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

for_each_year <- function(calculation) {
  purrr::pmap(FINANCIAL_YEARS, \(year, period_start, period_end) {
    calculation(period_start, period_end) |>
      dplyr::mutate(year = year, .before = 1)
  }) |>
    purrr::list_rbind()
}

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

in_treatment_counts <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      in_treatment(period_start, period_end) |>
      count_by_ost_type(COUNT_OST_TYPES)
  })
}

early_unplanned_exits <- function(journeys, group = NULL) {
  for_each_year(\(period_start, period_end) {
    cohort <- early_exit_cohort(journeys, period_start, period_end)
    if (!is.null(group)) {
      cohort <- dplyr::filter(cohort, substance_group == group)
    }
    rate_by_ost_type(cohort, early_unplanned_exit)
  })
}

early_exit_cohort <- function(journeys, period_start, period_end) {
  new_presentations(
    journeys,
    period_start - EARLY_EXIT_DELAY,
    period_end - EARLY_EXIT_DELAY
  )
}

early_unplanned_exits_by_substance <- function(journeys) {
  purrr::map(SUBSTANCE_GROUPS, \(group) {
    early_unplanned_exits(journeys, group) |>
      dplyr::mutate(substance_group = group, .before = 1)
  }) |>
    purrr::list_rbind()
}

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

re_presentations <- function(journeys, months) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      completions_with_followup(period_start, period_end, months) |>
      rate_by_ost_type(re_presented)
  }) |>
    dplyr::filter(denominator > 0)
}

#' Successful completions in the period with the follow-up in the data, each
#' client once by their latest such journey. Re-presentation is
#' repres_any_la_6m / repres_any_la_12m: the client's next journey in any
#' English local authority, not only the one they completed in. A completion
#' whose flag is NA is left out: the client's next journey started on or
#' before the discharge date, so they were already in treatment elsewhere
#' (the standard measure, mod.scnr.R, likewise drops a completion during
#' another journey)
#' @param journeys tibble. From summary_journeys()
#' @param period_start,period_end Date(1)
#' @param months numeric. Follow-up, 6 or 12
#' @return tibble. One row per client, with re_presented (logical) and age
completions_with_followup <- function(
  journeys,
  period_start,
  period_end,
  months
) {
  followup <- paste0("followup_available_", months, "m")
  repres <- paste0("repres_any_la_", months, "m")

  journeys |>
    journeys_in_period(period_start, period_end) |>
    dplyr::filter(
      successful_completion,
      dplyr::between(disd_jy, period_start, period_end),
      .data[[followup]] == 1,
      !is.na(.data[[repres]])
    ) |>
    opiate_clients(period_start, period_end) |>
    dplyr::mutate(re_presented = .data[[repres]] == 1, age = age_at_ref)
}

# Treatment progress ----------------------------------------------------------
#
# The NDTMS Treatment Progress Measure (mod.composite.R) for the clients in
# treatment in each year. It is not computed here: the standard monthly
# process computes it for the 12 months to each March, and the dataset build
# (build/R/08_standard_by_year.R) takes lof.composite_latest_grp_sml for each
# client's latest journey in the year from that output (standard_by_year).
# Categories:
# - showing: showing substantial progress (completed, not using, or
#   substantially reduced use of problem substances);
# - not_showing: not yet showing substantial progress (unplanned exit,
#   completed with an acute housing problem, use not substantially reduced);
# - missing_data: TOP or journey data missing;
# - new_client: in treatment 3 months or less, too little data to classify.
# The rate is showing out of showing plus not_showing; missing_data and
# new_client are left out of the rate, as in the published measure, and
# reported alongside. no_record counts clients in treatment whose journey
# has no row in standard_by_year (the standard output's latest journey in
# the year was a different one, or it wasn't matched).

TPM_CATEGORIES <- c("showing", "not_showing", "missing_data", "new_client")

#' Each client's Treatment Progress Measure category, by journey and year
#' @param standard_by_year tibble. From build/R/08_standard_by_year.R:
#'   client_random_id, n_jy, year, lof.composite_latest_grp_sml (1 to 4)
#' @return tibble. client_random_id, n_jy, year, progress (character, one
#'   of TPM_CATEGORIES)
treatment_progress_categories <- function(standard_by_year) {
  standard_by_year |>
    dplyr::transmute(
      client_random_id,
      n_jy,
      year,
      progress = TPM_CATEGORIES[lof.composite_latest_grp_sml]
    )
}

#' Treatment Progress Measure counts and rate, by year and OST type
#' @param people tibble. Clients with year, ost_type, progress (NA if no
#'   record) and weights (1 for the summary data, matching weights for the
#'   matched data)
#' @return tibble. year, ost_type, the count in each category, no_record,
#'   numerator (showing), denominator (showing + not_showing), rate (%).
#'   Counts are sums of weights
progress_rates <- function(people) {
  people |>
    dplyr::summarise(
      showing = sum(weights * (progress %in% "showing")),
      not_showing = sum(weights * (progress %in% "not_showing")),
      missing_data = sum(weights * (progress %in% "missing_data")),
      new_client = sum(weights * (progress %in% "new_client")),
      no_record = sum(weights * is.na(progress)),
      .by = c(year, ost_type)
    ) |>
    dplyr::mutate(
      numerator = showing,
      denominator = showing + not_showing,
      rate = 100 * numerator / denominator
    ) |>
    dplyr::arrange(year, ost_type)
}

#' Treatment Progress Measure among clients in treatment in each year
#' @param journeys tibble. From summary_journeys()
#' @param standard_by_year tibble. From build/R/08_standard_by_year.R
#' @return tibble. From progress_rates()
treatment_progress <- function(journeys, standard_by_year) {
  progress <- treatment_progress_categories(standard_by_year)

  purrr::pmap(FINANCIAL_YEARS, \(year, period_start, period_end) {
    journeys |>
      in_treatment(period_start, period_end) |>
      dplyr::mutate(year = year, weights = 1)
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(progress, by = c("client_random_id", "n_jy", "year")) |>
    with_all_ost_types() |>
    progress_rates()
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

# Early unplanned exits by every OST type -------------------------------------
#
# What the All rate of early_unplanned_exits() is made of. Every client in the
# early exit cohort has exactly one of ALL_OST_TYPES (add_ost_type()), so the
# types add up to All; the other tables show only OST_TYPES, and mixed oral
# OST, prescribing with the drug not recorded and no prescribing count in All
# only. The early_unplanned_exits() output is unchanged. composition_table()
# summarises it over the four years, as the summary deck's slide does.
#
# - no_sir: the journey has no SIR (review record) at all, so no OST could have
#   been recorded on it. OST types are taken from SIRs, so LAB, methadone,
#   oral buprenorphine and mixed oral OST all have at least one (a handful of
#   LAB journeys show none, from how the SIR count was linked).
# - No prescribing is almost entirely journeys with no prescribing
#   intervention at all (checked 2026-10-23: about 30 a year had prescribing
#   that started after the cohort window).

ALL_OST_TYPES <- c(
  "methadone",
  "oral_buprenorphine",
  "lab",
  "mixed_oral",
  "not_recorded",
  "no_prescribing"
)

#' Early unplanned exits for each of ALL_OST_TYPES, for each financial year
#' @param journeys tibble. From summary_journeys()
#' @return tibble. year, ost_type (factor, ALL_OST_TYPES), numerator (early
#'   unplanned exits), denominator (new presentations), rate (%), no_sir and
#'   no_sir_exits (integer: new presentations, and early unplanned exits,
#'   whose journey has no SIR). Every combination is present
early_unplanned_exits_all_types <- function(journeys) {
  for_each_year(\(period_start, period_end) {
    journeys |>
      early_exit_cohort(period_start, period_end) |>
      dplyr::mutate(
        exited = dplyr::coalesce(early_unplanned_exit, FALSE),
        no_sir = dplyr::coalesce(number_sir_records, 0) == 0,
        ost_type = factor(ost_type, ALL_OST_TYPES)
      ) |>
      dplyr::summarise(
        numerator = sum(exited),
        denominator = dplyr::n(),
        no_sir = sum(no_sir),
        no_sir_exits = sum(exited & no_sir),
        .by = ost_type
      ) |>
      tidyr::complete(
        ost_type,
        fill = list(
          numerator = 0L,
          denominator = 0L,
          no_sir = 0L,
          no_sir_exits = 0L
        )
      ) |>
      dplyr::mutate(
        rate = 100 * numerator / denominator,
        .after = denominator
      ) |>
      dplyr::arrange(ost_type)
  })
}

#' One row per OST type and All: the rate by year, the share of All's early
#' unplanned exits and the share with no SIR, over the four years
#' @param by_type tibble. From early_unplanned_exits_all_types()
#' @param totals tibble. From early_unplanned_exits(), for the All row
#' @return tibble. ost_type (character, ALL_OST_TYPES then all), one rate
#'   column per year (numeric), exit_share and no_sir_pct (numeric, %)
composition_table <- function(by_type, totals) {
  all_totals <- dplyr::filter(totals, ost_type == "all")
  sums <- by_type |>
    dplyr::summarise(
      numerator = sum(numerator),
      denominator = sum(denominator),
      .by = year
    ) |>
    dplyr::inner_join(all_totals, by = "year", suffix = c("", "_all"))
  adds_up <- nrow(sums) == nrow(all_totals) &&
    all(sums$numerator == sums$numerator_all) &&
    all(sums$denominator == sums$denominator_all)
  if (!adds_up) {
    stop("The OST types don't add up to All on the early unplanned exits slide")
  }

  with_all <- dplyr::bind_rows(
    dplyr::mutate(by_type, ost_type = as.character(ost_type)),
    by_type |>
      dplyr::summarise(
        dplyr::across(c(numerator, denominator, no_sir, no_sir_exits), sum),
        .by = year
      ) |>
      dplyr::mutate(ost_type = "all", rate = 100 * numerator / denominator)
  )

  rates <- with_all |>
    tidyr::pivot_wider(
      id_cols = ost_type,
      names_from = year,
      values_from = rate
    )

  pooled <- with_all |>
    dplyr::summarise(
      exits = sum(numerator),
      starts = sum(denominator),
      no_sir = sum(no_sir),
      .by = ost_type
    ) |>
    dplyr::mutate(
      exit_share = 100 * exits / exits[ost_type == "all"],
      no_sir_pct = 100 * no_sir / starts
    ) |>
    dplyr::select(ost_type, exit_share, no_sir_pct)

  dplyr::inner_join(rates, pooled, by = "ost_type") |>
    dplyr::arrange(match(ost_type, c(ALL_OST_TYPES, "all")))
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
