# NDTMS crosswalk -----------------------------------------------------------------
#
# How each measure in the summary deck relates to its nearest NDTMS indicator, so
# that figures can be reconciled with the dashboards. One row per measure:
# - ndtms_indicator: the nearest published indicator
#   (docs/data_docs/indicator_definitions.csv), or the nearest NDTMS concept
#   where none is published.
# - definition: ours, in one sentence.
# - unit, origin, population, period, exposure: how ours differs on each, or
#   "Same" where it doesn't.
#   - unit: journeys or individuals.
#   - origin: journey start (triage), time zero (prescribing start) or
#     discharge.
#   - population: everyone in treatment, closed journeys or person-time.
#   - period: rolling 12 months to March, or pooled over the cohort.
#   - exposure: OST in the period, or over the whole journey.
# - dashboard: the dashboard figure for the 12 months to March 2026, where one
#   is known.
# - note: one or two sentences for the foot of the slide, naming the indicator
#   and the main difference. Used by ndtms_note().
#
# ndtms_note() puts the note at the foot of each summary deck slide. The
# matched-cohort deck has no NDTMS notes.

NDTMS_CROSSWALK <- tibble::tribble(
  ~measure, ~label, ~ndtms_indicator, ~definition, ~unit, ~origin, ~population, ~period, ~exposure, ~dashboard, ~note,

  # Summary deck ----
  "new_presentations",
  "New presentations",
  "New presentations",
  "Adult opiate clients whose journey started in the financial year.",
  "Same (individuals, latest journey in period)",
  "Same (journey start)",
  "Opiate clients by latest journey",
  "Same (financial year)",
  "OST type in the period",
  NA_character_,
  "Nearest NDTMS indicator: new presentations, with the same definition; the split by OST type is ours and depends on OST being recorded, which lags.",

  "in_treatment",
  "Adults in treatment",
  "Adults in treatment",
  "Adult opiate clients with a journey open at any time in the financial year.",
  "Same (individuals, latest journey in period)",
  "Same (journey start)",
  "Same (opiate clients by latest journey)",
  "Same (financial year)",
  "OST type in the period",
  "About 134,500 (12 months to March 2026)",
  "Nearest NDTMS indicator: adults in treatment (opiate), same definition; 2025/26 should be close to the dashboard's 134,500.",

  "lab_in_treatment",
  "LAB in treatment",
  "Long acting buprenorphine uptake (opiate dashboard)",
  "Adult opiate clients in treatment whose span of LAB records (first to last LAB SIR) overlaps the year.",
  "Same (individuals)",
  "Same",
  "Same",
  "Same (financial year)",
  "Span of LAB records overlaps the period; the dashboard's rule is not published",
  "6,523 (4.85%, 12 months to March 2026)",
  "Nearest NDTMS indicator: LAB uptake on the opiate dashboard (6,523 in the 12 months to March 2026). Ours counts anyone whose span of LAB records overlaps the year; the dashboard's rule isn't published, so the two differ by definition.",

  "early_unplanned_exits",
  "Early unplanned exits",
  "Treatment completed/retained 12 weeks or more (early unplanned exits)",
  "New presentations whose journey ended within 12 weeks other than by successful completion, from the NDTMS effective treatment flag.",
  "Same (individuals)",
  "Same (journey start)",
  "Same (new presentations, delayed 3 months)",
  "Same (financial year delayed 3 months)",
  "OST type in the period",
  NA_character_,
  "Nearest NDTMS indicator: early unplanned exits (inverse of 12-week retention), same definition and 3-month delay.",

  "successful_completions",
  "Successful completions",
  "Successful completions (in place of the treatment progress measure)",
  "Clients whose latest journey in the year ended in successful completion during the year, as a share of those in treatment.",
  "Same (individuals, latest journey in period)",
  "Same",
  "Same (all in treatment)",
  "Same (financial year)",
  "OST type in the period",
  NA_character_,
  "Nearest NDTMS indicator: successful completions. The treatment progress measure that was asked for needs TOP days of use, which the extract doesn't have, so completions are shown instead.",

  "deaths",
  "Deaths in treatment",
  "Deaths in treatment",
  "Clients whose journey ended in the year with discharge reason 'Incomplete - client died', as a share of those in treatment.",
  "Same (individuals)",
  "Same",
  "Same (all in treatment)",
  "Same (financial year)",
  "OST type in the period",
  "2,559 (1.90%, 12 months to March 2026)",
  "Nearest NDTMS indicator: deaths in treatment (opiate dashboard: 2,559, 1.90% in the 12 months to March 2026). Same definition; ours is by latest journey in the year.",

  "re_presentations",
  "Re-presentations after successful completion",
  "Successful completion without re-presentation within 6 months (PHOF C19a)",
  "Successful completions in the year followed by a new journey within 6 or 12 months, as a share of those completions.",
  "Individuals, latest completion in the year",
  "Discharge",
  "Successful completions (PHOF's denominator is everyone in treatment)",
  "Completions in the financial year with full follow-up",
  "OST type in the period",
  NA_character_,
  "Nearest NDTMS indicator: PHOF C19a counts completions not followed by re-presentation within 6 months, as a share of everyone in treatment. Ours is the share of completions that did re-present."
)

#' The NDTMS sentence for a slide note
#' @param measure character(1). A measure in NDTMS_CROSSWALK
#' @return character(1)
ndtms_note <- function(measure) {
  note <- NDTMS_CROSSWALK$note[NDTMS_CROSSWALK$measure == measure]
  if (length(note) != 1) {
    stop("No NDTMS crosswalk row for ", measure)
  }
  note
}
