# Summary data deck ---------------------------------------------------------------
#
# write_summary_deck() writes output/<YYYY_mm_dd>_lab_summary_data.pptx from
# the summary_results target, with its exhibit data beside it (same name with
# .rds and .xlsx, one sheet per exhibit). Each section of the deck is a
# function that adds its slides to the deck and returns it.
#
# This deck carries no commentary: each slide has its chart or table and a
# note at the foot giving the definitions. The reader draws their own
# conclusions from the unadjusted figures. It has no title slide and no "what
# changed" slide: it opens on the first exhibit.

WHOLE_PERIOD_LABEL <- "April 2022 to March 2026"

SUMMARY_CHARACTERISTICS_NOTE <- paste(
  "Counts and column percentages of opiate clients, latest journey in the",
  "period. OST type in the period (LAB includes switchers); others count in",
  "the first column only. Housing at the journey's first assessment."
)

SUMMARY_TIME_IN_TREATMENT_NOTE <- paste(
  "Time in treatment on this journey is from the journey's start to its",
  "discharge, or to March 2026 if it is still open."
)

# Counts ------------------------------------------------------------------------------

#' New presentations and adults in treatment by OST type
#' @param doc rpptx
#' @param summary_results list. As write_summary_deck()
#' @return rpptx
add_summary_count_slides <- function(doc, summary_results) {
  doc <- add_count_slide(
    doc,
    summary_results$new_presentations,
    title = "New presentations by OST type",
    count_label = "New presentations",
    note = paste(
      "Financial years. Journeys starting in the year.",
      RECORDING_DELAY_NOTE
    ),
    ndtms = "new_presentations",
    name = "new_presentations"
  )

  add_count_slide(
    doc,
    summary_results$in_treatment,
    title = "Adults in treatment by OST type",
    count_label = "In treatment",
    note = paste(
      "Financial years. In treatment at any time in the year.",
      RECORDING_DELAY_NOTE,
      ndtms_note("lab_in_treatment")
    ),
    ndtms = "in_treatment",
    name = "in_treatment"
  )
}

# Early unplanned exits -----------------------------------------------------------------
#
# The indicator for all new presentations, then for each substance group
# (crack at assessment is recorded for every journey, so the two groups add
# up to the first slide), then the first slide's exits by discharge reason
# (each year's total is that slide's numerator; add_reason_slide() checks
# this).

#' @param doc rpptx
#' @param summary_results list. As write_summary_deck()
#' @return rpptx
add_early_exit_slides <- function(doc, summary_results) {
  doc <- add_rate_slide(
    doc,
    summary_results$early_unplanned_exits,
    title = "Early unplanned exits by OST type",
    numerator_label = "Early unplanned exits",
    denominator_label = "New presentations",
    chart_title = "% of new presentations",
    note = paste(
      "Exits within 12 weeks of starting treatment that weren't successful",
      "completions. As published, the cohort is delayed by 3 months:",
      "2025/26 is new presentations January to December 2025."
    ),
    ndtms = "early_unplanned_exits",
    name = "early_unplanned_exits"
  )

  for (group in SUBSTANCE_GROUPS) {
    doc <- add_rate_slide(
      doc,
      summary_results$early_unplanned_exits_by_substance |>
        dplyr::filter(substance_group == group) |>
        dplyr::select(-substance_group),
      title = paste0("Early unplanned exits by OST type: ", SUBSTANCE_LABELS[group]),
      numerator_label = "Early unplanned exits",
      denominator_label = "New presentations",
      chart_title = paste0("% of new presentations, ", SUBSTANCE_LABELS[group]),
      note = paste(
        "As the previous slide, limited to new presentations with",
        dplyr::if_else(
          group == "opiates_and_crack",
          "crack recorded at assessment as well as opiates.",
          "opiates but no crack recorded at assessment."
        ),
        "The two substance groups add up to the previous slide."
      ),
      ndtms = "early_unplanned_exits",
      name = paste0("eue_", group)
    )
  }

  add_reason_slide(
    doc,
    summary_results$early_unplanned_exit_reasons,
    totals = summary_results$early_unplanned_exits,
    title = "Early unplanned exits by discharge reason and OST type",
    total_label = "Early unplanned exits",
    note = paste(
      "Same exits as the first early unplanned exits slide, so each year's",
      "total is its numerator; percentages are of those exits. Custody is",
      "transferred in custody or retained in custody. Other is mainly",
      "transferred not in custody, plus treatment declined or withdrawn by the",
      "provider."
    ),
    ndtms = "early_unplanned_exits",
    name = "eue_reasons"
  )
}

# Outcomes ------------------------------------------------------------------------------

#' Successful completions, deaths in treatment and re-presentations
#' @param doc rpptx
#' @param summary_results list. As write_summary_deck()
#' @return rpptx
add_summary_outcome_slides <- function(doc, summary_results) {
  doc <- add_rate_slide(
    doc,
    summary_results$successful_completions,
    title = "Successful completions by OST type",
    numerator_label = "Successful completions",
    denominator_label = "In treatment",
    chart_title = "% of those in treatment",
    note = paste(
      "Financial years. Latest journey in the year ended in successful",
      "completion."
    ),
    ndtms = "successful_completions",
    name = "successful_completions"
  )

  doc <- add_rate_slide(
    doc,
    summary_results$deaths,
    title = "Deaths in treatment by OST type",
    numerator_label = "Deaths",
    denominator_label = "In treatment",
    chart_title = "% of those in treatment",
    note = paste(
      "Financial years. Journeys ending in the year with the client's death,",
      "from any cause. Deaths after leaving treatment are not counted."
    ),
    ndtms = "deaths",
    name = "deaths",
    .num_fmt = "0.00"
  )

  doc <- add_rate_slide(
    doc,
    summary_results$re_presentations_6m,
    title = "Re-presentations within 6 months of successful completion",
    numerator_label = "Re-presented",
    denominator_label = "Successful completions",
    chart_title = "% of successful completions",
    note = paste(
      "Financial years of completion. New journey within 6 months of",
      "completing. 2025/26 includes completions to September 2025 only."
    ),
    ndtms = "re_presentations",
    name = "re_presentations_6m"
  )

  add_rate_slide(
    doc,
    summary_results$re_presentations_12m,
    title = "Re-presentations within 12 months of successful completion",
    numerator_label = "Re-presented",
    denominator_label = "Successful completions",
    chart_title = "% of successful completions",
    note = paste(
      "Financial years of completion. New journey within 12 months of",
      "completing. 2025/26 is not shown: its completions don't yet have 12",
      "months' follow-up."
    ),
    ndtms = "re_presentations",
    name = "re_presentations_12m"
  )
}

# Characteristics ---------------------------------------------------------------------

#' Characteristics of adults in treatment and of new presentations, by OST
#' type, for the whole period
#' @param doc rpptx
#' @param summary_results list. As write_summary_deck()
#' @return rpptx
add_summary_characteristics_slides <- function(doc, summary_results) {
  doc <- add_table1_slides(
    doc,
    summary_results$characteristics_in_treatment,
    title = paste("Adults in treatment,", WHOLE_PERIOD_LABEL),
    note = paste(
      "In treatment at any time in", paste0(WHOLE_PERIOD_LABEL, "."),
      SUMMARY_CHARACTERISTICS_NOTE,
      "Housing is recorded only for journeys starting from April 2022.",
      "Age at April 2022, or journey start if later."
    ),
    name = "characteristics_in_tx"
  )

  add_table1_slides(
    doc,
    summary_results$characteristics_new_presentations,
    title = paste("New presentations,", WHOLE_PERIOD_LABEL),
    note = paste(
      "Adults whose journey started in", paste0(WHOLE_PERIOD_LABEL, "."),
      SUMMARY_CHARACTERISTICS_NOTE,
      "Age at journey start."
    ),
    name = "characteristics_new",
    overall_label = "All new presentations"
  )
}

# Characteristics by outcome --------------------------------------------------------
#
# The same characteristics, with a column for each side of the outcome
# instead of each OST type, plus time in treatment on the journey.

#' @param doc rpptx
#' @param summary_results list. As write_summary_deck()
#' @return rpptx
add_outcome_characteristics_slides <- function(doc, summary_results) {
  doc <- add_table1_slides(
    doc,
    summary_results$characteristics_completion,
    title = paste("Successful completion: characteristics,", WHOLE_PERIOD_LABEL),
    note = paste(
      "Adults in treatment at any time in", paste0(WHOLE_PERIOD_LABEL, ","),
      "the same clients as the in-treatment characteristics. Completed:",
      "latest journey in the period ended in successful completion in the",
      "period; not completed includes journeys still open. Counts and column",
      "percentages. Housing at the journey's first assessment, recorded only",
      "for journeys starting from April 2022.",
      SUMMARY_TIME_IN_TREATMENT_NOTE
    ),
    name = "characteristics_completion",
    overall_label = "All in treatment",
    slide_vars = OUTCOME_TABLE1_SLIDE_VARS
  )

  add_table1_slides(
    doc,
    summary_results$characteristics_re_presentation_6m,
    title = paste("Re-presentation within 6 months: characteristics,", WHOLE_PERIOD_LABEL),
    note = paste(
      "Adults whose latest successful completion in", WHOLE_PERIOD_LABEL,
      "has 6 months' follow-up (completions to September 2025), the same",
      "cohort as the 6-month re-presentation slide. Re-presented: new journey",
      "within 6 months of completing. Counts and column percentages. Housing",
      "at the journey's first assessment, recorded only for journeys starting",
      "from April 2022.",
      SUMMARY_TIME_IN_TREATMENT_NOTE
    ),
    name = "characteristics_repres_6m",
    overall_label = "All completions",
    slide_vars = OUTCOME_TABLE1_SLIDE_VARS
  )
}

# Characteristics by outcome and OST type ------------------------------------------
#
# The two tables above split by OST type as well as the outcome
# (outcome_characteristics(by_ost_type = TRUE)), in the exhibit workbook only:
# no slides (the user's choice, 2026-10-20).

#' Records the outcome characteristics split by OST type, without slides
#' @param summary_results list. As write_summary_deck()
#' @return NULL, invisibly
record_outcome_characteristics_by_ost <- function(summary_results) {
  record_exhibit(
    summary_results$characteristics_completion_by_ost,
    "characteristics_completion_ost"
  )
  record_exhibit(
    summary_results$characteristics_re_presentation_6m_by_ost,
    "characteristics_repres_6m_ost"
  )
  invisible(NULL)
}

# Deck ----------------------------------------------------------------------------------

#' Builds and writes the summary data deck, with its exhibit data beside it
#' @param summary_results list. The summary_results target: indicators by
#'   financial year (new_presentations, in_treatment, early_unplanned_exits,
#'   early_unplanned_exits_by_substance, early_unplanned_exit_reasons,
#'   successful_completions, deaths, re_presentations_6m,
#'   re_presentations_12m) and characteristics tables for the whole period
#'   (characteristics_in_treatment, characteristics_new_presentations,
#'   characteristics_completion, characteristics_re_presentation_6m, and the
#'   last two split by OST type too: characteristics_completion_by_ost,
#'   characteristics_re_presentation_6m_by_ost), each from its function in
#'   R/summary_data.R
#' @param path character(1). Where to write the .pptx
#' @return character(3). The .pptx, .rds and .xlsx paths (write_deck())
write_summary_deck <- function(summary_results, path = dated_deck_path(SUMMARY_DECK_STEM)) {
  reset_exhibit_data()

  # One section at a time rather than a pipe, so the exhibits are recorded in
  # deck order (see write_results_deck())
  doc <- pptxwriter::load_template()
  doc <- add_summary_count_slides(doc, summary_results)
  doc <- add_early_exit_slides(doc, summary_results)
  doc <- add_summary_outcome_slides(doc, summary_results)
  doc <- add_summary_characteristics_slides(doc, summary_results)
  doc <- add_outcome_characteristics_slides(doc, summary_results)
  record_outcome_characteristics_by_ost(summary_results)

  write_deck(doc, path)
}
