# Matched-cohort summary deck ---------------------------------------------------------
#
# write_matched_summary_deck() writes
# output/<YYYY_mm_dd>_lab_matched_summary_data.pptx from the
# matched_summary_results target, with its exhibit data beside it (same name
# with .rds and .xlsx, one sheet per exhibit). A short version of the summary
# deck on the propensity score matched journeys (R/matched_summary.R): one
# count slide, the outcome rates and the characteristics table.
#
# Like the summary deck it has no commentary, title or "what changed" slide.
# Its notes don't name an NDTMS indicator: these are not NDTMS figures, and
# every slide says so (MATCHED_DEFINITIONS_NOTE), so that nobody reconciles
# them with the summary deck or the dashboard.

MATCHED_SUMMARY_DECK_STEM <- "LAB_Matched_Summary_Data"

MATCHED_DEFINITIONS_NOTE <- paste(
  "Propensity score matched journeys, not NDTMS figures: LAB journeys from",
  "April 2022 matched to methadone journeys (3 per LAB journey) and,",
  "separately, to oral buprenorphine journeys (2 per LAB journey) on their",
  "characteristics at prescribing start. Journeys, not clients."
)

# Slides --------------------------------------------------------------------------

#' A rate by year and group, as add_rate_slide() without the NDTMS note
#' @param doc rpptx
#' @param rates tibble. From a rate indicator in R/matched_summary.R
#' @param title character(1). Slide title
#' @param numerator_label,denominator_label character(1). Table headings
#' @param chart_title character(1). Chart title
#' @param note character(1). Said before MATCHED_DEFINITIONS_NOTE
#' @param name character(1). Exhibit name for record_exhibit()
#' @param .num_fmt character(1). Number format for the chart
#' @return rpptx
add_matched_rate_slide <- function(
  doc,
  rates,
  title,
  numerator_label,
  denominator_label,
  chart_title,
  note,
  name,
  .num_fmt = "0.0"
) {
  rates <- record_exhibit(rates, name)

  columns <- stats::setNames(
    c("numerator", "denominator", "rate"),
    c(numerator_label, denominator_label, "%")
  )

  add_indicator_slide(
    doc,
    title = title,
    chart = indicator_chart(rates, "rate", chart_title, .num_fmt = .num_fmt),
    table = \(width) indicator_flextable(rates, columns, width),
    note = note,
    definitions = MATCHED_DEFINITIONS_NOTE
  )
}

# Sections ------------------------------------------------------------------------

#' Matched journeys in treatment by financial year
#' @param doc rpptx
#' @param matched_summary_results list. As write_matched_summary_deck()
#' @return rpptx
add_matched_count_slide <- function(doc, matched_summary_results) {
  counts <- record_exhibit(matched_summary_results$in_treatment, "matched_in_treatment")

  add_indicator_slide(
    doc,
    title = "Matched journeys in treatment by OST type",
    chart = indicator_chart(
      counts,
      "n",
      "Matched journeys in treatment",
      .num_fmt = "#,##0"
    ),
    table = \(width) count_flextable(counts, width),
    note = paste(
      "Financial years. Matched journeys open at any time in the year. The",
      "matched journeys start in April 2022, so 2022/23 is low. The ratio of",
      "comparators to LAB is set by the matching."
    ),
    definitions = MATCHED_DEFINITIONS_NOTE
  )
}

#' Early unplanned exits, successful completions, deaths and re-presentations
#' @param doc rpptx
#' @param matched_summary_results list. As write_matched_summary_deck()
#' @return rpptx
add_matched_outcome_slides <- function(doc, matched_summary_results) {
  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$early_unplanned_exits,
    title = "Early unplanned exits: matched journeys",
    numerator_label = "Early unplanned exits",
    denominator_label = "Journeys starting",
    chart_title = "% of matched journeys starting",
    note = paste(
      "Exits within 12 weeks of the journey's start that weren't successful",
      "completions, as in the summary deck. The cohort is delayed by 3",
      "months: 2025/26 is journeys starting January to December 2025, and",
      "2022/23 covers April to December 2022 only. Biased in LAB's favour: a",
      "journey counts as LAB only once LAB is recorded, a median of 84 days",
      "after prescribing starts, so few LAB journeys can exit within 12 weeks."
    ),
    name = "matched_early_unplanned_exits"
  )

  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$successful_completions,
    title = "Successful completions: matched journeys",
    numerator_label = "Successful completions",
    denominator_label = "In treatment",
    chart_title = "% of matched journeys in treatment",
    note = "Financial years. Matched journeys open in the year that ended in successful completion in the year.",
    name = "matched_successful_completions"
  )

  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$deaths,
    title = "Deaths in treatment: matched journeys",
    numerator_label = "Deaths",
    denominator_label = "In treatment",
    chart_title = "% of matched journeys in treatment",
    note = paste(
      "Financial years. Matched journeys open in the year that ended in the",
      "client's death, from any cause, in the year."
    ),
    name = "matched_deaths",
    .num_fmt = "0.00"
  )

  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$re_presentations_6m,
    title = "Re-presentations within 6 months of successful completion: matched journeys",
    numerator_label = "Re-presented",
    denominator_label = "Successful completions",
    chart_title = "% of matched successful completions",
    note = paste(
      "Financial years of completion. The results deck's re-presentation",
      "matches: completed journeys, with about 2 comparators per LAB journey.",
      "New journey within 6 months of completing. 2025/26 includes",
      "completions to September 2025 only."
    ),
    name = "matched_re_presentations_6m"
  )

  add_matched_rate_slide(
    doc,
    matched_summary_results$re_presentations_12m,
    title = "Re-presentations within 12 months of successful completion: matched journeys",
    numerator_label = "Re-presented",
    denominator_label = "Successful completions",
    chart_title = "% of matched successful completions",
    note = paste(
      "Financial years of completion. The results deck's re-presentation",
      "matches. New journey within 12 months of completing. 2025/26 is not",
      "shown: its completions don't yet have 12 months' follow-up."
    ),
    name = "matched_re_presentations_12m"
  )
}

#' Table 1 of the matched journeys
#' @param doc rpptx
#' @param matched_summary_results list. As write_matched_summary_deck()
#' @return rpptx
add_matched_characteristics_slides <- function(doc, matched_summary_results) {
  add_table1_slides(
    doc,
    matched_summary_results$characteristics,
    title = "Matched journeys: characteristics",
    note = paste(
      "Counts and column percentages of the matched journeys (time-to-event",
      "matches), unweighted: the matching weights are almost all 1. Age at",
      "prescribing start. Housing at the journey's first assessment. The",
      "first column is every matched journey. Propensity score matched",
      "journeys, not NDTMS figures; journeys, not clients."
    ),
    name = "matched_characteristics",
    overall_label = "All matched journeys"
  )
}

# Deck ------------------------------------------------------------------------------

#' @param matched_summary_results list. From the matched_summary_results
#'   target: in_treatment, early_unplanned_exits, successful_completions,
#'   deaths, re_presentations_6m, re_presentations_12m, characteristics
#' @param path character(1). Where to write the .pptx
#' @return character. The .pptx, .rds and .xlsx paths, for the file target
write_matched_summary_deck <- function(
  matched_summary_results,
  path = dated_deck_path(MATCHED_SUMMARY_DECK_STEM)
) {
  reset_exhibit_data()

  # One section at a time rather than a pipe, so the exhibits are recorded in
  # deck order (see write_results_deck())
  doc <- pptxwriter::load_template()
  doc <- add_matched_count_slide(doc, matched_summary_results)
  doc <- add_matched_outcome_slides(doc, matched_summary_results)
  doc <- add_matched_characteristics_slides(doc, matched_summary_results)

  write_deck(doc, path)
}
