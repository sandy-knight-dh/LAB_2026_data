# Matched-cohort summary deck ---------------------------------------------------------
#
# write_matched_summary_deck() writes
# output/<YYYY_mm_dd>_lab_matched_summary_data.pptx from the
# matched_summary_results target, with its exhibit data beside it (same name
# with .rds and .xlsx, one sheet per exhibit). A short version of the summary
# deck with propensity score matched comparators (R/matched_summary.R): one
# count slide, the outcome rates and the characteristics table. The match
# summaries (numbers matched and balance) are in the workbook only.
#
# Like the summary deck it has no commentary, title or "what changed" slide.
# Its notes don't name an NDTMS indicator: the comparator figures are not
# NDTMS figures, and every slide says so (MATCHED_DEFINITIONS_NOTE).

MATCHED_SUMMARY_DECK_STEM <- "LAB_Matched_Summary_Data"

MATCHED_DEFINITIONS_NOTE <- paste(
  "Propensity score matched clients, not NDTMS figures: each year, the",
  "summary deck's LAB clients matched to methadone clients (3 per LAB client)",
  "and, separately, to oral buprenorphine clients (2 per LAB client) on their",
  "characteristics and time in treatment. Adult opiate clients, latest",
  "journey in the period; OST type for the period."
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

#' Matched clients in treatment by financial year
#' @param doc rpptx
#' @param matched_summary_results list. As write_matched_summary_deck()
#' @return rpptx
add_matched_count_slide <- function(doc, matched_summary_results) {
  counts <- record_exhibit(matched_summary_results$in_treatment, "matched_in_treatment")

  add_indicator_slide(
    doc,
    title = "Matched clients in treatment by OST type",
    chart = indicator_chart(
      counts,
      "n",
      "Matched clients in treatment",
      .num_fmt = "#,##0"
    ),
    table = \(width) count_flextable(counts, width),
    note = paste(
      "Financial years. In treatment at any time in the year. The ratio of",
      "comparators to LAB clients is set by the matching."
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
    title = "Early unplanned exits: matched clients",
    numerator_label = "Early unplanned exits",
    denominator_label = "New presentations",
    chart_title = "% of matched new presentations",
    note = paste(
      "Exits within 12 weeks of starting treatment that weren't successful",
      "completions. As published, the cohort is delayed by 3 months: 2025/26",
      "is new presentations January to December 2025, matched within each",
      "year. LAB's rate is biased low, most in the earlier years: LAB was then",
      "mostly started months into a journey, too late for an early exit to",
      "count as LAB."
    ),
    name = "matched_early_unplanned_exits"
  )

  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$successful_completions,
    title = "Successful completions: matched clients",
    numerator_label = "Successful completions",
    denominator_label = "In treatment",
    chart_title = "% of matched clients in treatment",
    note = paste(
      "Financial years. Latest journey in the year ended in successful",
      "completion."
    ),
    name = "matched_successful_completions"
  )

  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$deaths,
    title = "Deaths in treatment: matched clients",
    numerator_label = "Deaths",
    denominator_label = "In treatment",
    chart_title = "% of matched clients in treatment",
    note = paste(
      "Financial years. Journeys ending in the year with the client's death,",
      "from any cause. Deaths after leaving treatment are not counted."
    ),
    name = "matched_deaths",
    .num_fmt = "0.00"
  )

  doc <- add_matched_rate_slide(
    doc,
    matched_summary_results$re_presentations_6m,
    title = "Re-presentations within 6 months of successful completion: matched clients",
    numerator_label = "Re-presented",
    denominator_label = "Successful completions",
    chart_title = "% of matched successful completions",
    note = paste(
      "Financial years of completion, matched within each year. New journey",
      "within 6 months of completing. 2025/26 includes completions to",
      "September 2025 only. From 2024/25 there are too few completions for the",
      "full ratio: 1 oral buprenorphine (and in 2025/26, 2 methadone)",
      "completion per LAB completion."
    ),
    name = "matched_re_presentations_6m"
  )

  add_matched_rate_slide(
    doc,
    matched_summary_results$re_presentations_12m,
    title = "Re-presentations within 12 months of successful completion: matched clients",
    numerator_label = "Re-presented",
    denominator_label = "Successful completions",
    chart_title = "% of matched successful completions",
    note = paste(
      "Financial years of completion, matched within each year. New journey",
      "within 12 months of completing. 2025/26 is not shown: its completions",
      "don't yet have 12 months' follow-up. In 2024/25 there are too few",
      "completions for the full ratio: 1 oral buprenorphine completion per LAB",
      "completion."
    ),
    name = "matched_re_presentations_12m"
  )
}

#' Table 1 of the matched clients
#' @param doc rpptx
#' @param matched_summary_results list. As write_matched_summary_deck()
#' @return rpptx
add_matched_characteristics_slides <- function(doc, matched_summary_results) {
  add_table1_slides(
    doc,
    matched_summary_results$characteristics,
    title = paste("Matched clients in treatment,", WHOLE_PERIOD_LABEL),
    note = paste(
      "Counts and column percentages of the matched clients in treatment at",
      "any time in", paste0(WHOLE_PERIOD_LABEL, ","), "matched over the whole",
      "period; unweighted, as the matching weights are almost all 1. OST type",
      "in the period (LAB includes switchers). Housing at the journey's first",
      "assessment, recorded only for journeys starting from April 2022. Age at",
      "April 2022, or journey start if later. Propensity score matched",
      "clients, not NDTMS figures."
    ),
    name = "matched_characteristics",
    overall_label = "All matched clients"
  )
}

# Deck ------------------------------------------------------------------------------

#' @param matched_summary_results list. From the matched_summary_results
#'   target: in_treatment, early_unplanned_exits, successful_completions,
#'   deaths, re_presentations_6m, re_presentations_12m, characteristics and
#'   matches (from match_summaries(), for the workbook)
#' @param path character(1). Where to write the .pptx
#' @return character. The .pptx, .rds and .xlsx paths, for the file target
write_matched_summary_deck <- function(
  matched_summary_results,
  path = dated_deck_path(MATCHED_SUMMARY_DECK_STEM)
) {
  reset_exhibit_data()

  # One section at a time rather than a pipe, so the exhibits are recorded in
  # deck order
  doc <- pptxwriter::load_template()
  doc <- add_matched_count_slide(doc, matched_summary_results)
  doc <- add_matched_outcome_slides(doc, matched_summary_results)
  doc <- add_matched_characteristics_slides(doc, matched_summary_results)

  # Workbook only: how many were matched in each population, and how well
  record_exhibit(matched_summary_results$matches, "matches")

  write_deck(doc, path)
}
