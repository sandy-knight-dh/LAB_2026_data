# Summary data slides ---------------------------------------------------------------
#
# Slide builders for the summary data deck: unadjusted counts and rates by OST
# type and financial year, then characteristics for the whole period. Built
# from the indicators in R/summary_data.R with the table and slide helpers in
# R/slides.R. The deck itself is put together by write_summary_deck()
# (R/summary_deck.R).
#
# Every builder records the frame it draws (record_exhibit(), R/exhibits.R),
# under the name it is given, and every count or rate slide's note names its
# nearest NDTMS indicator (ndtms_note(), R/ndtms_crosswalk.R).

SUMMARY_DECK_STEM <- "LAB_Summary_Data"

# The characteristics tables split by an outcome have three numeric columns
# and an extra variable (time in treatment on the journey), so they are split
# over four slides differently from TABLE1_SLIDE_VARS
OUTCOME_TABLE1_SLIDE_VARS <- list(
  c("Age", "Sex", "Housing at assessment"),
  c("Region", "Year journey started"),
  c(
    "Crack use at assessment",
    "Benzodiazepine use at assessment",
    "Injecting on any TOP in journey (not baseline)",
    "Previous treatment journeys",
    "Time out of treatment before journey"
  ),
  c(
    "Time in treatment on earlier journeys",
    TIME_IN_TREATMENT_LABEL
  )
)

SUBSTANCE_LABELS <- c(
  opiates_only = "opiates only",
  opiates_and_crack = "opiates and crack"
)

OST_LABELS <- c(
  all = "All",
  methadone = "Methadone",
  oral_buprenorphine = "Oral buprenorphine",
  lab = "LAB",
  not_recorded = "Prescribing, drug not recorded"
)

# One colour per OST type, the same on every chart
OST_COLOURS <- c(
  All = "#3D3D3D",
  Methadone = "#12436D",
  `Oral buprenorphine` = "#28A197",
  LAB = "#F46A25",
  `Prescribing, drug not recorded` = "#A6A6A6"
)

# Said on every slide, so the counts are read the same way throughout
SUMMARY_DEFINITIONS_NOTE <- paste(
  "Adult opiate clients; individuals, latest journey in the period.",
  "OST type is for the period: LAB if the span of LAB records overlaps it,",
  "otherwise the oral OST on a journey with prescribing in it. Mixed oral OST",
  "and no prescribing count in All only."
)

# On the count slides, where most of the drug not recorded is
RECORDING_DELAY_NOTE <- paste(
  "The OST drug is recorded at reviews, often months after starting, so",
  "2025/26 is incomplete."
)

# Chart and table -------------------------------------------------------------------

#' Clustered bars: one group of bars per financial year, one bar per OST type
#' @param indicator tibble. From an indicator function in R/summary_data.R
#' @param y character(1). Column to chart: "n" or "rate"
#' @param .title character(1). Chart title
#' @param .num_fmt character(1). Number format for the labels and axis
#' @param include_all logical(1). Include the All bars. Left out of the count
#'   charts, where they would dwarf the OST types
#' @return ms_barchart
indicator_chart <- function(
  indicator,
  y,
  .title,
  .num_fmt = "0.0",
  include_all = TRUE
) {
  if (!include_all) {
    indicator <- dplyr::filter(indicator, ost_type != "all")
  }
  series <- OST_LABELS[as.character(unique(indicator$ost_type))]

  indicator |>
    dplyr::mutate(
      ost_label = factor(
        OST_LABELS[as.character(ost_type)],
        levels = OST_LABELS
      )
    ) |>
    mschart::ms_barchart(x = "year", y = y, group = "ost_label") |>
    mschart::chart_labels(title = .title, xlab = "", ylab = "") |>
    mschart::chart_data_labels(
      num_fmt = .num_fmt,
      show_val = TRUE,
      position = "outEnd"
    ) |>
    pptxwriter::add_chart_theme(legend_position = "b") |>
    pptxwriter::set_data_label_formats(size = 8) |>
    mschart::chart_ax_y(num_fmt = .num_fmt, limit_min = 0) |>
    # After the theme, which would otherwise set its own colours
    mschart::chart_data_fill(values = OST_COLOURS[series]) |>
    mschart::chart_data_stroke(values = OST_COLOURS[series])
}

#' One row per financial year and OST type
#' @param indicator tibble. From an indicator function in R/summary_data.R
#' @param columns named character vector. Columns to show (n, numerator,
#'   denominator, rate), named by their heading
#' @param width numeric(1). Table width in inches
#' @return flextable
indicator_flextable <- function(indicator, columns, width) {
  rows <- indicator |>
    dplyr::mutate(
      Year = dplyr::if_else(duplicated(year), "", year),
      `OST type` = OST_LABELS[as.character(ost_type)],
      dplyr::across(dplyr::any_of(c("n", "numerator", "denominator")), fmt_n),
      dplyr::across(dplyr::any_of("rate"), fmt_dp)
    ) |>
    dplyr::select(Year, `OST type`, dplyr::all_of(columns))

  # A light rule between years
  last_rows <- which(!duplicated(indicator$year, fromLast = TRUE))
  data_widths <- rep(0.6 / length(columns), length(columns))

  # Compact, so that 16 rows fit under the title and above the note
  rows |>
    flextable::flextable() |>
    style_slide_table(
      width = width,
      widths = c(0.12, 0.28, data_widths),
      font_size = 9,
      row_height = 0.17
    ) |>
    flextable::align(j = 1:2, align = "left", part = "all") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::hline(
      i = utils::head(last_rows, -1),
      border = officer::fp_border(color = "grey70", width = 0.5)
    ) |>
    # Room for headings over two lines
    flextable::height(height = 0.35, part = "header")
}

#' One row per OST type and one column per financial year: the count slides
#' have too many OST types to fit as rows within years
#' @param counts tibble. From new_presentation_counts() or in_treatment_counts()
#' @param width numeric(1). Table width in inches
#' @return flextable
count_flextable <- function(counts, width) {
  rows <- counts |>
    dplyr::mutate(
      `OST type` = OST_LABELS[as.character(ost_type)],
      n = fmt_n(n)
    ) |>
    tidyr::pivot_wider(id_cols = `OST type`, names_from = year, values_from = n)

  n_years <- ncol(rows) - 1

  rows |>
    flextable::flextable() |>
    style_slide_table(
      width = width,
      widths = c(0.36, rep(0.64 / n_years, n_years))
    ) |>
    flextable::align(j = 1, align = "left", part = "all") |>
    # A light rule between All and the OST types
    flextable::hline(
      i = 1,
      border = officer::fp_border(color = "grey70", width = 0.5)
    )
}

# Slides ------------------------------------------------------------------------------

#' A chart on the left and its table on the right
#' @param doc rpptx
#' @param title character(1). Slide title
#' @param chart ms_barchart. From indicator_chart()
#' @param table function(width) returning a flextable, so it can be sized to
#'   the right-hand placeholder
#' @param note character(1). Said before the definitions
#' @param definitions character(1). Definitions said on every slide of the
#'   deck
#' @return rpptx
add_indicator_slide <- function(
  doc,
  title,
  chart,
  table,
  note,
  definitions = SUMMARY_DEFINITIONS_NOTE
) {
  doc <- officer::add_slide(
    doc,
    layout = "2_content_large",
    master = "ohid-theme"
  ) |>
    pptxwriter::add_slide_title(title) |>
    pptxwriter::add_slide_note(text = paste(note, definitions)) |>
    pptxwriter::add_chart_above_note(chart, id = 8)

  width <- pptxwriter::layout_placeholder(doc, 9)$cx

  pptxwriter::add_table(doc, table(width), id = 9, down = 0.1)
}

#' A count by year and OST type
#' @param doc rpptx
#' @param counts tibble. From new_presentation_counts() or in_treatment_counts()
#' @param title character(1). Slide title
#' @param count_label character(1). Name for the count, e.g. "In treatment"
#' @param note character(1). Said before the NDTMS note and
#'   SUMMARY_DEFINITIONS_NOTE
#' @param ndtms character(1). Measure in NDTMS_CROSSWALK
#' @param name character(1). Exhibit name for record_exhibit()
#' @return rpptx
add_count_slide <- function(doc, counts, title, count_label, note, ndtms, name) {
  counts <- record_exhibit(counts, name)

  add_indicator_slide(
    doc,
    title = title,
    chart = indicator_chart(
      counts,
      "n",
      paste(count_label, "by OST type"),
      .num_fmt = "#,##0",
      include_all = FALSE
    ),
    table = \(width) count_flextable(counts, width),
    note = paste(note, ndtms_note(ndtms))
  )
}

#' A rate by year and OST type
#' @param doc rpptx
#' @param rates tibble. From a rate indicator in R/summary_data.R
#' @param title character(1). Slide title
#' @param numerator_label,denominator_label character(1). Table headings
#' @param chart_title character(1). Chart title
#' @param note character(1). Said before the NDTMS note and
#'   SUMMARY_DEFINITIONS_NOTE
#' @param ndtms character(1). Measure in NDTMS_CROSSWALK
#' @param name character(1). Exhibit name for record_exhibit()
#' @param .num_fmt character(1). Number format for the chart
#' @param definitions character(1). Definitions said on every slide of the
#'   deck
#' @return rpptx
add_rate_slide <- function(
  doc,
  rates,
  title,
  numerator_label,
  denominator_label,
  chart_title,
  note,
  ndtms,
  name,
  .num_fmt = "0.0",
  definitions = SUMMARY_DEFINITIONS_NOTE
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
    note = paste(note, ndtms_note(ndtms)),
    definitions = definitions
  )
}

# Counts split by reason --------------------------------------------------------------
#
# A rate indicator's numerator split by a reason, e.g. early unplanned exits
# by discharge reason (early_unplanned_exit_reasons()). A table only: a block
# of rows for each financial year, the total first (labelled with the year)
# and then each reason, with a column for each OST type. The totals come from
# the rate indicator itself, and the builder stops if the reasons don't add up
# to them, so the slide can't disagree with the rate slide it splits.
#
# No cell is left empty: LibreOffice draws an empty cell at the template's
# default font size, which makes its row much taller than the others.

#' @param reasons tibble. year, ost_type, reason (factor), n, pct, e.g. from
#'   early_unplanned_exit_reasons()
#' @param totals tibble. The rate indicator the reasons split (year,
#'   ost_type, numerator), e.g. from early_unplanned_exits()
#' @param total_label character(1). Label for each year's total row, followed
#'   by the year
#' @param width numeric(1). Table width in inches
#' @return flextable. Total rows show the count, reason rows the count and
#'   its share of the total
reason_flextable <- function(reasons, totals, total_label, width) {
  sums <- reasons |>
    dplyr::summarise(n = sum(n), .by = c(year, ost_type)) |>
    dplyr::inner_join(totals, by = c("year", "ost_type"))
  if (nrow(sums) != nrow(totals) || any(sums$n != sums$numerator)) {
    stop("The reasons don't add up to the totals they split")
  }

  total_rows <- totals |>
    dplyr::transmute(
      year,
      ost_type,
      reason = total_label,
      cell = fmt_n(numerator)
    )
  reason_rows <- reasons |>
    dplyr::transmute(
      year,
      ost_type,
      reason = as.character(reason),
      cell = paste0(fmt_n(n), " (", fmt_dp(pct), "%)")
    )

  rows <- dplyr::bind_rows(total_rows, reason_rows) |>
    dplyr::mutate(
      reason = factor(reason, c(total_label, levels(reasons$reason))),
      ost_label = factor(OST_LABELS[as.character(ost_type)], levels = OST_LABELS)
    ) |>
    tidyr::pivot_wider(
      id_cols = c(year, reason),
      names_from = ost_label,
      values_from = cell,
      names_sort = TRUE
    ) |>
    dplyr::arrange(year, reason)

  is_total <- rows$reason == total_label
  last_rows <- which(!duplicated(rows$year, fromLast = TRUE))
  n_types <- ncol(rows) - 2

  rows |>
    dplyr::mutate(
      `Discharge reason` = dplyr::if_else(
        is_total,
        paste0(total_label, ", ", year),
        as.character(reason)
      ),
      .before = 1
    ) |>
    dplyr::select(-year, -reason) |>
    flextable::flextable() |>
    style_slide_table(
      width = width,
      widths = c(0.32, rep(0.68 / n_types, n_types)),
      font_size = 9,
      row_height = 0.17
    ) |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::bold(i = which(is_total), part = "body") |>
    # Reasons indented under their total
    flextable::padding(i = which(!is_total), j = 1, padding.left = 14) |>
    flextable::hline(
      i = utils::head(last_rows, -1),
      border = officer::fp_border(color = "grey70", width = 0.5)
    )
}

#' A rate indicator's numerator split by reason, as a table
#' @param doc rpptx
#' @param reasons tibble. From early_unplanned_exit_reasons()
#' @param totals tibble. The rate indicator the reasons split, from
#'   early_unplanned_exits()
#' @param title character(1). Slide title
#' @param total_label character(1). Label for each year's total row
#' @param note character(1). Said before the NDTMS note and
#'   SUMMARY_DEFINITIONS_NOTE
#' @param ndtms character(1). Measure in NDTMS_CROSSWALK
#' @param name character(1). Exhibit name for record_exhibit()
#' @param definitions character(1). Definitions said on every slide of the
#'   deck
#' @return rpptx
add_reason_slide <- function(
  doc,
  reasons,
  totals,
  title,
  total_label,
  note,
  ndtms,
  name,
  definitions = SUMMARY_DEFINITIONS_NOTE
) {
  reasons <- record_exhibit(reasons, name)

  add_table_slide(
    doc,
    title = title,
    table = \(width) reason_flextable(reasons, totals, total_label, width),
    note = paste(note, ndtms_note(ndtms), definitions)
  )
}
