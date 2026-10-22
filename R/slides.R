# Slides --------------------------------------------------------------------------
#
# Table builders and deck writing shared by the summary deck and the
# matched-cohort deck, built with pptxwriter (the OHID template, chart theme
# and slide layout). Each builder records the frame it draws with
# record_exhibit() (R/exhibits.R) under the name it is given.

GROUP_LABELS <- c(
  methadone = "Methadone",
  oral_buprenorphine = "Oral buprenorphine",
  lab = "LAB"
)

# Table 1 is too long for one slide, so it is split over four: the
# TABLE1_VARS labels on each
TABLE1_SLIDE_VARS <- list(
  c("Age", "Sex", "Housing at assessment"),
  c("Region", "Year journey started"),
  c(
    "Crack use at assessment",
    "Benzodiazepine use at assessment",
    "Injecting on any TOP in journey (not baseline)",
    "Previous treatment journeys"
  ),
  c(
    "Time out of treatment before journey",
    "Time in treatment on earlier journeys"
  )
)

# Tables ----------------------------------------------------------------------------

#' The look shared by the deck's tables: Arial, no fills or grid, rules above
#' and below the header and at the foot
#' @param ft flextable
#' @param width numeric(1). Table width in inches
#' @param widths numeric. Each column's share of width
#' @param font_size numeric(1). Point size
#' @param row_height numeric(1). Row height in inches
#' @return flextable
style_slide_table <- function(
  ft,
  width,
  widths,
  font_size = 10,
  row_height = 0.2
) {
  border <- officer::fp_border(color = "black", width = 0.5)

  ft |>
    flextable::bold(part = "header") |>
    flextable::align(j = -1, align = "right", part = "all") |>
    flextable::font(fontname = "Arial", part = "all") |>
    flextable::fontsize(size = font_size, part = "all") |>
    flextable::border_remove() |>
    flextable::hline_top(border = border, part = "header") |>
    flextable::hline_bottom(border = border, part = "header") |>
    flextable::hline_bottom(border = border, part = "body") |>
    flextable::padding(
      padding.top = 0,
      padding.bottom = 0,
      padding.left = 3,
      padding.right = 3,
      part = "all"
    ) |>
    flextable::valign(valign = "center", part = "all") |>
    flextable::width(width = width * widths) |>
    flextable::height_all(height = row_height, part = "all") |>
    flextable::hrule(rule = "exact", part = "all")
}

#' @param table1 tibble. From make_table1()
#' @param vars character vector. Labels (names of TABLE1_VARS) to show
#' @param width numeric(1). Table width in inches
#' @param total logical(1). Include the Total row at the top
#' @param overall_label character(1). Heading for the Overall column
#' @return flextable. n (%) by exposure group, one variable's levels grouped
#'   under its label, with a light rule between variables
table1_flextable <- function(
  table1,
  vars,
  width,
  total = TRUE,
  overall_label = "All in treatment"
) {
  rows <- table1 |>
    dplyr::filter(
      variable %in% vars | (total & variable == "Total")
    ) |>
    format_table1()

  groups <- names(rows)[-(1:3)]
  names(rows) <- c(
    "Characteristic", "Level", overall_label,
    dplyr::coalesce(unname(GROUP_LABELS[groups]), groups)
  )

  # The label on the first row of each variable only
  last_rows <- which(!duplicated(rows$Characteristic, fromLast = TRUE))
  rows$Characteristic[duplicated(rows$Characteristic)] <- ""
  n_numeric <- ncol(rows) - 2

  # Compact, so that up to 18 rows fit under the commentary and above the note
  rows |>
    flextable::flextable() |>
    style_slide_table(
      width = width,
      widths = c(0.27, 0.17, rep(0.56 / n_numeric, n_numeric)),
      font_size = 9,
      row_height = 0.17
    ) |>
    flextable::align(j = 1:2, align = "left", part = "all") |>
    flextable::bold(j = 1, part = "body") |>
    flextable::hline(
      i = utils::head(last_rows, -1),
      border = officer::fp_border(color = "grey70", width = 0.5)
    )
}

# One colour per comparison on the estimate charts
# Covariate names for the balance chart, in propensity model order
# Slides ----------------------------------------------------------------------------

#' A table filling the content area of a plain slide
#' @param doc rpptx
#' @param title character(1). Slide title
#' @param table function(width) returning a flextable, so it can be sized to
#'   the content placeholder
#' @param note character(1) or NULL. Note at the foot of the slide
#' @return rpptx
add_table_slide <- function(doc, title, table, note = NULL) {
  doc <- officer::add_slide(
    doc,
    layout = "Title and Content",
    master = "ohid-theme"
  )
  width <- pptxwriter::layout_placeholder(doc, 3)$cx

  doc <- pptxwriter::add_slide_title(doc, title)
  if (!is.null(note)) {
    doc <- pptxwriter::add_slide_note(doc, text = note)
  }

  # Close under the title, to leave room for long tables above the note
  pptxwriter::add_table(doc, table(width), id = 3, down = 0.1)
}

#' The Table 1 slides
#' @param doc rpptx
#' @param table1 tibble. From make_table1()
#' @param title character(1). Slide title, numbered "(1 of 3)" and so on
#' @param note character(1). Note at the foot of each slide
#' @param overall_label character(1). Heading for the Overall column
#' @param name character(1). Exhibit name for record_exhibit()
#' @param commentary character, one per slide, or NULL. Written in the deck
#'   script; added last on each slide
#' @param slide_vars list of character vectors. Labels (names of TABLE1_VARS)
#'   on each slide
#' @return rpptx
add_table1_slides <- function(
  doc,
  table1,
  title,
  note,
  name,
  overall_label = "All in treatment",
  commentary = NULL,
  slide_vars = TABLE1_SLIDE_VARS
) {
  table1 <- record_exhibit(table1, name)
  if (!is.null(commentary) && length(commentary) != length(slide_vars)) {
    stop("Give one commentary per Table 1 slide (", length(slide_vars), ")")
  }

  for (i in seq_along(slide_vars)) {
    doc <- add_table_slide(
      doc,
      title = paste0(title, " (", i, " of ", length(slide_vars), ")"),
      table = \(width) {
        table1_flextable(
          table1,
          slide_vars[[i]],
          width = width,
          total = i == 1,
          overall_label = overall_label
        )
      },
      note = note
    )
    if (!is.null(commentary)) {
      doc <- pptxwriter::add_commentary(doc, commentary[[i]])
    }
  }

  doc
}

# Deck ------------------------------------------------------------------------------

#' Decks are date-stamped per iteration, so earlier versions are kept
#' @param stem character(1). Deck name, e.g. "LAB_Results"; lower-cased
#' @param date Date(1). Date of the iteration
#' @return character(1). output/<YYYY_mm_dd>_<stem in lower case>.pptx, e.g.
#'   output/2026_10_19_lab_results.pptx
dated_deck_path <- function(stem, date = Sys.Date()) {
  file.path("output", paste0(format(date, "%Y_%m_%d"), "_", tolower(stem), ".pptx"))
}

#' Writes the deck, then its exhibit data beside it (write_exhibits())
#' @param doc rpptx
#' @param path character(1). Where to write the .pptx
#' @return character(3). The .pptx, .rds and .xlsx paths, for a file target
write_deck <- function(doc, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  pptxwriter::write_pptx(doc, path)
  c(path, write_exhibits(path))
}
