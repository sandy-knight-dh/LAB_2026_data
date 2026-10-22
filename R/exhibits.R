# Exhibit data --------------------------------------------------------------------
#
# The data behind every chart and table in the decks, following RS_performance
# (its R/exhibit_data.R and R/excel_output.R).
#
# - Each slide builder pipes the frame it is about to draw through
#   record_exhibit(), which files it under a name and passes it straight on.
#   write_deck() then saves the collection to output/<deck>.rds (named as the
#   deck) and a workbook with one sheet per exhibit, so every figure on a
#   slide can be read back without rebuilding it.
# - Where one frame feeds a chart and the table beside it, it is recorded once.
# - Rate frames keep their numerator and denominator.
# - Names are passed in rather than taken from the function, because most
#   builders draw several slides. They become sheet names, so are at most 31
#   characters and unique within a deck.
# - Each deck function starts with reset_exhibit_data(), so two decks built
#   in one R session keep their data apart.

EXHIBIT_NAME_MAX <- 31

.exhibits <- new.env(parent = emptyenv())
.exhibits$data <- list()

#' Empties the exhibit store, at the start of each deck
#' @return NULL, invisibly
reset_exhibit_data <- function() {
  .exhibits$data <- list()
  invisible(NULL)
}

#' Records a frame under a name and returns it unchanged, so it can sit in a
#' pipe immediately before the mschart:: or flextable:: call that draws it
#' @param data data.frame. What the exhibit draws
#' @param name character(1). Exhibit name, at most 31 characters
#' @return data, unchanged
record_exhibit <- function(data, name) {
  if (nchar(name) > EXHIBIT_NAME_MAX) {
    stop("Exhibit name longer than ", EXHIBIT_NAME_MAX, " characters: ", name)
  }
  if (name %in% names(.exhibits$data)) {
    stop("Exhibit name already used in this deck: ", name)
  }

  .exhibits$data[[name]] <- tibble::as_tibble(data)
  data
}

#' @return list. Every exhibit recorded since reset_exhibit_data(), named, in
#'   deck order
exhibit_data <- function() {
  .exhibits$data
}

# Files -----------------------------------------------------------------------------

#' @param data list. From exhibit_data()
#' @param path character(1). .rds file to write
#' @return path, invisibly
save_exhibit_data <- function(data, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, path)
  invisible(path)
}

#' One sheet per exhibit, styled with xlwriter. Percentages in this project
#' are 0 to 100, not proportions, so no column is given a percent format
#' @param data list. From exhibit_data(), or read back from the .rds
#' @param path character(1). .xlsx file to write
#' @return path, invisibly
write_exhibit_workbook <- function(data, path) {
  wb <- xlwriter::make_template_workbook()

  for (name in names(data)) {
    sheet <- data[[name]] |>
      # Factors and difftimes as text and numbers Excel can show
      dplyr::mutate(
        dplyr::across(dplyr::where(is.factor), as.character),
        dplyr::across(dplyr::where(\(x) inherits(x, "difftime")), as.numeric),
        dplyr::across(dplyr::where(is.logical), as.character)
      )

    wb <- openxlsx2::wb_add_worksheet(wb, name)
    wb <- xlwriter::add_styled_data(
      wb,
      df = sheet,
      row_pos = 1,
      col_pos = 1,
      year_cols = intersect(c("year", "period"), names(sheet))
    )
  }

  wb <- openxlsx2::wb_remove_worksheet(wb, sheet = "Sheet1")

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  wb$save(path)
  invisible(path)
}

#' Saves the deck's exhibits as .rds and .xlsx beside it, named as the deck:
#' <deck>.rds and <deck>.xlsx. The workbook is written from the .rds read back, so it can
#' only contain what was saved
#' @param deck_path character(1). The deck's .pptx path
#' @return character(2). The .rds and .xlsx paths, invisibly
write_exhibits <- function(deck_path) {
  stem <- sub("\\.pptx$", "", deck_path)
  rds <- paste0(stem, ".rds")
  xlsx <- paste0(stem, ".xlsx")

  save_exhibit_data(exhibit_data(), rds)
  write_exhibit_workbook(readRDS(rds), xlsx)

  invisible(c(rds, xlsx))
}

# Formatting --------------------------------------------------------------------------
#
# Number formats shared by the slides

#' @param x numeric. Counts
#' @return character. With thousands separators
fmt_n <- function(x) format(round(x), big.mark = ",", trim = TRUE, scientific = FALSE)

#' @param x numeric
#' @param digits integer(1). Decimal places
#' @return character
fmt_dp <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
