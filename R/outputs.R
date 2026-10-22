# Outputs ---------------------------------------------------------------------
#
# Everything the project produces goes to output/ as plain data files, which
# any tool can read:
# - Tables (counts, rates, characteristics tables, match summaries): one CSV
#   per table, named <prefix>_<name>.csv, e.g. summary_in_treatment.csv and
#   matched_deaths.csv. Each is one element of summary_results or
#   matched_summary_results, unrounded.
# - Row-level datasets (one row per journey or client): feather, named
#   <name>.feather. Too big for CSV, and feather keeps the column types
#   (dates, factors). Read them with arrow::read_feather().
# The files are overwritten on each run, not date-stamped. The datasets hold
# client identifiers, and output/ is gitignored: they stay with the data.

OUTPUT_DIR <- "output"

#' Writes each table to a CSV file
#' @param tables list. Named tibbles, e.g. summary_results
#' @param prefix character(1). Start of each file name, e.g. "summary"
#' @param dir character(1). Directory to write to
#' @return character. The paths written, for a file target
write_tables <- function(tables, prefix, dir = OUTPUT_DIR) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(dir, paste0(prefix, "_", names(tables), ".csv"))
  purrr::walk2(tables, paths, \(table, path) {
    arrow::write_csv_arrow(table, path)
  })
  paths
}

#' Writes each dataset to a feather file
#' @param datasets list. Named tibbles
#' @param dir character(1). Directory to write to
#' @return character. The paths written, for a file target
write_datasets <- function(datasets, dir = OUTPUT_DIR) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(dir, paste0(names(datasets), ".feather"))
  purrr::walk2(datasets, paths, \(dataset, path) {
    arrow::write_feather(dataset, path)
  })
  paths
}
