# Standard inputs as parquet, read with tidypolars
#
# The standard files the build reads (the journey file, the Z-verified TOP
# and SIR files, the March recent_journey_pri files) are .rds, which R can
# only read whole: every row of every client in England, and every column
# (the journey file has about 170, the recent_journey_pri files about 570).
# Reading them whole is most of the build's memory.
#
# Instead, each is converted once to parquet in a separate R session, which
# frees all its memory when it ends, and the scripts then read only the
# columns and rows they need: scan_parquet_polars() sets up the read,
# filter(), semi_join() and select() are done by polars while it reads,
# and as_tibble() brings just the result into R. Everything after that is
# the build's own dplyr code, as before.
#
# The parquet copies hold the same identifiable data as the .rds files, so
# they stay in data/interim/inputs/ (gitignored, never brought back). A copy
# is remade whenever its .rds is newer. Delete the folder to free the disk
# space.
#
# Rules for reading this way (checked on synthetic data, 2026-10-28, when
# every file the build writes was identical to the .rds version's):
# - Keep clients with filter(attrbdat %in% ...), not semi_join(): polars
#   applies a filter while it reads, so other clients' rows are never read,
#   and the file's row order is kept.
# - It pays where the part read is a small part of the file (the journey
#   file, the TOP file and the March files: 20 to 55% less memory). Memory
#   polars has used stays with the session, so reading most of a file this
#   way gains nothing: our clients have most SIRs, and 05 uses the same
#   memory either way (it reads this way for consistency, and is faster).
# - Never select() the columns of data a standard module then reads: the
#   modules on Q: are not the reference copy and read columns it doesn't
#   (mod.effective_treatment.R reads modst_ep), so 02 reads every column of
#   the journey file for our clients.
# - What comes back is a tibble, even from a data.table: use distinct(), not
#   unique(), whose data frame method pastes every row into a string.
# - difftime columns come back in seconds rather than days (the same
#   durations); set units() back where it matters.
# - Do any arithmetic in R after as_tibble(): in polars, a date minus a date
#   is in milliseconds, so as.numeric(d2 - d1) is not days.

library(tidypolars)

input_parquet <- function(rds_path) {
  parquet_path <- file.path(
    "data/interim/inputs",
    paste0(gsub("[^A-Za-z0-9]+", "_", sub("\\.rds$", "", rds_path)),
           ".parquet")
  )
  if (!file.exists(parquet_path) ||
      file.mtime(parquet_path) < file.mtime(rds_path)) {
    dir.create(dirname(parquet_path), recursive = TRUE, showWarnings = FALSE)
    message("Converting ", rds_path, " to parquet (separate R session)")
    # Written under a temporary name first, so that a conversion that fails
    # part way never leaves a file that looks complete
    temporary_path <- paste0(parquet_path, ".partial")
    # The session reports its peak memory (MB), which is the most the
    # build needs at any point for this file
    peak_mb <- callr::r(
      function(from, to) {
        arrow::write_parquet(readRDS(from), to)
        if (file.exists("/proc/self/status")) {
          status <- readLines("/proc/self/status")
          as.numeric(gsub("[^0-9]", "", grep("^VmHWM:", status,
                                              value = TRUE))) / 1024
        } else {
          ps::ps_memory_info()[["peak_wset"]] / 1024^2
        }
      },
      args = list(rds_path, temporary_path)
    )
    file.rename(temporary_path, parquet_path)
    message("Converted; that session's peak memory was ", round(peak_mb),
            " MB")
  }
  parquet_path
}

scan_input <- function(rds_path) {
  scan_parquet_polars(input_parquet(rds_path))
}
