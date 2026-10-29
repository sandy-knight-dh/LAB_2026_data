# -------------------------------------------------------------------------
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: profile_memory.R
#
# Purpose:
# Runs the build as master.R does (01 to 08 and the QA scripts, in one R
# session), one top-level step at a time, and records the memory after each
# step, to find where the build runs out of memory.
#
# Run from the folder that holds master.R and R/, instead of master.R:
#   Rscript R/profile_memory.R
# or, for some scripts only (01_setup.R always runs first; the scripts read
# the files earlier scripts wrote, so these must exist):
#   Rscript R/profile_memory.R R/07_link_analysis_dataset.R
#
# Outputs:
# - outputs/memory_profile.csv, rewritten after every step, so that
#   it survives a crash: script, line, the step's first 70 characters,
#   seconds, the session's peak memory so far and the memory in use after
#   the step (MB). On Linux the peak is reset before each step, so it is
#   the step's own peak; on Windows it is the peak so far, and the step
#   that raised it is the one to look at.
#
# PII:
# - None: the file holds code, timings and memory sizes only, and can be
#   brought back.
# ------------------------------------------------------------------------

if (!file.exists("R/01_setup.R")) {
  stop(
    "Run from the folder that contains master.R and R/; ",
    "the working directory is ", getwd()
  )
}

profile_build <- function(scripts) {
  if (length(scripts) == 0) {
    scripts <- c(
      "R/02_build_journey.R", "R/03_build_top.R", "R/04_build_cir.R",
      "R/05_build_sir.R", "R/06_deidentify_outputs.R",
      "R/07_link_analysis_dataset.R", "R/08_standard_by_year.R",
      "R/qa_column_manifest.R", "R/qa_representation.R"
    )
  }
  scripts <- c("R/01_setup.R", scripts)
  linux <- file.exists("/proc/self/status")

  # Memory in MB: peak so far and in use now
  memory_mb <- function() {
    if (linux) {
      status <- readLines("/proc/self/status")
      field <- function(name) {
        as.numeric(gsub("[^0-9]", "", grep(paste0("^", name, ":"), status,
                                            value = TRUE))) / 1024
      }
      c(peak = field("VmHWM"), now = field("VmRSS"))
    } else {
      info <- ps::ps_memory_info()
      c(peak = info[["peak_wset"]], now = info[["wset"]]) / 1024^2
    }
  }
  reset_peak <- function() {
    if (linux) writeLines("5", "/proc/self/clear_refs")
  }

  dir.create("outputs", showWarnings = FALSE)
  output <- "outputs/memory_profile.csv"
  rows <- list()
  for (script in scripts) {
    expressions <- parse(script, keep.source = TRUE)
    sources <- attr(expressions, "srcref")
    for (i in seq_along(expressions)) {
      code <- paste(as.character(sources[[i]]), collapse = " ")
      reset_peak()
      start <- Sys.time()
      eval(expressions[[i]], globalenv())
      memory <- memory_mb()
      rows[[length(rows) + 1]] <- data.frame(
        script = basename(script),
        line = sources[[i]][1],
        step = substr(gsub("\\s+", " ", code), 1, 70),
        seconds = round(as.numeric(Sys.time() - start, units = "secs"), 1),
        peak_mb = round(memory[["peak"]]),
        after_mb = round(memory[["now"]])
      )
      utils::write.csv(do.call(rbind, rows), output, row.names = FALSE)
    }
    message(script, " done; ", round(memory[["now"]]), " MB in use")
  }
}

profile_build(commandArgs(trailingOnly = TRUE))
