# -------------------------------------------------------------------------
# Script: 00_run_all.R
#
# Purpose:
# Master script to run the full LAB dataset build process from setup through
# to the final linked analysis dataset.
#
# Key functions:
# - Check it is run from the build folder (it doesn't change the working
#   directory)
# - Run the build scripts in order, each in its own R session
# - Deidentify intermediate outputs
# - Create final linked LAB analysis dataset
#
# Inputs:
# - NDTMS journey, TOP, CIR and SIR source files
# - Standard periodic setup/modules
#
# Outputs:
# - Identified source extracts
# - Deidentified source extracts
# - Final linked LAB analysis dataset (rds and feather)
# - Standard-module measures by financial year (feather)
#
# Notes:
# - Scripts must be run in order because downstream modules depend on outputs
#   from earlier steps.
# - Outputs remain sensitive and should be handled according to IG
# -------------------------------------------------------------------------
# Working directory ------------------------------------------------------
# Run from the folder that holds this file and R/ (for example the LAB
# project folder, or an RStudio project there). Every path in the scripts is
# relative to it, and none of them changes it.
if (!file.exists("R/01_setup.R")) {
  stop(
    "Run the build from the folder that contains master.R and R/; ",
    "the working directory is ", getwd()
  )
}

# Run scripts --------------------------------------------------------
# Each script runs in a new R session, which ends with it: R holds on to the
# memory of everything a script loads, so in one session the national
# journey, TOP and SIR files and every script's objects added up (the
# build's peak was the sum of the scripts' rather than the largest). Each
# script sources 01_setup.R itself and reads what earlier scripts saved, so
# nothing else changes. Its console output appears as before, and if it
# fails the build stops there.
run_script <- function(script) {
  message("Running ", script)
  callr::rscript(script, wd = getwd())
  invisible(NULL)
}

run_script("R/02_build_journey.R")
run_script("R/03_build_top.R")
run_script("R/04_build_cir.R")
run_script("R/05_build_sir.R")

run_script("R/06_deidentify_outputs.R")

run_script("R/07_link_analysis_dataset.R")

# Standard-module measures by financial year (Treatment Progress Measure,
# age and latest-journey flags), read from the March standard outputs
run_script("R/08_standard_by_year.R")

# QA to bring out of the secure environment (counts only) ------------------

run_script("R/qa_column_manifest.R")
run_script("R/qa_representation.R")

# Run once, separately: the standard successful completion and
# non-re-presentation module for 2024/25, to compare with the analysis
# source("R/validate_scnr.R")
