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
# - Run setup and source-specific build scripts
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

source("R/01_setup.R")

source("R/02_build_journey.R")
source("R/03_build_top.R")
source("R/04_build_cir.R")
source("R/05_build_sir.R")

source("R/06_deidentify_outputs.R")

source("R/07_link_analysis_dataset.R")

# Standard-module measures by financial year (Treatment Progress Measure,
# age and latest-journey flags), read from the March standard outputs
source("R/08_standard_by_year.R")

# QA to bring out of the secure environment (counts only) ------------------

source("R/qa_column_manifest.R")
source("R/qa_representation.R")

# Run once, separately: the standard successful completion and
# non-re-presentation module for 2024/25, to compare with the analysis
# source("R/validate_scnr.R")
