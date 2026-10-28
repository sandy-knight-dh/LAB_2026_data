# -------------------------------------------------------------------------
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: 01_setup.R
#
# Purpose:
# Defines parameters and settings used across all scripts in the LAB
# analysis project. This includes:
# - Analysis period
# - Folder paths for input data
# - Shared configuration variables required by downstream scripts
#
# Inputs:
# - period (month, year) used to construct data_path
# - Standard setup script: Q:/R_standard_modules_periodic/init/init.setup.R
#
# Outputs:
# - No direct outputs (this script sets up variables for other scripts)
#
# Dependencies:
# - init.setup.R (shared module)
#
# Notes:
# - This script must be sourced at the start of all build scripts
# - Changing 'period' will automatically update data_path
#
# PII:
# - No outputs produced, but downstream scripts using this setup will handle
#   identifiable data
# ------------------------------------------------------------------------
# 01_setup.R

# Working directory: run from the folder that holds master.R and R/. Every
# path in the scripts (R/..., data/interim/..., outputs/extracts/...) is
# relative to it, and the scripts don't change it.

# Folders the scripts write to. 02 to 05 write here before 06 (which also
# creates them) runs, so create them first
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/extracts", recursive = TRUE, showWarnings = FALSE)

# Load libraries
library(lubridate)

# Since this outputs a new and different set of files, I suggest increasing
# the version number - JC
# Addressed in version 6: ver is now 6.
# Version
# 6: builds the additional fields request (first OST SIR, per-drug SIR dates,
# first and latest TOP), England only, with CIR housing and mental health
# from the standard CIR modules, re-presentation in any English local
# authority, and the Treatment Progress Measure by year from the standard
# outputs. Columns of version 5 that the analysis reads keep their names and
# meaning.
ver <- 6

# Sort out date starts
data_start <- as.Date("2021-04-01")
period <- c(3, 2026)

# Standard modules: every module this project sources comes from here
std_modules <- "Q:/R_standard_modules_periodic"

# Load standard NDTMS setup
source(file.path(std_modules, "init/init.setup.R"))

# Treatment history across local authorities (adapted from the time out of
# treatment code in 02_build_journey.R)
source("R/functions_cross_la.R")

# The standard .rds inputs, converted to parquet once and read with
# tidypolars (only the columns and rows each script needs)
source("R/functions_inputs.R")

# (On data_path, which an earlier version defined here) Already calculated
# as data_dir.full - JC
# Addressed in version 6: data_path is no longer used; 03 reads from
# data_dir.full.
# Monthly reporting settings
run_type <- "monthly"

# Standard period dates created by init.setup.R
period.end <- mon.end
period.start <- rolling.start

