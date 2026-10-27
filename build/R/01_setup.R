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

# Project working directory
setwd("Q:/Work Streams/Projects/Depot buprenorphine deep dive/LAB")

# Load libraries
library(lubridate)

# Version
ver <- 5

# Sort out date starts
data_start <- as.Date("2021-04-01")
period <- c(3, 2026)

# Load standard NDTMS setup
source("Q:/R_standard_modules_periodic/init/init.setup.R")

# Monthly reporting settings
run_type <- "monthly"

# Standard period dates created by init.setup.R
period.end <- mon.end
period.start <- rolling.start

