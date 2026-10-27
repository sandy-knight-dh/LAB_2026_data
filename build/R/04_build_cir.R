# -------------------------------------------------------------------------
# Script: 04_build_cir.R
#
# Purpose:
# Builds the CIR housing dataset for the LAB analysis.
#
# The CIR rows come from the standard module modules/mod.cir.R, run in
# 02_build_journey.R: CIR reviews appended to the housing recorded at each
# episode's assessment, with the journey number copied from the episode
# (attrb, dat, triaged, agncy), and rows after the period end or after the
# episode's discharge removed. This script keeps the housing fields and the
# project's clients.
#
# assign_date is the observation date:
# - cirdt for CIR review observations
# - triaged for journey assessment observations
#
# Journey-level housing measures are derived in 07_link_analysis_dataset.R.
#
# Version 5 built cir_jy here with the same steps as mod.cir.R; sourcing the
# module keeps one definition.
#
# Inputs:
# - data/interim/cir_jy_v{ver}.rds (from 02_build_journey.R)
# - data/interim/data_attrbdats_v{ver}.rds
#
# Output:
# - outputs/extracts/cir_data_with_identifiers_v{ver}.rds
#
# PII:
# - Contains identifiable client-level data (attrb, attrbdat).
# -------------------------------------------------------------------------
#setup and load
source("R/01_setup.R")

unique_attrbdat <- readRDS(
  paste0(
    "data/interim/data_attrbdats_v",
    ver,
    ".rds"
  )
) %>%
  select(attrbdat) %>%
  distinct()

cir_jy <- readRDS(
  paste0(
    "data/interim/cir_jy_v",
    ver,
    ".rds"
  )
)

message(
  "CIR and assessment rows from mod.cir.R: ",
  nrow(cir_jy)
)


# Clean homelessness field ------------------------------------------------

# HOMELESS can contain "" rather than NA.
cir_jy <- cir_jy %>%
  mutate(
    homeless = if_else(
      homeless == "",
      NA_character_,
      homeless
    )
  )


# Remove observations with no housing information -------------------------

cir_jy <- cir_jy %>%
  filter(
    !(is.na(housing) & is.na(homeless))
  )


# Create attrbdat and restrict to relevant clients -------------------------

cir_jy <- cir_jy %>%
  mutate(
    attrbdat = paste0(attrb, dat)
  ) %>%
  semi_join(
    unique_attrbdat,
    by = "attrbdat"
  ) %>%
  distinct()


# QA ----------------------------------------------------------------------

required_cir_fields <- c(
  "attrbdat",
  "n_jy",
  "assign_date",
  "housing",
  "homeless"
)

cir_field_check <- tibble(
  field = required_cir_fields,
  present = required_cir_fields %in% names(cir_jy)
)

print(cir_field_check)

stopifnot(all(cir_field_check$present))

message(
  "CIR records in final extract: ",
  nrow(cir_jy)
)

message(
  "CIR records with missing assign_date: ",
  sum(is.na(cir_jy$assign_date))
)

message(
  "CIR records with missing n_jy: ",
  sum(is.na(cir_jy$n_jy))
)


# Save CIR output ----------------------------------------------------------
# THIS OUTPUT CONTAINS PII

saveRDS(
  cir_jy,
  paste0(
    "outputs/extracts/cir_data_with_identifiers_v",
    ver,
    ".rds"
  )
)
