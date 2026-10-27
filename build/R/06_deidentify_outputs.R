# -------------------------------------------------------------------------
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: 06_deidentify_outputs.R
#
# Purpose:
# Produces pseudonymised Journey, TOP, CIR and SIR extracts by replacing
# attrbdat/attrb with a project-specific client_random_id.
#
# All extracts generated during this run use the same client ID lookup and
# timestamp. Only timestamped deidentified extracts are saved to reduce the
# risk of combining files created using different client ID mappings.
#
# Inputs:
# - outputs/extracts/journey_data_with_identifiers_v{ver}.rds
# - outputs/extracts/top_data_with_identifiers_v{ver}.rds
# - outputs/extracts/cir_data_with_identifiers_v{ver}.rds
# - outputs/extracts/sir_data_with_identifiers_v{ver}.rds
# - data/interim/data_attrbdats_v{ver}.rds
#
# Outputs:
# - outputs/extracts/journey_data_deidentified_v{ver}_t{timestamp}.rds
# - outputs/extracts/top_data_deidentified_v{ver}_t{timestamp}.rds
# - outputs/extracts/cir_data_deidentified_v{ver}_t{timestamp}.rds
# - outputs/extracts/sir_data_deidentified_v{ver}_t{timestamp}.rds
# - data/interim/client_id_lookup_v{ver}_t{timestamp}.rds
# - data/interim/latest_deidentified_timestamp_v{ver}.rds
#
# PII:
# - Identified inputs contain attrbdat/attrb.
# - The client ID lookup remains PII and must not be shared.
# - Pseudonymised outputs remain sensitive analytical data..
# Setup -------------------------------------------------------------------

source("R/01_setup.R")

library(tidyverse)

#output folders
dir.create(
  "outputs/extracts",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/interim",
  recursive = TRUE,
  showWarnings = FALSE
)

# Load data ---------------------------------------------------------
data_attrbdats <- readRDS(
  paste0(
    "data/interim/data_attrbdats_v",
    ver,
    ".rds"
  )
) %>%
  select(attrbdat) %>%
  distinct()

#Check client id in attrbdats
stopifnot(
  "attrbdat" %in% names(data_attrbdats),
  !any(is.na(data_attrbdats$attrbdat)),
  !anyDuplicated(data_attrbdats$attrbdat)
)

message(
  "Clients in IDS: ",
  nrow(data_attrbdats)
)

# Create client ID lookup -------------------------------------------------

# sample.int() generates a random permutation of 1:n.
# Each attrbdat receives one unique client_random_id.
client_id_lookup <- data_attrbdats %>%
  mutate(
    client_random_id = sample.int(
      n(),
      replace = FALSE
    )
  )


# QA the client ID lookup 

stopifnot(
  nrow(client_id_lookup) == nrow(data_attrbdats),
  !any(is.na(client_id_lookup$client_random_id)),
  !anyDuplicated(client_id_lookup$attrbdat),
  !anyDuplicated(client_id_lookup$client_random_id)
)


# Save client ID lookup securely 
# THIS FILE REMAINS PII AND MUST NOT BE SHARED.

saveRDS(
  client_id_lookup,
  file.path(
    getwd(),
    paste0(
      "data/interim/client_id_lookup_v",
      ver,
      "_t",
      run_timestamp,
      ".rds"
    )
  )
)


# Load identified extracts ------------------------------------------------

journey_output <- readRDS(
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/journey_data_with_identifiers_v",
      ver,
      ".rds"
    )
  )
)

top <- readRDS(
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/top_data_with_identifiers_v",
      ver,
      ".rds"
    )
  )
)

cir_jy <- readRDS(
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/cir_data_with_identifiers_v",
      ver,
      ".rds"
    )
  )
)

strsubinterventions <- readRDS(
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/sir_data_with_identifiers_v",
      ver,
      ".rds"
    )
  )
)


# Store original row counts -----------------------------------------------

journey_rows_before <- nrow(journey_output)
top_rows_before <- nrow(top)
cir_rows_before <- nrow(cir_jy)
sir_rows_before <- nrow(strsubinterventions)


# Confirm attrbdat exists before joining ----------------------------------

stopifnot(
  "attrbdat" %in% names(journey_output),
  "attrbdat" %in% names(top),
  "attrbdat" %in% names(cir_jy),
  "attrbdat" %in% names(strsubinterventions)
)


# Deidentify Journey ------------------------------------------------------

journey_output_deident <- journey_output %>%
  left_join(
    client_id_lookup,
    by = "attrbdat",
    relationship = "many-to-one"
  ) %>%
  select(
    -any_of(c(
      "attrbdat",
      "attrb"
    ))
  )


# Deidentify TOP ----------------------------------------------------------

top_deident <- top %>%
  left_join(
    client_id_lookup,
    by = "attrbdat",
    relationship = "many-to-one"
  ) %>%
  select(
    -any_of(c(
      "attrbdat",
      "attrb"
    ))
  )


# Deidentify CIR ----------------------------------------------------------

cir_jy_deident <- cir_jy %>%
  left_join(
    client_id_lookup,
    by = "attrbdat",
    relationship = "many-to-one"
  ) %>%
  select(
    -any_of(c(
      "attrbdat",
      "attrb"
    ))
  )


# Deidentify SIR ----------------------------------------------------------

strsubinterventions_deident <- strsubinterventions %>%
  left_join(
    client_id_lookup,
    by = "attrbdat",
    relationship = "many-to-one"
  ) %>%
  select(
    -any_of(c(
      "attrbdat",
      "attrb"
    ))
  )


# Row-count QA -------------------------------------------------------------

stopifnot(
  nrow(journey_output_deident) == journey_rows_before,
  nrow(top_deident) == top_rows_before,
  nrow(cir_jy_deident) == cir_rows_before,
  nrow(strsubinterventions_deident) == sir_rows_before
)

message(
  "Journey rows before/after deidentification: ",
  journey_rows_before,
  " / ",
  nrow(journey_output_deident)
)

message(
  "TOP rows before/after deidentification: ",
  top_rows_before,
  " / ",
  nrow(top_deident)
)

message(
  "CIR rows before/after deidentification: ",
  cir_rows_before,
  " / ",
  nrow(cir_jy_deident)
)

message(
  "SIR rows before/after deidentification: ",
  sir_rows_before,
  " / ",
  nrow(strsubinterventions_deident)
)


# Client ID QA -------------------------------------------------------------

message(
  "Journey records without client_random_id: ",
  sum(is.na(journey_output_deident$client_random_id))
)

message(
  "TOP records without client_random_id: ",
  sum(is.na(top_deident$client_random_id))
)

message(
  "CIR records without client_random_id: ",
  sum(is.na(cir_jy_deident$client_random_id))
)

message(
  "SIR records without client_random_id: ",
  sum(is.na(strsubinterventions_deident$client_random_id))
)

stopifnot(
  !any(is.na(journey_output_deident$client_random_id)),
  !any(is.na(top_deident$client_random_id)),
  !any(is.na(cir_jy_deident$client_random_id)),
  !any(is.na(strsubinterventions_deident$client_random_id))
)


# Direct-identifier removal QA --------------------------------------------

stopifnot(
  !any(
    c("attrbdat", "attrb") %in%
      names(journey_output_deident)
  ),
  
  !any(
    c("attrbdat", "attrb") %in%
      names(top_deident)
  ),
  
  !any(
    c("attrbdat", "attrb") %in%
      names(cir_jy_deident)
  ),
  
  !any(
    c("attrbdat", "attrb") %in%
      names(strsubinterventions_deident)
  )
)


# Required Journey field QA -----------------------------------------------

journey_required_fields <- c(
  "client_random_id",
  "n_jy",
  "age_at_journey_start",
  "age_first_use_drug1",
  "ethnicity",
  "employment_status",
  "parent",
  "dualdiag"
)

journey_field_check <- tibble(
  field = journey_required_fields,
  present = journey_required_fields %in%
    names(journey_output_deident)
)

print(journey_field_check)

stopifnot(
  all(journey_field_check$present)
)


# Required TOP field QA ---------------------------------------------------

top_required_fields <- c(
  "client_random_id",
  "topdate",
  "opiuse",
  "crause",
  "cocause",
  "alcuse",
  "ivdrguse",
  "acuthpbm"
)

top_field_check <- tibble(
  field = top_required_fields,
  present = top_required_fields %in%
    names(top_deident)
)

print(top_field_check)

stopifnot(
  all(top_field_check$present)
)


# Required CIR field QA ---------------------------------------------------

cir_required_fields <- c(
  "client_random_id",
  "n_jy",
  "assign_date",
  "housing",
  "homeless"
)

cir_field_check <- tibble(
  field = cir_required_fields,
  present = cir_required_fields %in%
    names(cir_jy_deident)
)

print(cir_field_check)

stopifnot(
  all(cir_field_check$present)
)


# Required SIR field QA ---------------------------------------------------

sir_required_fields <- c(
  "client_random_id",
  "n_jy",
  "submoddt",
  
  "is_lab",
  "is_oral_buprenorphine",
  "is_methadone",
  
  "is_comparator_ost",
  "is_diamorphine",
  "is_injectable_methadone",
  "is_other_ost",
  "is_any_ost",
  
  "s.ph.dose.meth_opi",
  "s.ph.dose.bupr_opi",
  "s.ph.superv",
  "s.ph.intent"
)

sir_field_check <- tibble(
  field = sir_required_fields,
  present = sir_required_fields %in%
    names(strsubinterventions_deident)
)

print(sir_field_check)

stopifnot(
  all(sir_field_check$present)
)


# Save timestamped deidentified extracts ----------------------------------
# These are the authoritative outputs for the current run.
# All four files must be loaded together using this same timestamp.

saveRDS(
  journey_output_deident,
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/journey_data_deidentified_v",
      ver,
      "_t",
      run_timestamp,
      ".rds"
    )
  )
)

saveRDS(
  top_deident,
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/top_data_deidentified_v",
      ver,
      "_t",
      run_timestamp,
      ".rds"
    )
  )
)

saveRDS(
  cir_jy_deident,
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/cir_data_deidentified_v",
      ver,
      "_t",
      run_timestamp,
      ".rds"
    )
  )
)

saveRDS(
  strsubinterventions_deident,
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/sir_data_deidentified_v",
      ver,
      "_t",
      run_timestamp,
      ".rds"
    )
  )
)


# Save latest-run timestamp pointer ---------------------------------------

# Script 07 reads this value and uses it to load all four extracts from the
# same deidentification run.
saveRDS(
  run_timestamp,
  file.path(
    getwd(),
    paste0(
      "data/interim/latest_deidentified_timestamp_v",
      ver,
      ".rds"
    )
  )
)

message(
  "Deidentified extracts saved successfully for timestamp: ",
  run_timestamp
)
