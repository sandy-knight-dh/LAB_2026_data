# -------------------------------------------------------------------------
# Script: 07_link_dataset.R
#
# Purpose:
# Creates the final deidentified LAB analysis dataset by linking journey,
# TOP, CIR and SIR extracts.
#
# Final unit:
# - One row per client treatment journey.
#
# Key functions:
# - Load deidentified journey, TOP, CIR and SIR extracts
# - Derive TOP current-state variables at client level
# - Derive CIR housing variables at client level
# - Derive SIR treatment exposure variables at journey level
# - Add journey history variables for modelling/PSM
# - Add cohort, representation and follow-up flags
# - Save final linked dataset and a disclosure-controlled sample
#
# Inputs:
# - outputs/extracts/journey_data_deidentified_v{ver}.rds
# - outputs/extracts/top_data_deidentified_v{ver}.rds
# - outputs/extracts/cir_data_deidentified_v{ver}.rds
# - outputs/extracts/sir_data_deidentified_v{ver}.rds
#
# Outputs:
# - outputs/extracts/final_linked_dataset_v{ver}.rds
# - outputs/extracts/final_dataset_sample_no_pid_v{ver}.xlsx
#
# Notes:
# - The main final dataset is left unfiltered so Sandy/analysts can apply
#   cohort restrictions as required.
# - lab_analysis_cohort_2022 can be used to restrict to journeys starting
#   on or after 01-Jan-2022.
# - TOP and CIR housing are treated as client current-state information.
# - SIR is used for treatment exposure and is summarised at journey level
#   using client_random_id and n_jy.
# - SIR records without n_jy are excluded from journey-level exposure
#   summaries because they cannot be assigned to a journey.
# - dob_month and rolling age are removed from the final output. The modelling
#   age variable is age_jy_start.
#
# PII:
# - Inputs are pseudonymised but remain sensitive.
# - Final outputs should still be handled according to project IG controls.

# Setup -------------------------------------------------------------------

source("R/01_setup.R")
library(tidyverse)
library(lubridate)
library(writexl)

# Load deidentified extracts 
run_timestamp <- readRDS(
  file.path(
    getwd(),
    paste0(
      "data/interim/latest_deidentified_timestamp_v",
      ver,
      ".rds"
    )
  )
)

# Journey
journey <- readRDS(
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

# TOP
top <- readRDS(
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

# CIR
cir_jy <- readRDS(
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

# SIR
sir <- readRDS(
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

# TOP journey linkage ------------------------------------------------------
# TOP records do not contain n_jy, so attach each client's possible journeys
# and retain TOP records within the agreed validity window:
# 14 days before modality start to 14 days after discharge/period end.


# Create journey lookup ----------------------------------------------------

journey_top_lookup <- journey %>%
  select(
    client_random_id,
    n_jy,
    triaged_jy,
    modst_jy,
    disd_jy
  ) %>%
  distinct()

# Link TOP records to possible journeys -----------------------------------

top_linked <- top %>%
  left_join(
    journey_top_lookup,
    by = "client_random_id",
    relationship = "many-to-many"
  ) %>%
  
  # Remove candidate journeys without a usable modality start date
  filter(
    !is.na(modst_jy)
  ) %>%
  
  # Apply agreed +/-14-day TOP validity window
  filter(
    !is.na(topdate),
    
    topdate >=
      modst_jy %m-% days(14),
    
    topdate <=
      pmin(
        disd_jy,
        period.end,
        na.rm = TRUE
      ) %m+% days(14)
  )

#qa
nrow(top)
nrow(top_linked)

# Journey lookup no longer required
rm(journey_top_lookup)
gc()

# Collapse duplicate TOPs on the same date --------------------------------
# Produce one observation per person, journey and TOP date.

top_by_date <- top_linked %>%
  group_by(
    client_random_id,
    n_jy,
    topdate
  ) %>%
  summarise(
    opiuse = safe_max_numeric(opiuse),
    crause = safe_max_numeric(crause),
    cocause = safe_max_numeric(cocause),
    alcuse = safe_max_numeric(alcuse),
    ivdrguse = safe_max_numeric(ivdrguse),
    acuthpbm = safe_max_numeric(acuthpbm),
    
    .groups = "drop"
  )


# First TOP within journey -------------------------------------------------

top_first <- top_by_date %>%
  arrange(
    client_random_id,
    n_jy,
    topdate
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(
    client_random_id,
    n_jy,
    
    first_top_date = topdate,
    
    # Sandy-requested first TOP measures
    first_opiuse = opiuse,
    first_crause = crause,
    first_ivdrguse = ivdrguse,
    
    first_acute_housing_problem = acuthpbm
  )


# Latest TOP within journey ------------------------------------------------

top_latest <- top_by_date %>%
  arrange(
    client_random_id,
    n_jy,
    topdate
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(
    client_random_id,
    n_jy,
    
    latest_top_date = topdate,
    
    # Sandy-requested latest TOP measures
    latest_opiuse = opiuse,
    latest_crause = crause,
    
    # Retained as additional useful measure
    latest_ivdrguse = ivdrguse,
    
    latest_acute_housing_problem = acuthpbm
  )


# Journey-level TOP summary ------------------------------------------------

top_summary <- top_by_date %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    # Number of distinct TOP dates linked to journey
    number_top_records = n(),
    
    # Any recorded behaviour during journey
    ever_injecting = as.integer(
      any(ivdrguse > 0, na.rm = TRUE)
    ),
    
    ever_opiate_use = as.integer(
      any(opiuse > 0, na.rm = TRUE)
    ),
    
    ever_crack_use = as.integer(
      any(crause > 0, na.rm = TRUE)
    ),
    
    ever_cocaine_use = as.integer(
      any(cocause > 0, na.rm = TRUE)
    ),
    
    ever_alcohol_use = as.integer(
      any(alcuse > 0, na.rm = TRUE)
    ),
    
    ever_acute_housing_problem = as.integer(
      any(acuthpbm == 1, na.rm = TRUE)
    ),
    
    .groups = "drop"
  ) %>%
  
  left_join(
    top_first,
    by = c(
      "client_random_id",
      "n_jy"
    ),
    relationship = "one-to-one"
  ) %>%
  
  left_join(
    top_latest,
    by = c(
      "client_random_id",
      "n_jy"
    ),
    relationship = "one-to-one"
  )


# TOP summary QA -----------------------------------------------------------

# Confirm exactly one summary row per journey
top_duplicate_check <- top_summary %>%
  count(
    client_random_id,
    n_jy,
    name = "n_records"
  ) %>%
  filter(
    n_records > 1
  )

nrow(top_duplicate_check)

# Confirm first TOP date is never after latest TOP date
top_date_check <- top_summary %>%
  filter(
    !is.na(first_top_date),
    !is.na(latest_top_date),
    first_top_date > latest_top_date
  )
nrow(top_date_check)

# check Sandy-requested TOP variables --------------------

top_completeness <- top_summary %>%
  summarise(
    journeys_with_top = n(),
    
    missing_first_opiuse =
      sum(is.na(first_opiuse)),
    
    missing_first_crause =
      sum(is.na(first_crause)),
    
    missing_first_ivdrguse =
      sum(is.na(first_ivdrguse)),
    
    missing_latest_opiuse =
      sum(is.na(latest_opiuse)),
    
    missing_latest_crause =
      sum(is.na(latest_crause)),
    
    missing_first_acute_housing_problem =
      sum(is.na(first_acute_housing_problem)),
    
    missing_latest_acute_housing_problem =
      sum(is.na(latest_acute_housing_problem))
  )

print(top_completeness)

# CIR housing summary ------------------------------------------------------
# CIR and journey housing observations have already been assigned to n_jy
# in 04_build_cir.R.
#
# Summarise housing within each treatment journey using assign_date.
# This provides housing/homelessness at the first and latest available
# observations, plus whether homelessness was recorded at any point.
cir_summary <- cir_jy %>%
  arrange(
    client_random_id,
    n_jy,
    assign_date
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    
    # Number of housing observations within journey
    number_cir_records = n(),
    
    # First available housing observation date
    initial_cir_date = first(assign_date),
    
    # Housing/homelessness at first available observation
    initial_housing = first(housing),
    initial_homeless = first(homeless),
    
    # Latest available housing observation date
    latest_cir_date = last(assign_date),
    
    # Housing/homelessness at latest available observation
    latest_housing = last(housing),
    latest_homeless = last(homeless),
    
    # Whether homelessness was recorded at any point during the journey
    ever_homeless = as.integer(
      any(homeless == "Y", na.rm = TRUE)
    ),
    
    .groups = "drop"
  )

# CIR summary QA -----------------------------------------------------------

# Confirm one summary row per client/journey
cir_duplicate_check <- cir_summary %>%
  count(
    client_random_id,
    n_jy,
    name = "n_records"
  ) %>%
  filter(n_records > 1)
nrow(cir_duplicate_check)

# Confirm initial observation is never later than latest observation
cir_date_check <- cir_summary %>%
  filter(
    !is.na(initial_cir_date),
    !is.na(latest_cir_date),
    initial_cir_date > latest_cir_date
  )
nrow(cir_date_check)

# Review completeness of housing measures
cir_completeness <- cir_summary %>%
  summarise(
    journeys_with_housing = n(),
    
    missing_initial_housing =
      sum(is.na(initial_housing)),
    
    missing_initial_homeless =
      sum(is.na(initial_homeless)),
    
    missing_latest_housing =
      sum(is.na(latest_housing)),
    
    missing_latest_homeless =
      sum(is.na(latest_homeless))
  )

print(cir_completeness)

# SIR treatment exposure summary ------------------------------------------
# SIR records without n_jy cannot be assigned to a journey and are excluded
# from journey-level summaries.

sir_linked <- sir %>%
  filter(
    !is.na(client_random_id),
    !is.na(n_jy)
  )


# Helper functions ---------------------------------------------------------

# Return the earliest date, handling groups where all dates are missing.
safe_min_date <- function(x) {
  if (all(is.na(x))) {
    as.Date(NA)
  } else {
    min(x, na.rm = TRUE)
  }
}

# Return a numeric value only where there is one unique non-missing value.
# Conflicting values on the first OST date are returned as NA.
single_numeric_value <- function(x) {
  values <- unique(x[!is.na(x)])
  
  if (length(values) == 1) {
    as.numeric(values)
  } else {
    NA_real_
  }
}

# Equivalent helper for character fields such as prescribing intention.
single_character_value <- function(x) {
  values <- unique(as.character(x[!is.na(x)]))
  
  if (length(values) == 1) {
    values
  } else {
    NA_character_
  }
}


# Basic journey-level SIR summary -----------------------------------------

sir_basic_summary <- sir_linked %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    # SIR availability
    has_sir = 1,
    number_sir_records = n(),
    
    # First SIR of any type within journey
    first_sir_date = safe_min_date(submoddt),
    
    # Number of SIR records containing one of the three comparator OSTs
    ost_sir_count = sum(
      is_comparator_ost == 1,
      na.rm = TRUE
    ),
    
    # Ever-exposure indicators
    ever_lab = as.integer(
      any(is_lab == 1, na.rm = TRUE)
    ),
    
    ever_methadone = as.integer(
      any(is_methadone == 1, na.rm = TRUE)
    ),
    
    ever_oral_buprenorphine = as.integer(
      any(is_oral_buprenorphine == 1, na.rm = TRUE)
    ),
    
    ever_comparator_ost = as.integer(
      any(is_comparator_ost == 1, na.rm = TRUE)
    ),
    
    ever_other_injectable_ost = as.integer(
      any(is_other_ost == 1, na.rm = TRUE)
    ),
    
    ever_any_ost = as.integer(
      any(is_any_ost == 1, na.rm = TRUE)
    ),
    
    ever_diamorphine = as.integer(
      any(is_diamorphine == 1, na.rm = TRUE)
    ),
    
    ever_injectable_methadone = as.integer(
      any(is_injectable_methadone == 1, na.rm = TRUE)
    ),
    
    # Record counts
    lab_record_count = sum(
      is_lab == 1,
      na.rm = TRUE
    ),
    
    methadone_record_count = sum(
      is_methadone == 1,
      na.rm = TRUE
    ),
    
    oral_buprenorphine_record_count = sum(
      is_oral_buprenorphine == 1,
      na.rm = TRUE
    ),
    
    other_injectable_ost_record_count = sum(
      is_other_ost == 1,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# First and last LAB SIR recording dates ----------------------------------

lab_dates <- sir_linked %>%
  filter(
    is_lab == 1,
    !is.na(submoddt)
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    first_lab_date = min(submoddt),
    last_lab_date = max(submoddt),
    .groups = "drop"
  )


# First and last methadone SIR recording dates ----------------------------

methadone_dates <- sir_linked %>%
  filter(
    is_methadone == 1,
    !is.na(submoddt)
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    first_methadone_date = min(submoddt),
    last_methadone_date = max(submoddt),
    .groups = "drop"
  )


# First and last oral buprenorphine SIR recording dates -------------------

oral_buprenorphine_dates <- sir_linked %>%
  filter(
    is_oral_buprenorphine == 1,
    !is.na(submoddt)
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    first_oral_buprenorphine_date = min(submoddt),
    last_oral_buprenorphine_date = max(submoddt),
    .groups = "drop"
  )


# Identify first comparator OST SIR date ----------------------------------

first_ost_dates <- sir_linked %>%
  filter(
    is_comparator_ost == 1,
    !is.na(submoddt)
  ) %>%
  group_by(
    client_random_id,
    n_jy
  ) %>%
  summarise(
    first_ost_sir_date = min(submoddt),
    .groups = "drop"
  )


# Retain all comparator OST records on the first OST date -----------------

first_ost_records <- sir_linked %>%
  inner_join(
    first_ost_dates,
    by = c(
      "client_random_id",
      "n_jy"
    ),
    relationship = "many-to-one"
  ) %>%
  filter(
    submoddt == first_ost_sir_date,
    is_comparator_ost == 1
  )


# QA: multiple records on first OST date ----------------------------------

first_ost_same_date_qa <- first_ost_records %>%
  count(
    client_random_id,
    n_jy,
    first_ost_sir_date,
    name = "number_first_date_records"
  ) %>%
  filter(
    number_first_date_records > 1
  )

message(
  "Journeys with multiple comparator OST records on first OST date: ",
  nrow(first_ost_same_date_qa)
)


# Summarise first OST date information ------------------------------------

first_ost_summary <- first_ost_records %>%
  group_by(
    client_random_id,
    n_jy,
    first_ost_sir_date
  ) %>%
  summarise(
    # Treatment type recorded on first comparator OST date
    first_ost_sir_methadone = as.integer(
      any(is_methadone == 1, na.rm = TRUE)
    ),
    
    first_ost_sir_oral_buprenorphine = as.integer(
      any(is_oral_buprenorphine == 1, na.rm = TRUE)
    ),
    
    first_ost_sir_lab = as.integer(
      any(is_lab == 1, na.rm = TRUE)
    ),
    
    # Dose recorded on first OST date
    first_ost_sir_methadone_dose =
      single_numeric_value(s.ph.dose.meth_opi),
    
    first_ost_sir_buprenorphine_dose =
      single_numeric_value(s.ph.dose.bupr_opi),
    
    # s.ph.superv:
    # 1 = supervised
    # 2 = unsupervised
    # Output:
    # 1 = supervised
    # 0 = unsupervised
    # NA = absent or conflicting
    first_ost_sir_supervised = case_when(
      any(s.ph.superv == 1, na.rm = TRUE) &
        !any(s.ph.superv == 2, na.rm = TRUE) ~ 1,
      
      any(s.ph.superv == 2, na.rm = TRUE) &
        !any(s.ph.superv == 1, na.rm = TRUE) ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    # Prescribing intention on first OST date
    first_ost_sir_intention =
      single_character_value(s.ph.intent),
    
    # Same-date conflict flags
    first_ost_methadone_dose_conflict = as.integer(
      n_distinct(
        s.ph.dose.meth_opi,
        na.rm = TRUE
      ) > 1
    ),
    
    first_ost_buprenorphine_dose_conflict = as.integer(
      n_distinct(
        s.ph.dose.bupr_opi,
        na.rm = TRUE
      ) > 1
    ),
    
    first_ost_supervision_conflict = as.integer(
      n_distinct(
        s.ph.superv,
        na.rm = TRUE
      ) > 1
    ),
    
    first_ost_intention_conflict = as.integer(
      n_distinct(
        s.ph.intent,
        na.rm = TRUE
      ) > 1
    ),
    
    number_first_ost_date_records = n(),
    
    .groups = "drop"
  )


# Combine all journey-level SIR summaries ---------------------------------

sir_summary <- sir_basic_summary %>%
  left_join(
    first_ost_summary,
    by = c("client_random_id", "n_jy"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    lab_dates,
    by = c("client_random_id", "n_jy"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    methadone_dates,
    by = c("client_random_id", "n_jy"),
    relationship = "one-to-one"
  ) %>%
  left_join(
    oral_buprenorphine_dates,
    by = c("client_random_id", "n_jy"),
    relationship = "one-to-one"
  )


# SIR summary QA -----------------------------------------------------------

# Confirm one row per client/journey
sir_duplicate_check <- sir_summary %>%
  count(
    client_random_id,
    n_jy,
    name = "n_records"
  ) %>%
  filter(
    n_records > 1
  )

message(
  "Duplicate client/journey keys in SIR summary: ",
  nrow(sir_duplicate_check)
)

stopifnot(
  nrow(sir_duplicate_check) == 0
)


# First comparator OST date cannot precede first SIR date
sir_date_check <- sir_summary %>%
  filter(
    !is.na(first_sir_date),
    !is.na(first_ost_sir_date),
    first_ost_sir_date < first_sir_date
  )

message(
  "Journeys where first OST SIR predates first SIR: ",
  nrow(sir_date_check)
)

stopifnot(
  nrow(sir_date_check) == 0
)


# Comparator OST records should have a first comparator OST date
sir_ost_date_check <- sir_summary %>%
  filter(
    ost_sir_count > 0,
    is.na(first_ost_sir_date)
  )

message(
  "Journeys with comparator OST records but no first OST date: ",
  nrow(sir_ost_date_check)
)

stopifnot(
  nrow(sir_ost_date_check) == 0
)


# First medication-recording date cannot be after last recording date
sir_treatment_date_check <- sir_summary %>%
  filter(
    (
      !is.na(first_lab_date) &
        !is.na(last_lab_date) &
        first_lab_date > last_lab_date
    ) |
      (
        !is.na(first_methadone_date) &
          !is.na(last_methadone_date) &
          first_methadone_date > last_methadone_date
      ) |
      (
        !is.na(first_oral_buprenorphine_date) &
          !is.na(last_oral_buprenorphine_date) &
          first_oral_buprenorphine_date >
          last_oral_buprenorphine_date
      )
  )

message(
  "Journeys with invalid first/last treatment-recording dates: ",
  nrow(sir_treatment_date_check)
)

stopifnot(
  nrow(sir_treatment_date_check) == 0
)


# Review first-OST conflicts ----------------------------------------------

sir_conflict_summary <- sir_summary %>%
  summarise(
    journeys_with_sir = n(),
    
    journeys_with_first_ost =
      sum(!is.na(first_ost_sir_date)),
    
    journeys_with_multiple_first_date_records =
      sum(
        number_first_ost_date_records > 1,
        na.rm = TRUE
      ),
    
    methadone_dose_conflicts =
      sum(
        first_ost_methadone_dose_conflict == 1,
        na.rm = TRUE
      ),
    
    buprenorphine_dose_conflicts =
      sum(
        first_ost_buprenorphine_dose_conflict == 1,
        na.rm = TRUE
      ),
    
    supervision_conflicts =
      sum(
        first_ost_supervision_conflict == 1,
        na.rm = TRUE
      ),
    
    intention_conflicts =
      sum(
        first_ost_intention_conflict == 1,
        na.rm = TRUE
      )
  )

print(sir_conflict_summary)


# Check other injectable OST prevalence -----------------------------------

other_ost_prevalence <- sir_summary %>%
  summarise(
    journeys_with_any_ost =
      sum(ever_any_ost == 1, na.rm = TRUE),
    
    journeys_with_other_injectable_ost =
      sum(
        ever_other_injectable_ost == 1,
        na.rm = TRUE
      ),
    
    pct_other_injectable_ost = if_else(
      journeys_with_any_ost > 0,
      round(
        journeys_with_other_injectable_ost /
          journeys_with_any_ost * 100,
        2
      ),
      NA_real_
    )
  )

print(other_ost_prevalence)

# Link journey-level summaries --------------------------------------------

journey_rows_before_linkage <- nrow(journey)

final_dataset <- journey %>%
  left_join(
    top_summary,
    by = c("client_random_id", "n_jy"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    cir_summary,
    by = c("client_random_id", "n_jy"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    sir_summary,
    by = c("client_random_id", "n_jy"),
    relationship = "many-to-one"
  )

message(
  "Journey rows before linkage: ",
  journey_rows_before_linkage
)

message(
  "Rows after TOP/CIR/SIR linkage: ",
  nrow(final_dataset)
)

stopifnot(
  nrow(final_dataset) == journey_rows_before_linkage
)

journey_rows_before_linkage <- nrow(journey)


# Replace missing flags and counts -----------------------------------------

final_dataset <- final_dataset %>%
  mutate(
    across(
      any_of(c(
        # TOP flags
        "ever_injecting",
        "ever_opiate_use",
        "ever_crack_use",
        "ever_cocaine_use",
        "ever_alcohol_use",
        "ever_acute_housing_problem",
        
        # SIR flags
        "ever_lab",
        "ever_methadone",
        "ever_oral_buprenorphine",
        "ever_comparator_ost",
        "ever_other_injectable_ost",
        "ever_any_ost",
        "ever_diamorphine",
        "ever_injectable_methadone"
      )),
      ~ replace_na(.x, 0)
    ),
    
    has_sir =
      replace_na(has_sir, 0),
    
    number_top_records =
      replace_na(number_top_records, 0),
    
    number_cir_records =
      replace_na(number_cir_records, 0),
    
    number_sir_records =
      replace_na(number_sir_records, 0),
    
    ost_sir_count =
      replace_na(ost_sir_count, 0),
    
    lab_record_count =
      replace_na(lab_record_count, 0),
    
    methadone_record_count =
      replace_na(methadone_record_count, 0),
    
    oral_buprenorphine_record_count =
      replace_na(oral_buprenorphine_record_count, 0),
    
    other_injectable_ost_record_count =
      replace_na(other_injectable_ost_record_count, 0)
  )

#final datasetsaved--
# The dataset retains the underlying Journey modality/episode rows.
# Therefore, multiple rows may occur for the same client_random_id + n_jy.
saveRDS(
  final_dataset,
  file.path(
    getwd(),
    paste0(
      "outputs/extracts/LAB_full_linked_dataset_v",
      ver,
      "_t",
      run_timestamp,
      ".rds"
    )
  )
)


# Final dataset QA ---------------------------------------------------------
nrow(final_dataset)
n_distinct(final_dataset$client_random_id)
sum(is.na(final_dataset$client_random_id))
sum(is.na(final_dataset$n_jy))

# Check Sandy-requested variables are present
final_required_fields <- c(
  # Covariates
  "sex",
  "age_at_journey_start",
  "age_first_use_drug1",
  "ethnicity",
  "employment_status",
  "parent",
  "dualdiag",
  
  # TOP
  "first_top_date",
  "first_opiuse",
  "first_crause",
  "first_ivdrguse",
  "latest_top_date",
  "latest_opiuse",
  "latest_crause",
  
  # Housing
  "initial_housing",
  "initial_homeless",
  "latest_housing",
  "latest_homeless",
  "ever_homeless",
  
  # SIR
  "first_sir_date",
  "ost_sir_count",
  "first_ost_sir_date",
  "first_ost_sir_methadone",
  "first_ost_sir_oral_buprenorphine",
  "first_ost_sir_lab",
  "first_ost_sir_methadone_dose",
  "first_ost_sir_buprenorphine_dose",
  "first_ost_sir_supervised",
  "first_ost_sir_intention",
  
  # Medication recording dates
  "first_lab_date",
  "last_lab_date",
  "first_methadone_date",
  "last_methadone_date",
  "first_oral_buprenorphine_date",
  "last_oral_buprenorphine_date"
)

final_field_check <- tibble(
  field = final_required_fields,
  present = final_required_fields %in%
    names(final_dataset)
)

print(final_field_check)

stopifnot(
  all(final_field_check$present)
)


final_key_check <- final_dataset %>%
  count(
    client_random_id,
    n_jy
  ) %>%
  filter(n > 1)
nrow(final_key_check)

