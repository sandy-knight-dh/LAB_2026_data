# -------------------------------------------------------------------------
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: 02_build_journey.R
#
# Purpose:
# Builds a cleaned, analysis-ready journey-level dataset for the LAB deep
# dive. Includes:
# - Filtering to relevant population (opiate users, in-period)
# - Merging lookups (geography, housing, discharge reasons)
# - Calculating time out of treatment
# - Preparing covariates for analysis
#
# Inputs:
# - Journey dataset data_dir.full
# - Lookup tables from shared drive
#
# Outputs:
# - outputs/extracts/journey_data_with_identifiers_v*.rds
# - data/interim/data_attrbdats_v*.rds (for reuse in other scripts)
# - data/interim/journey_filtered_v*.rds
#
# Dependencies:
# - 01_setup.R
# - Standard modules (length of stay, effective treatment)
#
# Notes:
# - This script outputs row-level PII data
# - Output files should not be shared without deidentification
# - Used as input for TOP, CIR, SIR, and modelling steps
#
# PII:
# - Contains identifiable client-level data (attrbdat, attrb)
# -------------------------------------------------------------------------
# Setup and load 
source("R/01_setup.R")

journey <- readRDS(
  paste0(
    data_dir.full,
    "/journey_",
    data_dir.trnc,
    ".rds"
  )
) %>%
  filter(nchar(dat) == 4)

journey_orig <- journey

# Initial filtering -------------------------------------------------------

# Filtering out-of-period records early for performance reasons

# Create list of relevant attrbdats
# We'll need this later for SIR/TOP/CIR
data_attrbdats <- journey %>% 
  filter(
    (disd_jy >= data_start | is.na(disd_jy)) &
      drug.grp_sml == 1
  ) %>%
  select(attrbdat) %>%
  unique()

journey <- journey %>% 
  semi_join(data_attrbdats, by = "attrbdat")

# This is including pre data_start journeys; not yet sure if they will be
# required or can be filtered here
# XXX Check

# Note that some non-opiate clients get OST - either misrecording of substances
# at assessment or developing an opiate problem during tx. Excluding them
# per spec.

# Also including non-opiate journeys for opiate clients to tell how
# long out of treatment they are - this is debatable depending on if the aim
# is to keep people out of treatment or specifically off opiates.
# XXX Check - have asked Sandy


# Modules -----------------------------------------------------------------

# Run any modules not applied to the all-journey file that are needed

source("Q:/R_standard_modules_periodic/modules/mod.length_of_stay.R")

source("Q:/R_standard_modules_periodic/modules/mod.effective_treatment.R")

## Journey-level housing variables are retained here and additional housing
# history measures (e.g. latest_housing, latest_homeless, ever_homeless and
# potentially initial_homeless) are derived later using CIR data.
#
# TOP acute housing problem variables may also be included to provide a
# longer-term indicator of housing instability.
#

# Factors and groups ------------------------------------------------------

# Set up factors for usability, add lookups etc.

# Discharge reason lookup, load and merge in
# Edited lookup to fix a text encoding problem
disrsn_lookup <- 
  fread("Q:/R_standard_modules_periodic/lookups/k.discharge_reasons.csv") %>% 
  select(disrsn_jy = disrsn, disrsn_jy_value = disrsn_value)

journey <- merge(journey,
                 disrsn_lookup,
                 by = c("disrsn_jy"),
                 all.x = TRUE)

# Add LA names and regions
# Also add final UTLA via calculated DAT lookup
# Note the "utla" column in the file is the submitted UTLA which can be wrong
# due to boundary changes. This won't get into the output.
# Geog lookup
geog_lookup <- fread("Q:/R_standard_modules_periodic/lookups/m.dat.csv",
                     header = TRUE) %>% # 1st row is header
  select(dat, dat.value, region, utla_final = gss)

# Match in lookup
# boundary changes
journey <- merge(journey,
                 geog_lookup,
                 by="dat",
                 all.x=TRUE)

# Add housing statuses
# housing lookup
housing_lookup <- fread("Q:/R_standard_modules_periodic/lookups/k.housing.csv",
                        header = TRUE) %>% # 1st row is header
  select(housing, housing_value, housing_grp_value
  )

# Match in lookup
journey <- merge(journey,
                 housing_lookup,
                 by="housing",
                 all.x=TRUE) 

# Clean up
rm(disrsn_lookup, geog_lookup, housing_lookup)
gc()


# Calculated fields ---------------------------------------------------------

# Length of time out of treatment is in the spec and isn't in modules, so
# calculate it here (triaged_jy_next is in the file but we might as
# well recalc here to find the following_time)
# Note that there is an edge case of overlapping journeys
# NDTMS journeys use modst_jy as the start date but triage might be preferred
# so give both

# Create a list of unique journeys with starts and ends
journeys <- journey %>% 
  select(attrbdat, n_jy, triaged_jy, modst_jy, disd_jy) %>% 
  unique() %>% 
  group_by(attrbdat) %>%
  arrange(n_jy) %>%
  # lag/lead are previous/next row in group
  mutate(disd_jy_previous = lag(disd_jy),
         triaged_jy_next = lead(triaged_jy),
         modst_jy_next = lead(modst_jy)) %>%
  ungroup() %>% 
  # -1 because e.g. leaving tx on the 1st and starting on the 2nd is 0
  # days without tx
  mutate(previous_time_out_of_tx_triage = triaged_jy - disd_jy_previous - 1,
         previous_time_out_of_tx_modst = modst_jy - disd_jy_previous - 1,
         following_time_out_of_tx_triage = triaged_jy_next - disd_jy - 1,
         following_time_out_of_tx_modst = modst_jy_next - disd_jy - 1) %>% 
  # Cut out unnecessary columns
  select(attrbdat, n_jy,
         disd_jy_previous, triaged_jy_next, modst_jy_next,
         previous_time_out_of_tx_triage, previous_time_out_of_tx_modst,
         following_time_out_of_tx_triage, following_time_out_of_tx_modst)

# merge back in to journey file (remove column that would be duplicated)
journey <- merge(journey %>%
                   select(-triaged_jy_next),
                 journeys,
                 by = c("attrbdat", "n_jy"),
                 all.x = TRUE)


# Because you need to have had 12 weeks/6 months in treatment to tell if you
# were retained in treatment that long, recent journeys aren't usable with
# these fields
# Add cohort flags for 12 week and 6 month retention, filter to these for 
# analysis
# Following standard NDTMS where we use 3 month delay for the 12 week cohort
journey <- journey %>%
  mutate(
    effective_tx_cohort = if_else(
      (triaged_jy %m+% months(3) <= period.end),
      1,
      0),
    effective_tx_6mo_cohort = if_else(
      (triaged_jy %m+% months(6) <= period.end),
      1,
      0)
  )

# Clean up
rm(journeys)
gc()

# Create month of birth and calculate age at journey start.
# This replaces the rolling age measure based on reporting year
# and provides a more accurate age for modelling. 
journey <- journey %>% 
  mutate(
    dob_month = as.Date(
      paste0(
        year(dob),
        "-",
        sprintf("%02d", month(dob)),
        "-01"
      )
    ),
    
    age_jy_start =
      floor(time_length(
        interval(dob_month, triaged_jy),
        "years"
      ))
  )

# Selecting journey fields ------------------------------------------------

# Requested fields

# Might be worth discussing other possible PSM matching variables - parent,
# ethnicity, employment, mental health...

# Keeping the original file for debug
journey_output <- journey %>%
  select(
    # Identifiers
    attrbdat,
    attrb,
    n_jy,
    agncy,
    
    # Demographics / potential matching covariates
    sex,
    ethnicity = ethnic,
    dob_month,
    age_at_journey_start = age_jy_start,
    age_first_use_drug1 = drug1age,
    
    # Socioeconomic / family characteristics
    employment_status = empstat,
    parent,
    
    # Mental health characteristics
    dualdiag,
    mthtn,
    crtmhn,
    crtmhn2,
    crtmhn3,
    
    # Substance group
    drug.grp_sml,
    
    # Specific drug use at assessment
    drug_a.crack,
    drug_a.benzo,
    
    # Geography
    utla_final,
    dat,
    dat.value,
    region,
    
    # Journey dates
    triaged_jy,
    modst_jy,
    disd_jy,
    
    # Episode dates
    triaged,
    disd,
    disd_ep,
    
    # Treatment history
    disd_jy_previous,
    triaged_jy_next,
    modst_jy_next,
    previous_time_out_of_tx_triage,
    previous_time_out_of_tx_modst,
    following_time_out_of_tx_triage,
    following_time_out_of_tx_modst,
    
    # Discharge reasons
    disrsn_ep,
    disrsn_jy,
    disrsn_jy_value,
    disrsn_jy.grp,
    
    # Interventions
    modal,
    modst,
    modend,
    
    # Length of stay
    starts_with("los"),
    
    # Effective treatment
    starts_with("effective_tx"),
    
    # Housing at assessment
    housing,
    housing_value,
    housing_grp_value
  )

# Journey output ----------------------------------------------------------
# THIS WILL OUTPUT PII
# RDS file is suitable here
# Save main dataset
saveRDS(
  journey_output,
  paste0("outputs/extracts/journey_data_with_identifiers_v", ver, ".rds")
)

# Save helper data
saveRDS(
  data_attrbdats,
  paste0("data/interim/data_attrbdats_v", ver, ".rds")
)

saveRDS(
  journey,
  paste0("data/interim/journey_filtered_v", ver, ".rds")
)

