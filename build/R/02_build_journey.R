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
# - data/interim/cir_jy_v*.rds (CIR housing rows from mod.cir.R, for 04)
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

# See below about non-England LAs - JC
# Addressed in version 6: non-England DATs are filtered out here.
# England only: DAT codes starting W, X, Y or Z are outside England and have
# no UTLA in the geography lookup (the reviewer's suggestion, and the same
# filter as modules/mod.scnr.R)
# The journey file is not loaded whole: journey_file is a lazy tidypolars
# query on its parquet copy (R/functions_inputs.R), and each step below reads
# only the columns and rows it needs (as_tibble() runs the query). Polars
# has substr() but not substring().
journey_file <- scan_input(
  paste0(
    data_dir.full,
    "/journey_",
    data_dir.trnc,
    ".rds"
  )
) %>%
  filter(
    nchar(dat) == 4,
    !substr(dat, 1, 1) %in% c("W", "X", "Y", "Z")
  )

# The columns of the journey file the build uses: the build's own (the
# select() into journey_output below, the time out of treatment, the
# lookups, 05's and 08's episode lookups from journey_filtered) and the
# modules' (mod.length_of_stay.R and mod.effective_treatment.R, and every
# field mod.cir.R takes with any_of(), so its cir_jy is unchanged). The
# other columns of the file are never read.
JOURNEY_COLUMNS <- c(
  # Journey structure and dates
  "attrb", "attrbdat", "dat", "n_jy", "agncy", "sex", "dob",
  "triaged", "disd", "disd_ep", "triaged_jy", "modst_jy", "disd_jy",
  "modal", "modst", "modend",
  # From the standard substance and discharge modules
  "drug.grp_sml", "drug_a.crack", "drug_a.benzo",
  "disrsn_ep", "disrsn_jy", "disrsn_jy.grp",
  # Assessment fields carried into the output
  "ethnic", "drug1age", "empstat", "parent", "dualdiag",
  "mthtn", "crtmhn", "crtmhn2", "crtmhn3", "housing", "homeless",
  # The rest of mod.cir.R's any_of() list
  "pregnant", "prnstat", "childwth", "ehcsc", "ehcsc2", "ehcsc3",
  "hepbstat", "hepcstat", "hepbvac", "hepctd", "hcvas", "hcvpcr",
  "hepctstd", "refhepctx", "hivstat", "hivtestdt", "livscrn", "naloxiss",
  "naloxad", "domvic", "domper", "ypcpl", "ypsstich", "hlcassdt"
)

# Initial filtering -------------------------------------------------------

# Filtering out-of-period records early for performance reasons

# This includes lots of other journey data - should have a
# select(attrbdat) %>% unique()
# It's used later for to join on n_jy at ep level, I suggest that should be
# a different object from select(attrbdat, triaged, agncy, n_jy) %>%
# unique() - JC
# Addressed: data_attrbdats is the distinct attrbdats, and 05 builds
# its own episode lookup (journey_lookup) from journey_filtered.
# Create list of relevant attrbdats
# We'll need this later for SIR/TOP/CIR
data_attrbdats <- journey_file %>%
  select(attrbdat, disd_jy, drug.grp_sml) %>%
  filter(
    (disd_jy >= data_start | is.na(disd_jy)) &
      drug.grp_sml == 1
  ) %>%
  distinct(attrbdat) %>%
  as_tibble()

# Treatment history across local authorities ------------------------------
# client_random_id is created per attrbdat, so the dataset can't see a
# client's journeys in other local authorities. While the full journey file
# is loaded, find each journey's previous and next journey among all of the
# client's (attrb) journeys in England: journeys_any_la() in
# R/functions_cross_la.R, the lag/lead code under "Calculated fields" below
# grouped by attrb.

# Clients and people are kept with filter(... %in% ...) rather than
# semi_join(): polars applies a filter while it reads the file, so the rows
# of other clients are never read, and the file's row order is kept
our_attrbs <- journey_file %>%
  select(attrbdat, attrb) %>%
  filter(attrbdat %in% data_attrbdats$attrbdat) %>%
  distinct(attrb) %>%
  as_tibble()

all_journeys <- journey_file %>%
  select(attrbdat, attrb, dat, n_jy, triaged_jy, disd_jy) %>%
  filter(attrb %in% our_attrbs$attrb) %>%
  distinct() %>%
  as_tibble()

cross_la <- journeys_any_la(all_journeys) %>%
  semi_join(
    all_journeys %>% semi_join(data_attrbdats, by = "attrbdat"),
    by = c("attrb", "dat", "n_jy")
  )

message(
  "Journeys of our clients: ", nrow(cross_la),
  "; all England journeys of the same people: ", nrow(all_journeys)
)

rm(our_attrbs, all_journeys)
gc()

# Our clients' journeys, the columns the build uses, as the plain data
# frame the standard modules expect
journey <- journey_file %>%
  filter(attrbdat %in% data_attrbdats$attrbdat) %>%
  select(any_of(JOURNEY_COLUMNS)) %>%
  as_tibble() %>%
  as.data.frame()
rm(journey_file)
gc()

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

source(file.path(std_modules, "modules/mod.length_of_stay.R"))

source(file.path(std_modules, "modules/mod.effective_treatment.R"))

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
  fread(file.path(std_modules, "lookups/k.discharge_reasons.csv")) %>% 
  select(disrsn_jy = disrsn, disrsn_jy_value = disrsn_value)

journey <- merge(journey,
                 disrsn_lookup,
                 by = c("disrsn_jy"),
                 all.x = TRUE)

# This lookup only covers England and non-England clients haven't been
# removed, so it leaves some NA UTLAs.
# I suggest filter(!substring(dat,1,1) %in% c('W','X','Y','Z')) when
# loading journey to just filter them out, unless Sandy wants them
# included. - JC
# Addressed in version 6: filtered when the journey file is loaded.
# Add LA names and regions
# Also add final UTLA via calculated DAT lookup
# Note the "utla" column in the file is the submitted UTLA which can be wrong
# due to boundary changes. This won't get into the output.
# Geog lookup
geog_lookup <- fread(file.path(std_modules, "lookups/m.dat.csv"),
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
housing_lookup <- fread(file.path(std_modules, "lookups/k.housing.csv"),
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

# Cross-local-authority history ------------------------------------------

journey <- merge(journey,
                 cross_la,
                 by = c("attrb", "dat", "n_jy"),
                 all.x = TRUE)

rm(cross_la)
gc()


# CIR modules ---------------------------------------------------------------
# mod.cir.R appends CIR reviews to the information recorded at each
# episode's assessment and assigns them to journeys (cir_jy). The CIR
# modules then derive journey-level housing (cir.housing.R: hou.*) and
# mental health (cir.mental_health.R: mh.*). These are the latest values in
# the journey, so they describe the journey rather than its start.
#
# cir.mental_health.R recodes the episode-level dualdiag (Y/N) to 0/1/2;
# the raw value is kept so that dualdiag in the output is as recorded.

journey <- journey %>%
  mutate(dualdiag_raw = dualdiag)

source(file.path(std_modules, "modules/mod.cir.R"))
source(file.path(std_modules, "modules/cir/cir.housing.R"))
source(file.path(std_modules, "modules/cir/cir.mental_health.R"))

journey <- journey %>%
  mutate(dualdiag = dualdiag_raw) %>%
  select(-dualdiag_raw)

# The housing rows of cir_jy are what 04_build_cir.R summarises
saveRDS(
  cir_jy %>%
    select(attrb, dat, n_jy, assign_date, any_of("cirdt"),
           housing, homeless),
  paste0("data/interim/cir_jy_v", ver, ".rds")
)

rm(cir_jy)
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

# merge back in to journey file (the file's own triaged_jy_next, which this
# replaces, is not among the columns read)
journey <- merge(journey,
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

# This comment is scrambled - JC
# Addressed: the comment below has been rewritten since.
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
    age_jy_start,
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
    
    # Treatment history (within the local authority, as in version 5)
    disd_jy_previous,
    triaged_jy_next,
    modst_jy_next,
    previous_time_out_of_tx_triage,
    previous_time_out_of_tx_modst,
    following_time_out_of_tx_triage,
    following_time_out_of_tx_modst,
    
    # Treatment history in any English local authority (journeys_any_la())
    n_prev_journeys_any_la,
    disd_jy_previous_any_la,
    triaged_jy_next_any_la,
    next_dat_same,
    previous_time_out_of_tx_triage_any_la,
    following_time_out_of_tx_triage_any_la,
    
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
    housing_grp_value,
    
    # Latest housing and mental health in the journey (CIR modules)
    any_of(c(
      "hou.housing_curr",
      "hou.housing.grp_curr",
      "hou.homeless_curr",
      "mh.mthtn_jy",
      "mh.dualdiag_jy",
      "mh.crtmhn.any",
      "mh.crtmhn.none"
    ))
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

