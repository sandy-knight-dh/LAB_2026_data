# -------------------------------------------------------------------------
# Script: 05_build_sir.R
#
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: 05_build_sir.R
#
# Purpose:
# Builds the row-level SIR dataset for the LAB analysis.
#
# The output retains the fields needed to derive:
# - LAB, oral buprenorphine and methadone exposure
# - First OST SIR date and treatment type
# - Dose, supervision and prescribing intention at first OST SIR
# - First and last treatment-recording dates
#
# Journey-level summaries are created later in 07_link_dataset.R.
#
# Inputs:
# - z_verified_submodality_str.rds
# - data/interim/data_attrbdats_v{ver}.rds
# - data/interim/journey_filtered_v{ver}.rds
#
# Output:
# - outputs/extracts/sir_data_with_identifiers_v{ver}.rds
#
# PII:
# - Contains identifiable client-level data, including attrbdat.
# Setup -------------------------------------------------------------------

source("R/01_setup.R")

# I think I previously mentioned that this has loads of extraneous
# columns. Some of these are actually used here. Maybe needs a new name as
# well as trimming if agncy etc. are to be used. - JC
# Addressed: data_attrbdats is attrbdat only, and the episode lookup is a
# separate object (journey_lookup).
#unique attrbdat
unique_attrbdat <- readRDS(
  paste0(
    "data/interim/data_attrbdats_v",
    ver,
    ".rds"
  )
) %>%
  select(attrbdat) %>%
  distinct()

#adding in journey look up
journey_lookup <- readRDS(
  paste0(
    "data/interim/journey_filtered_v",
    ver,
    ".rds"
  )
) %>%
  select(
    attrbdat,
    agncy,
    triaged,
    disd_ep,
    n_jy
  ) %>%
  distinct()


# Check whether linkage key maps to multiple journeys.
journey_lookup_duplicates <- journey_lookup %>%
  count(
    attrbdat,
    agncy,
    triaged,
    name = "n_matches"
  ) %>%
  filter(n_matches > 1)

message(
  "Journey lookup keys with multiple matches: ",
  nrow(journey_lookup_duplicates)
)

# SIR data ----------------------------------------------------------------
strsubinterventions <- readRDS(
  paste0(
    data_dir.full,
    "/Z-verified files/z_verified_submodality_str.rds"
  )
) %>%
  select(
    attrbdat,
    agncy,
    triaged,
    submoddt,
    
    # Pharmacological, psychosocial and young-person structured interventions
    starts_with(c(
      "s.ph.",
      "s.psy.",
      "s.yp."
    ))
  )

# This comment contradicts the previous one. This filtering has been
# adopted for monthly but not annual data. - JC
# Addressed: the comment below has been rewritten since.
# Apply monthly period rule ------------------------------------------------

# This is a monthly extract, so SIRs after the submission month end are
# excluded in line with the monthly implementation of the standard module.
#
# This rule differs from some annual processing, where later SIR records
# may be retained.
if (run_type == "monthly") {
  strsubinterventions <- strsubinterventions %>%
    filter(
      submoddt <= period.end
    )
}

#apply attrbdat
strsubinterventions <- strsubinterventions %>%
  semi_join(
    unique_attrbdat,
    by = "attrbdat"
  ) %>%
  distinct()



# Assign journey number ----------------------------------------------------

strsubinterventions <- strsubinterventions %>%
  left_join(
    journey_lookup,
    by = c(
      "attrbdat",
      "agncy",
      "triaged"
    ),
    relationship = "many-to-one"
  )

# Journey-assignment QA ----------------------------------------------------

message(
  "SIR records without assigned n_jy: ",
  sum(is.na(strsubinterventions$n_jy))
)

message(
  "Percentage of SIR records without assigned n_jy: ",
  round(
    mean(is.na(strsubinterventions$n_jy)) * 100,
    2
  ),
  "%"
)

# Convenience fields ------------------------------------------------------

if ("s.ph.buprinj_opi" %in% names(strsubinterventions)) {
  strsubinterventions <- strsubinterventions %>%
    mutate(# Depot buprenorphine
      s.ph.buprinj_any = as.numeric(s.ph.buprinj_opi_main == 1 |
                                      s.ph.buprinj_opi_with == 1 |
                                      s.ph.buprinj_opi == 1),
      # All the old and new non-depot bup, including with naloxone
      # Arguably > 0 in s.ph.dose.bupr_opi should count too See
      # mod.subinterventions_str.R for why not
      s.ph.bupr_any = as.numeric(
        s.ph.buprwn_opi_main == 1 |
          s.ph.buprwn_opi_stbl == 1 |
          s.ph.buprwn_opi_with == 1 |
          s.ph.bupr_opi_main == 1 |
          s.ph.bupr_opi_stbl == 1 |
          s.ph.bupr_opi_with == 1 |
          s.ph.bupr_opi == 1 |
          s.ph.su.buprwn_opi == 1 |
          s.ph.us.buprwn_opi == 1 |
          s.ph.su.bupr_opi == 1 |
          s.ph.us.bupr_opi == 1),
      # Methadone old and new
      # Not including injected methadone, under methinj
      # Arguably > 0 in s.ph.dose.meth_opi should count too
      s.ph.meth_any = as.numeric(
        s.ph.us.meth_opi == 1 |
          s.ph.su.meth_opi == 1 |
          s.ph.meth_opi_stbl == 1 |
          s.ph.meth_opi_with == 1 |
          s.ph.meth_opi_main == 1 |
          s.ph.meth_opi == 1),
      # Un/Supervised buperenorphine  - note that supervised does not
      # include depot buprenorphine
      s.ph.us.bupr_any = as.numeric(
        s.ph.us.buprwn_opi == 1 |
          s.ph.us.bupr_opi == 1 |
          ((s.ph.buprwn_opi_main == 1 |
              s.ph.buprwn_opi_stbl == 1 |
              s.ph.buprwn_opi_with == 1 |
              s.ph.bupr_opi_main == 1 |
              s.ph.bupr_opi_stbl == 1 |
              s.ph.bupr_opi_with == 1 |
              s.ph.bupr_opi == 1) & s.ph.superv == 2)),
      s.ph.su.bupr_any = as.numeric(
        s.ph.su.buprwn_opi == 1 |
          s.ph.su.bupr_opi == 1 |
          ((s.ph.buprwn_opi_main == 1 |
              s.ph.buprwn_opi_stbl == 1 |
              s.ph.buprwn_opi_with == 1 |
              s.ph.bupr_opi_main == 1 |
              s.ph.bupr_opi_stbl == 1 |
              s.ph.bupr_opi_with == 1 |
              s.ph.bupr_opi == 1) & s.ph.superv == 1)),
      # Un/Supervised methadone - note that supervised includes injectable
      # methadone
      s.ph.us.meth_any = as.numeric(
        s.ph.us.meth_opi == 1 |
          ((s.ph.meth_opi_stbl == 1 |
              s.ph.meth_opi_with == 1 |
              s.ph.meth_opi_main == 1 |
              s.ph.meth_opi == 1) & s.ph.superv == 2)),
      s.ph.su.meth_any = as.numeric(
        s.ph.su.meth_opi == 1 |
          s.ph.methinj_opi == 1 |
          ((s.ph.meth_opi_stbl == 1 |
              s.ph.meth_opi_with == 1 |
              s.ph.meth_opi_main == 1 |
              s.ph.meth_opi == 1) & s.ph.superv == 1)),
      # These will likely be needed for blank SIR handling
      # Any other pharma SIR item (skipping methadone/bupe doses) but including
      # injected methadone OST
      s.ph.other = as.numeric(
        s.ph.alc_relpr == 1 |
          s.ph.alc_with == 1 |
          s.ph.stbl == 1 |
          s.ph.with == 1 |
          s.ph.main == 1 |
          s.ph.relprev == 1 |
          s.ph.diam == 1 |
          s.ph.nalt == 1 |
          s.ph.chlor == 1 |
          s.ph.acamp == 1 |
          s.ph.nalm == 1 |
          s.ph.disulf == 1 |
          s.ph.oth == 1 |
          s.ph.diam_opi == 1 |
          s.ph.methinj_opi == 1 |
          s.ph.benz_benz_main == 1 |
          s.ph.benz_stim_with == 1 |
          s.ph.benz_ghb_with == 1 |
          s.ph.stim_stim_wth == 1 |
          s.ph.pregab_gaba_with == 1 |
          s.ph.gaba_gaba_with == 1 |
          s.ph.nalt_opi_relprev == 1 |
          s.ph.nalt_alc_relprev == 1 |
          s.ph.chlor_alc_with == 1 |
          s.ph.benz_alc_with == 1 |
          s.ph.carba_alc_with == 1 |
          s.ph.oth_alc_with == 1 |
          s.ph.acamp_alc_relprev == 1 |
          s.ph.disulf_alc_relprev == 1 |
          s.ph.vit == 1 |
          s.ph.drg_oth == 1 |
          s.ph.superv %in% c(1, 2) | # Sticking to valid answers
          s.ph.intent %in% c("A","B","C")  # Sticking to valid answers
      ),
      # Any structured intervention non-pharma SIR item
      # Recovery support SIRs are in a different file and any recovery support
      # only SIRs are already excluded.
      s.non_ph.any = as.numeric(
        # Psychosocial
        s.psy.mi == 1 |
          s.psy.conting == 1 |
          s.psy.family == 1 |
          s.psy.behav == 1 |
          s.psy.mh == 1 |
          s.psy.psychody == 1 |
          s.psy.12s == 1 |
          s.psy.counsel == 1 |
          s.psy.dayprog == 1 |
          # YP psychosocial
          s.yp.behav == 1 |
          s.yp.mi == 1 |
          s.yp.family == 1 |
          s.yp.multic == 1 |
          s.yp.conting == 1 |
          s.yp.counsel == 1
      )
    )
} else {
  stop("SIR data file too old, see mod.subinterventions_str.R")
}

# Select only required fields
# This includes raw SIR data; I think really it's only likely that these
# will be needed

ost_sirs <- c(
  "s.ph.buprinj_any", # Depot bupe
  "s.ph.bupr_any", # Oral bupe
  "s.ph.us.bupr_any", # Oral bupe unsupervised
  "s.ph.su.bupr_any", # Oral bupe supervised
  "s.ph.meth_any", # Oral methadone
  "s.ph.us.meth_any", # Oral methadone unsupervised
  "s.ph.su.meth_any" # Oral methadone supervised
)


# Technically s.ph.diam and s.ph.methinj are also OST. Perhaps people with
# these rare sub-interventions should get excluded for analysis? - JC
# Partly addressed: is_diamorphine and is_injectable_methadone below (and
# ever_diamorphine, ever_injectable_methadone in 07) make the exclusion
# possible; the analysis doesn't exclude them yet.
# Create simplified SIR-level treatment flags for LAB modelling -----------
strsubinterventions <- strsubinterventions %>%
  mutate(
    # Main comparator treatments
    is_lab = if_else(
      s.ph.buprinj_any == 1,
      1, 0,
      missing = 0
    ),
    
    is_oral_buprenorphine = if_else(
      s.ph.bupr_any == 1,
      1, 0,
      missing = 0
    ),
    
    is_methadone = if_else(
      s.ph.meth_any == 1,
      1, 0,
      missing = 0
    ),
    
    # Main comparator OST:
    # LAB, oral buprenorphine or oral methadone
    is_comparator_ost = if_else(
      is_lab == 1 |
        is_oral_buprenorphine == 1 |
        is_methadone == 1,
      1, 0
    ),
    
    # Diamorphine OST
    is_diamorphine = if_else(
      s.ph.diam == 1 |
        s.ph.diam_opi == 1,
      1, 0,
      missing = 0
    ),
    
    # Injectable methadone OST
    is_injectable_methadone = if_else(
      s.ph.methinj_opi == 1,
      1, 0,
      missing = 0
    ),
    
    # Other injectable OST
    is_other_ost = if_else(
      is_diamorphine == 1 |
        is_injectable_methadone == 1,
      1, 0
    ),
    
    # Any OST, including comparator OST and other injectable OST
    is_any_ost = if_else(
      is_comparator_ost == 1 |
        is_other_ost == 1,
      1, 0
    )
  )


# Journey-level SIR summaries ---------------------------------------------

# Retain linkage and treatment variables required for journey-level exposure
# derivation in the final LAB analysis dataset.
strsubinterventions <- strsubinterventions %>%
  select(
    attrbdat,
    n_jy,
    agncy,
    triaged,
    disd_ep,
    submoddt,
    s.ph.superv,
    s.ph.intent,
    
    # Simple SIR-level treatment flags
    is_lab,
    is_oral_buprenorphine,
    is_methadone,
    is_comparator_ost,
    is_diamorphine,
    is_injectable_methadone,
    is_other_ost,
    is_any_ost,
    
    # Selected raw/convenience treatment fields for QA
    contains(c("meth", "bupr")),
    -any_of("s.ph.methinj_opi"),
    s.ph.other,
    s.non_ph.any
  ) %>% 
  # Remove any extra duplicates from selecting down
  unique()

# SIR output
# THIS WILL OUTPUT PII
# RDS file is suitable here
saveRDS(strsubinterventions,
        paste0(getwd(),
               "/outputs/extracts/sir_data_with_identifiers_v",
               ver,
               ".rds"))
