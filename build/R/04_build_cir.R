# -------------------------------------------------------------------------
# Script: 04_build_cir.R
#
# Purpose:
# Builds cir dataset for LAB analysis.

# Imports CIR review file selecting only relevant variables but including
# all CIR reviews

# CIR review housing observations are combined with housing information
# recorded at journey triage to create a housing history.
#
# assign_date is used as the observation date:
# - cirdt for CIR review observations
# - triaged for journey assessment observations
#
# n_jy is assigned using episode/journey information so that journey-level
# housing measures can be derived later in 07_link_dataset.R.
#
# Note: if other CIR variables such as BBV testing, vaccinations or naloxone
# are used later, their linkage requirements may differ because those fields
# relate to services delivered by a provider.
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

journey <- readRDS(
  paste0(
    "data/interim/journey_filtered_v",
    ver,
    ".rds"
  )
)

# CIR data -----------------------------------------------------------------

# Load CIR review housing fields.
# CIR-prefixed variables are renamed to match their journey equivalents so
# that CIR and journey housing observations can be combined below.

cir <- readRDS(
  file.path(
    data_dir.full,
    "Z-verified files",
    "z_verified_cir.rds"
  )
) %>%
  select(
    attrb,
    dat,
    agncy,
    triaged,
    cirdt,
    housing = cirhousing,
    homeless = cirhomeless
  )

# Create journey assessment of housing.
# These are combined with CIR review to create housing history for each journey.
cir_jy <- journey %>%
  select(
    attrb,
    dat,
    sex,
    n_jy,
    agncy,
    triaged,
    disd_ep,
    housing,
    any_of("homeless")
  )

setDT(cir_jy) # Convert data to data.table format to save RAM
setDT(cir) 

cir_jy <- base::rbind(
  cir_jy,
  cir,
  fill = TRUE
)

rm(cir)
gc()


# Assign housing observation date and journey information -----------------

cir_jy <- cir_jy %>%
  mutate(
    # CIR reviews use cirdt; journey assessment records use triaged.
    assign_date = coalesce(cirdt, triaged)
  ) %>%
  group_by(
    attrb,
    dat,
    triaged,
    agncy
  ) %>%
  mutate(
    # CIR review records do not contain n_jy/disd_ep, so copy these
    # from the matching journey/episode record.
    n_jy = dplyr::first(na.omit(n_jy)),
    disd_ep = dplyr::first(na.omit(disd_ep)),
    sex = dplyr::first(na.omit(sex))
  ) %>%
  ungroup() %>%
  
  # Keep records assigned to a journey and within the valid episode/month.
  filter(
    !is.na(n_jy),
    assign_date <= period.end,
    assign_date <= disd_ep | is.na(disd_ep)
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

message(
  "CIR records in final extract: ",
  nrow(cir_jy)
)

message(
  "CIR records with missing assign_date: ",
  sum(is.na(cir_jy$assign_date))
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
