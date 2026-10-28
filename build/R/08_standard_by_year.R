# -------------------------------------------------------------------------
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: 08_standard_by_year.R
#
# Purpose:
# Takes the standard monthly outputs for the end of each financial year and
# keeps, for this project's clients, the measures the standard modules
# compute for that year: the Treatment Progress Measure (mod.top.R,
# mod.composite.R), age and the latest-journey and new-journey flags
# (jy.age_related.R, jy.primary_case.R), effective treatment and length of
# stay. Nothing is recomputed: the March file of each year already has every
# module applied for the rolling 12 months to March, which is the financial
# year.
#
# One row per client, journey and year: the client's latest journey in the
# year (pri_case.rolling == 1), as NDTMS counts people in treatment.
#
# Inputs:
# - Q:/Data/Datasets/{yyyy}-03/R_journeys/recent_journey_monthly_{yyyy}-03.rds,
#   for each year in STANDARD_YEARS: the journey file with every module
#   applied (the file mod.composite.R adds the Treatment Progress Measure
#   to). It has a row per episode and modality, of which pri_case.rolling
#   flags one per journey
# - data/interim/client_id_lookup_v{ver}_t{timestamp}.rds (from 06)
# - data/interim/journey_filtered_v{ver}.rds (from 02, to match journeys)
#
# Output:
# - outputs/extracts/standard_by_year_v{ver}_t{timestamp}.feather (copy it
#   to the analysis project as data/standard_by_year_v{ver}.feather)
#
# Matching journeys:
# n_jy is renumbered every month, so a journey in an earlier March file can
# have a different n_jy from the same journey in this build. Journeys are
# matched on attrbdat, n_jy and triaged_jy first, then on attrbdat and
# triaged_jy. The counts matched at each step and the unmatched are printed.
#
# PII:
# - Inputs contain attrbdat; the output is pseudonymised (client_random_id)
#   but remains sensitive.
# -------------------------------------------------------------------------

source("R/01_setup.R")

library(tidyverse)

# Financial years: the March file of the year's end
STANDARD_YEARS <- tibble(
  year = c("2022/23", "2023/24", "2024/25", "2025/26"),
  file_year = c(2023, 2024, 2025, 2026)
) %>%
  mutate(
    path = paste0(
      "Q:/Data/Datasets/", file_year,
      "-03/R_journeys/recent_journey_monthly_",
      file_year, "-03.rds"
    )
  )

# Check every year's file exists before reading any. If one doesn't, stop
# and list the files in the nearest folder that does exist (names only), so
# the path above can be put right
for (path in STANDARD_YEARS$path[!file.exists(STANDARD_YEARS$path)]) {
  folder <- dirname(path)
  while (!dir.exists(folder) && folder != dirname(folder)) {
    folder <- dirname(folder)
  }
  message("Not found: ", path, "
Files in ", folder, ":
  ",
          paste(list.files(folder), collapse = "\n  "))
}
if (!all(file.exists(STANDARD_YEARS$path))) {
  stop("Standard files missing (listed above); none were read")
}

STANDARD_COLUMNS <- c(
  # Age at the later of journey start and the year's start, from dob
  "age_jy.rolling",
  # Latest journey in the year, adult latest journey, new journey
  "pri_jy.rolling",
  "pri_case.adult.rolling",
  "new_jy.rolling",
  # Retention and length of stay as computed for the year
  "effective_tx",
  "effective_tx.reg",
  "los.days",
  "los.months.grp",
  # Treatment Progress Measure (mod.composite.R)
  "lof.composite_latest",
  "lof.composite_latest_grp_sml",
  "lof.composite_latest_grp_lrg",
  # Successful completion and non-re-presentation for the lagged window
  # (scnr.start to scnr.end): for reference only
  "scnr.comp",
  "scnr.repres"
)

run_timestamp <- readRDS(
  paste0("data/interim/latest_deidentified_timestamp_v", ver, ".rds")
)

client_id_lookup <- readRDS(
  paste0(
    "data/interim/client_id_lookup_v", ver, "_t", run_timestamp, ".rds"
  )
)

# This build's journeys: one row per attrbdat + n_jy
build_journeys <- readRDS(
  paste0("data/interim/journey_filtered_v", ver, ".rds")
) %>%
  as_tibble() %>%
  distinct(attrbdat, n_jy, triaged_jy)

stopifnot(!anyDuplicated(build_journeys[c("attrbdat", "n_jy")]))


# Read each year ----------------------------------------------------------

read_standard_year <- function(year, file_year, path) {
  # Read with tidypolars from the parquet copy (R/functions_inputs.R): only
  # our clients' latest journeys and the columns used below. los.days comes
  # back in seconds; it is put back in days, as in the .rds.
  standard <- scan_input(path)
  columns <- names(standard)

  # Older files (the March 2023 one) name the rolling year's columns without
  # the .rolling suffix: pri_case, pri_jy, pri_case.adult, age_jy (beside
  # pri_case.annual, .ytd, .qtr and so on). In the current files the
  # unsuffixed pri_case is the primary case of any period, so the unsuffixed
  # name is used only where the .rolling one is missing, and checked below
  rolling_names <- c("pri_case.rolling", "pri_jy.rolling",
                     "pri_case.adult.rolling", "age_jy.rolling",
                     "new_jy.rolling")
  older_names <- sub("\\.rolling$", "", rolling_names)
  use_older <- !rolling_names %in% columns & older_names %in% columns
  pri_case_column <- if_else(use_older[1], older_names[1], rolling_names[1])

  # Stop if the file lacks a column the read needs, naming its primary-case
  # and age columns (names only), so the right ones can be chosen
  missing_keys <- setdiff(c("attrbdat", "n_jy", "triaged_jy", "disd_jy",
                            pri_case_column), columns)
  if (length(missing_keys) > 0) {
    stop(path, " has no ", paste(missing_keys, collapse = ", "),
         "; its primary-case and age columns are: ",
         paste(grep("^pri_|^age_jy", columns, value = TRUE),
               collapse = ", "))
  }
  standard <- standard %>%
    filter(.data[[pri_case_column]] == 1) %>%
    filter(attrbdat %in% client_id_lookup$attrbdat) %>%
    select(attrbdat, n_jy, triaged_jy, disd_jy,
           any_of(c(STANDARD_COLUMNS, older_names[use_older]))) %>%
    as_tibble()
  if ("los.days" %in% names(standard)) {
    units(standard$los.days) <- "days"
  }

  if (any(use_older)) {
    standard <- standard %>%
      rename(any_of(setNames(older_names[use_older],
                             rolling_names[use_older])))
    # The rolling year to this March. Every latest journey must overlap it:
    # the primary case of any period, or of the calendar year (annual),
    # would include journeys that ended before the year began
    rolling_start <- as.Date(paste0(file_year - 1, "-04-01"))
    rolling_end <- as.Date(paste0(file_year, "-03-31"))
    outside <- sum(standard$triaged_jy > rolling_end |
                     (!is.na(standard$disd_jy) &
                        standard$disd_jy < rolling_start))
    message(
      year, ": older column names, read as the rolling year's: ",
      paste(older_names[use_older], collapse = ", "),
      "; latest journeys outside the rolling year: ", outside
    )
    if (outside > 0) {
      stop(pri_case_column, " in ", path, " is not the rolling year's ",
           "primary case")
    }
    # A new journey in the rolling year is one triaged in it
    if ("new_jy.rolling" %in% rolling_names[use_older]) {
      disagree <- sum(standard$new_jy.rolling !=
                        as.numeric(standard$triaged_jy >= rolling_start),
                      na.rm = TRUE)
      message(year, ": new_jy differs from triage in the rolling year for ",
              disagree, " journeys", if (disagree > 0) "; left NA")
      if (disagree > 0) {
        standard$new_jy.rolling <- NA
      }
    }
  }

  missing_columns <- setdiff(STANDARD_COLUMNS, names(standard))
  message(
    year, ": ", nrow(standard), " latest journeys of our clients",
    if (length(missing_columns) > 0) {
      paste0("; columns missing (left NA): ",
             paste(missing_columns, collapse = ", "))
    } else {
      "; all columns present"
    }
  )

  standard %>%
    select(attrbdat, n_jy_standard = n_jy, triaged_jy,
           any_of(STANDARD_COLUMNS)) %>%
    mutate(year = year, .before = 1)
}

standard <- pmap(STANDARD_YEARS, read_standard_year) %>%
  list_rbind()

# Columns a year lacks are NA
for (column in setdiff(STANDARD_COLUMNS, names(standard))) {
  standard[[column]] <- NA
}

stopifnot(
  "more than one latest journey per client and year" =
    !anyDuplicated(standard[c("year", "attrbdat")])
)


# Match to this build's journeys ------------------------------------------

# 1) Same journey number and triage date
matched_exact <- standard %>%
  inner_join(
    build_journeys,
    by = c("attrbdat", "n_jy_standard" = "n_jy", "triaged_jy")
  ) %>%
  mutate(n_jy = n_jy_standard, match = "n_jy and triaged_jy")

# 2) The rest, on triage date
matched_triage <- standard %>%
  anti_join(matched_exact, by = c("year", "attrbdat")) %>%
  inner_join(
    build_journeys,
    by = c("attrbdat", "triaged_jy"),
    relationship = "many-to-one"
  ) %>%
  mutate(match = "triaged_jy")

unmatched <- standard %>%
  anti_join(matched_exact, by = c("year", "attrbdat")) %>%
  anti_join(matched_triage, by = c("year", "attrbdat"))

print(
  bind_rows(
    count(matched_exact, year, match),
    count(matched_triage, year, match),
    count(unmatched, year) %>% mutate(match = "unmatched")
  ) %>%
    arrange(year, match)
)

standard_by_year <- bind_rows(matched_exact, matched_triage) %>%
  inner_join(client_id_lookup, by = "attrbdat") %>%
  select(client_random_id, n_jy, year, match, all_of(STANDARD_COLUMNS))

stopifnot(
  !anyDuplicated(standard_by_year[c("client_random_id", "n_jy", "year")]),
  !any(c("attrbdat", "attrb") %in% names(standard_by_year))
)


# QA ----------------------------------------------------------------------

# Treatment Progress Measure categories by year (counts only)
print(
  standard_by_year %>%
    count(year, lof.composite_latest_grp_sml) %>%
    pivot_wider(names_from = year, values_from = n)
)


# Save --------------------------------------------------------------------

arrow::write_feather(
  standard_by_year,
  paste0(
    "outputs/extracts/standard_by_year_v", ver, "_t", run_timestamp,
    ".feather"
  )
)

message("Saved standard_by_year_v", ver, "_t", run_timestamp, ".feather")
