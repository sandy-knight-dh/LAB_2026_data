# -------------------------------------------------------------------------
# Script: qa_column_manifest.R
#
# Purpose:
# Prints a description of the final linked dataset that can be taken out of
# the secure environment: row, journey and client counts, and each column's
# class and share missing. No values are printed.
#
# Input:
# - outputs/extracts/final_linked_dataset_v{ver}_t{timestamp}.feather
# -------------------------------------------------------------------------

source("R/01_setup.R")

library(tidyverse)

run_timestamp <- readRDS(
  paste0("data/interim/latest_deidentified_timestamp_v", ver, ".rds")
)

final_dataset <- arrow::read_feather(
  paste0(
    "outputs/extracts/final_linked_dataset_v", ver, "_t", run_timestamp,
    ".feather"
  )
)

cat("Version", ver, "run", run_timestamp, "\n")
cat("Rows:", nrow(final_dataset), "\n")
cat(
  "Journeys:",
  nrow(distinct(final_dataset, client_random_id, n_jy)),
  "\n"
)
cat("Clients:", n_distinct(final_dataset$client_random_id), "\n\n")

manifest <- tibble(
  column = names(final_dataset),
  class = map_chr(final_dataset, ~ paste(class(.x), collapse = "/")),
  pct_missing = map_dbl(final_dataset, ~ round(100 * mean(is.na(.x)), 1))
)

print(manifest, n = Inf)

# Open journeys' length of stay is censored at a period end: version 5 used
# 2026-03-31. A different date here means mod.length_of_stay.R has changed.
censor_dates <- final_dataset %>%
  distinct(client_random_id, n_jy, .keep_all = TRUE) %>%
  filter(is.na(disd_jy)) %>%
  mutate(censor_date = triaged_jy + as.numeric(los.days, units = "days") - 1) %>%
  count(censor_date, sort = TRUE) %>%
  head(3)

cat("\nCensor date of open journeys' los.days (most common):\n")
print(censor_dates)
