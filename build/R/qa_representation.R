# -------------------------------------------------------------------------
# Script: qa_representation.R
#
# Purpose:
# Compares re-presentation as in version 5 (repres_6m/12m: the next journey
# in the same local authority within 183/365 days) with re-presentation in
# any English local authority (repres_any_la_6m/12m; NA when the client was
# already in treatment elsewhere at discharge), among successful completions
# in each financial year with the follow-up available. Prints counts only.
#
# Input:
# - outputs/extracts/final_linked_dataset_v{ver}_t{timestamp}.feather
# -------------------------------------------------------------------------

source("R/01_setup.R")

library(tidyverse)

run_timestamp <- readRDS(
  paste0("data/interim/latest_deidentified_timestamp_v", ver, ".rds")
)

journeys <- arrow::read_feather(
  paste0(
    "outputs/extracts/final_linked_dataset_v", ver, "_t", run_timestamp,
    ".feather"
  ),
  col_select = c(
    "client_random_id", "n_jy", "disd_jy", "disrsn_jy.grp", "drug.grp_sml",
    "repres_6m", "repres_12m", "repres_any_la_6m", "repres_any_la_12m",
    "next_dat_same", "followup_available_6m",
    "followup_available_12m"
  )
) %>%
  distinct(client_random_id, n_jy, .keep_all = TRUE)

completions <- journeys %>%
  filter(disrsn_jy.grp == 1, drug.grp_sml == 1) %>%
  mutate(
    year = case_when(
      between(disd_jy, as.Date("2022-04-01"), as.Date("2023-03-31")) ~ "2022/23",
      between(disd_jy, as.Date("2023-04-01"), as.Date("2024-03-31")) ~ "2023/24",
      between(disd_jy, as.Date("2024-04-01"), as.Date("2025-03-31")) ~ "2024/25",
      between(disd_jy, as.Date("2025-04-01"), as.Date("2026-03-31")) ~ "2025/26"
    )
  ) %>%
  filter(!is.na(year))

for (months in c(6, 12)) {
  old <- paste0("repres_", months, "m")
  new <- paste0("repres_any_la_", months, "m")
  followup <- paste0("followup_available_", months, "m")

  cat("\n== Re-presentation within", months, "months ==\n")

  cohort <- filter(completions, .data[[followup]] == 1)

  cat("\nOld (same local authority) by new (any local authority; NA =",
      "already in treatment elsewhere), journeys:\n")
  print(
    cohort %>%
      count(year, old = .data[[old]], new = .data[[new]]) %>%
      pivot_wider(names_from = year, values_from = n, values_fill = 0)
  )

  cat("\nRates, %:\n")
  print(
    cohort %>%
      summarise(
        completions = n(),
        not_followed_up = sum(is.na(.data[[new]])),
        old_rate = round(100 * mean(.data[[old]] == 1, na.rm = TRUE), 1),
        new_rate = round(100 * mean(.data[[new]] == 1, na.rm = TRUE), 1),
        new_in_other_la = sum(.data[[new]] == 1 & next_dat_same == 0,
                              na.rm = TRUE),
        .by = year
      ) %>%
      arrange(year)
  )
}
