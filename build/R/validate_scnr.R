# -------------------------------------------------------------------------
# Script: validate_scnr.R
#
# Purpose:
# Runs the standard successful completion and non-re-presentation module
# (modules/mod.scnr.R) once, for completions from April 2024 to March 2025
# with six months' follow-up, and prints its counts for adult opiate
# clients. These are compared with the analysis project's 2024/25
# six-month re-presentation figures (summary_re_presentations_6m.csv, All
# row), which use repres_any_la_6m from this build: the next journey in any
# English local authority within 183 days, with completions during another
# journey left out. The module uses calendar months and its own
# ineligibility rules, so the two should be close, not equal.
#
# period = c(9, 2025) gives scnr.start = 2024-04-01, scnr.end = 2025-03-31
# and mon.end = 2025-09-30 (init.globals.R).
#
# Expected differences, to report alongside the comparison:
# - Age: the module uses age_jy.scnr (from date of birth, at the later of
#   journey start and scnr.start); the analysis approximates age from
#   age_jy_start.
# - The module takes one journey per client and local authority
#   (pri_jy.scnr.adult), preferring a completion; the analysis takes the
#   latest journey open in the year that ended in a completion.
# - The analysis counts completions with six months of follow-up in the
#   data (to 2026-03-31); this run's data end at 2025-09-30, which is
#   exactly six months after the last completion.
#
# Runs on its own: it sets its own period and does not use 01_setup.R, so
# it doesn't change anything the build uses. Prints counts only.
# -------------------------------------------------------------------------

library(lubridate)

std_modules <- "Q:/R_standard_modules_periodic"
period <- c(9, 2025)
run_type <- "monthly"

source(file.path(std_modules, "init/init.setup.R"))

stopifnot(
  scnr.start == as.Date("2024-04-01"),
  scnr.end == as.Date("2025-03-31")
)

# The September 2025 journey file with every module applied, if it exists;
# otherwise the journey file with the modules mod.scnr.R needs
journey_all_path <- paste0(output_dir, "/recent_journey_all_", data_dir.trnc,
                           ".rds")

if (file.exists(journey_all_path)) {
  journey <- readRDS(journey_all_path)
} else {
  journey <- readRDS(paste0(data_dir.full, "/journey_", data_dir.trnc, ".rds"))
  source(file.path(std_modules, "modules/mod.substances.R"))
  source(file.path(std_modules, "modules/mod.discharges.R"))
}

# Re-run the module so its output is the current code's
journey <- journey %>%
  select(-any_of(c(
    "pri_case.scnr.adult", "pri_jy.scnr.adult", "pri_case.scnr.yp",
    "pri_jy.scnr.yp", "scnr.comp", "scnr.repres", "scnr.repres_inelig",
    "scnr.repres_triage_jy"
  )))

source(file.path(std_modules, "modules/mod.scnr.R"))

scnr_adult_opiate <- journey %>%
  filter(pri_case.scnr.adult == 1, drug.grp_sml == 1)

cat("\nmod.scnr.R, completions 2024-04-01 to 2025-03-31, adult opiate\n")
cat("clients (one row per client and local authority):\n")
print(
  scnr_adult_opiate %>%
    summarise(
      clients_in_cohort = n(),
      completions = sum(scnr.comp == 1, na.rm = TRUE),
      re_presented_6m = sum(scnr.repres == 1, na.rm = TRUE),
      rate_pct = round(100 * re_presented_6m / completions, 1),
      ineligible_completions = sum(scnr.repres_inelig > 0, na.rm = TRUE)
    )
)
