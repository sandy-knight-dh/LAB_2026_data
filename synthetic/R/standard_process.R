# The standard monthly process on the synthetic extracts.
#
# Run from synthetic/ (the folder holding Q:), after generate.R:
#   cd synthetic && Rscript R/standard_process.R
#
# For each period it does what reg.journeys.R and reg.attributes.R do in the
# secure environment, sourcing the standard scripts unchanged from
# Q:/R_standard_modules (the reference copy), and writes:
# - Q:/Data/Datasets/<yyyy-mm>/journey_<yyyy-mm>.rds, which
#   02_build_journey.R reads;
# - .../R_journeys/recent_journey_monthly_<yyyy-mm>.rds, the journey file
#   with every module applied (08_standard_by_year.R, validate_scnr.R), as
#   the March folders on Q: name it (the reference reg.attributes.R calls it
#   recent_journey_all_).
#
# Where it differs from the secure environment:
# - It starts from the Z-verified .rds files (generate.R writes them), so
#   init.modality.R and the other init scripts are not run.
# - The journey file gets mod.substances.R and mod.discharges.R before it
#   is saved. 02 reads drug.grp_sml, drug_a.*, disrsn_jy and disrsn_jy.grp
#   from it, so the periodic journey file must carry them, but the
#   reference reg.journeys.R adds neither. The same goes for
#   triaged_jy_next (add_next_triage() below).
# - Of reg.attributes.R it runs only the modules whose outputs the build
#   reads (effective treatment, length of stay, SCNR, TOP and the
#   Treatment Progress Measure, mod.composite.R).
# - pri_case.adult, which mod.composite.R uses but the reference copy never
#   makes, is set as pri_case.adult.rolling, the monthly report's adult
#   primary case. The periodic copy on Q: presumably makes it.

STANDARD <- "Q:/R_standard_modules"
PERIODS <- list(c(3, 2023), c(3, 2024), c(3, 2025), c(9, 2025), c(3, 2026))

#' Add the next journey's triage date, which the periodic journey file has
#'
#' The reference jy scripts don't make triaged_jy_next, but 02 drops it from
#' the journey file before deriving its own ("triaged_jy_next is in the file"
#' - JC notes on 02), so the periodic file carries it.
#'
#' @param journey Journey file (one row per modality).
#' @return `journey` with `triaged_jy_next`: the triage date of the next
#'   journey in the same local authority, NA for the last.
add_next_triage <- function(journey) {
  next_triage <- journey |>
    dplyr::distinct(attrb, dat, n_jy, triaged_jy) |>
    dplyr::arrange(attrb, dat, n_jy) |>
    dplyr::mutate(
      triaged_jy_next = dplyr::lead(triaged_jy),
      .by = c(attrb, dat)
    ) |>
    dplyr::select(-triaged_jy)
  dplyr::left_join(journey, next_triage, by = c("attrb", "dat", "n_jy"))
}

#' Source a standard script unchanged, into the global environment
#'
#' @param path Character, the path under STANDARD.
#' @return Invisible NULL.
source_standard <- function(path) {
  message("  ", path)
  source(file.path(STANDARD, path), local = globalenv())
  invisible(NULL)
}

for (run_period in PERIODS) {
  period <- run_period
  source_standard("init/init.setup.R")
  run_type <- "monthly"
  message("Period ", data_dir.trnc)

  # reg.journeys.R, from the Z-verified modality file on
  interventions <- readRDS(
    paste0(data_dir.full, "/Z-verified files/z_verified_modality.rds")
  )
  source_standard("jy/jy.exclusions.R")
  interventions <- readRDS(
    paste0(data_dir.full, "/intervention_", data_dir.trnc, ".rds")
  )
  source_standard("jy/jy.sequencing.R")
  source_standard("jy/jy.age_related.R")
  source_standard("jy/jy.primary_case.R")
  journey <- interventions
  rm(interventions)
  source_standard("modules/mod.substances.R")
  source_standard("modules/mod.discharges.R")
  journey <- add_next_triage(journey)
  saveRDS(journey, paste0(data_dir.full, "/journey_", data_dir.trnc, ".rds"))

  journey <- journey |>
    dplyr::filter(
      triaged_jy <= mon.end,
      disd_jy >= scnr.start | is.na(disd_jy)
    )
  saveRDS(
    journey,
    paste0(output_dir, "/recent_journey_", data_dir.trnc, ".rds")
  )

  # reg.attributes.R, the modules the build reads
  source_standard("modules/mod.effective_treatment.R")
  source_standard("modules/mod.length_of_stay.R")
  source_standard("modules/mod.scnr.R")
  source_standard("modules/mod.top.R")
  journey$pri_case.adult <- journey$pri_case.adult.rolling
  source_standard("modules/mod.composite.R")
  saveRDS(
    journey,
    paste0(output_dir, "/recent_journey_monthly_", data_dir.trnc, ".rds")
  )
  rm(journey)
  gc()
}
