# Aggregate parameters for the synthetic data, from the v5 linked dataset.
#
# Run from the repo root: Rscript synthetic/R/parameters.R
#
# Everything saved is an aggregate (shares, percentiles, frequency tables)
# or a lookup (local authority names and codes, housing codes); no row of
# v5 is carried over. generate.R reads only this file, never v5 itself.
# The v5 dataset holds the opiate clients' journeys only, so the shares of
# non-opiate clients, of OST types and of record frequencies are set in
# generate.R, not here.

V5_PATH <- "data/final_linked_dataset_v5.feather"
PARAMETERS_PATH <- "synthetic/parameters.rds"
PERCENTILES <- seq(0, 1, 0.01)

v5 <- arrow::read_feather(
  V5_PATH,
  col_select = c(
    "client_random_id", "n_jy", "sex", "age_jy_start", "dat", "dat.value",
    "region", "utla_final", "drug.grp_sml", "drug_a.crack", "drug_a.benzo",
    "triaged_jy", "modst_jy", "disd_jy", "disrsn_jy",
    "previous_time_out_of_tx_triage", "housing", "housing_value",
    "housing_grp_value", "ever_lab", "ever_injecting"
  )
)

journeys <- v5 |>
  dplyr::distinct(client_random_id, n_jy, .keep_all = TRUE)

first_journeys <- journeys |>
  dplyr::filter(n_jy == 1)

closed <- journeys |>
  dplyr::filter(!is.na(disd_jy), !disrsn_jy %in% c(500, 999)) |>
  dplyr::mutate(
    duration = as.numeric(disd_jy - triaged_jy, units = "days"),
    early = duration < 84
  )

#' Frequency table as shares
#'
#' @param x Vector to tabulate (NA is a level).
#' @return Tibble with `value` and `share`.
shares <- function(x) {
  tibble::tibble(value = x) |>
    dplyr::count(value) |>
    dplyr::mutate(share = n / sum(n)) |>
    dplyr::select(value, share)
}

parameters <- list(
  sex_male = mean(first_journeys$sex == "M"),
  age_first_triage = stats::quantile(
    first_journeys$age_jy_start, PERCENTILES,
    na.rm = TRUE, names = FALSE
  ),
  first_triage = stats::quantile(
    as.numeric(first_journeys$triaged_jy), PERCENTILES,
    type = 1, names = FALSE
  ),
  journeys_per_client = shares(pmin(
    dplyr::count(journeys, client_random_id)$n, 12
  )),
  duration = stats::quantile(
    closed$duration, PERCENTILES,
    names = FALSE
  ),
  gap = stats::quantile(
    as.numeric(journeys$previous_time_out_of_tx_triage, units = "days"),
    PERCENTILES,
    na.rm = TRUE, names = FALSE
  ),
  wait = stats::quantile(
    as.numeric(journeys$modst_jy - journeys$triaged_jy, units = "days"),
    PERCENTILES,
    na.rm = TRUE, names = FALSE
  ),
  discharge_reason_early = shares(closed$disrsn_jy[closed$early]),
  discharge_reason_late = shares(closed$disrsn_jy[!closed$early]),
  crack = mean(journeys$drug_a.crack[journeys$drug.grp_sml == 1]),
  benzo = mean(journeys$drug_a.benzo[journeys$drug.grp_sml == 1]),
  injecting = mean(journeys$ever_injecting == 1, na.rm = TRUE),
  lab_clients = mean(
    dplyr::summarise(
      journeys,
      lab = max(ever_lab, na.rm = TRUE),
      .by = client_random_id
    )$lab == 1
  ),
  dat = shares(journeys$dat[!is.na(journeys$region)]),
  housing = shares(journeys$housing),
  dat_lookup = v5 |>
    dplyr::filter(!is.na(region)) |>
    dplyr::distinct(dat, dat.value, region, gss = utla_final),
  housing_lookup = v5 |>
    dplyr::filter(!is.na(housing)) |>
    dplyr::distinct(housing, housing_value, housing_grp_value) |>
    dplyr::arrange(housing)
)

saveRDS(parameters, PARAMETERS_PATH)
message("Saved ", PARAMETERS_PATH)
