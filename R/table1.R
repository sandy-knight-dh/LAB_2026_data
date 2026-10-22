# Table 1 ---------------------------------------------------------------------
#
# Characteristics by exposure group: counts and column percentages for
# categorical variables, with an Overall column. Works on the summary deck's
# clients (summary_characteristics()), matched or not.
#
# Usage:
#   table1 <- data |> add_table1_variables(df) |> make_table1()
#   format_table1(table1)
#
# Age is the data's age column: at the period start, or the journey start if
# later.
# Other variables are at the journey's start except where the label says
# otherwise. Missing values are shown as a "Missing" level.

# Variables -------------------------------------------------------------------

# Columns taken from the linked dataset; all constant within a journey
TABLE1_SOURCE_COLS <- c(
  "drug_a.crack",
  "drug_a.benzo",
  "ever_injecting",
  "n_prev_journeys",
  "previous_time_out_of_tx_triage",
  "los.days",
  "triaged_jy"
)

# Rows of Table 1, in order: label = column created by add_table1_variables()
TABLE1_VARS <- c(
  "Age" = "age_band",
  "Sex" = "sex_label",
  "Region" = "region",
  "Housing at assessment" = "housing_baseline",
  "Crack use at assessment" = "crack_assessment",
  "Benzodiazepine use at assessment" = "benzo_assessment",
  "Injecting on any TOP in journey (not baseline)" = "injecting_any_top",
  "Previous treatment journeys" = "prev_journeys_band",
  "Time out of treatment before journey" = "time_out_band",
  "Time in treatment on earlier journeys" = "prior_treatment_band",
  "Year journey started" = "triage_year"
)

# Housing groups from best to worst; where a journey's first assessment has
# more than one value (same-day assessments at different agencies), the worst
# is used
HOUSING_ORDER <- c(
  "Home owner",
  "Rented home",
  "Other home",
  "NFA",
  "Rough sleeping"
)

# Baseline housing ------------------------------------------------------------

#' Runs in dtplyr: it summarises the raw dataset by journey
#' @param df data.frame. Linked dataset
#' @return tibble. client_random_id, n_jy, housing_baseline (character).
#'   Housing group recorded at the journey's first assessment (episode triage
#'   date equal to journey triage date). Journeys with housing missing are
#'   left out, so they become NA when joined
baseline_housing <- function(df) {
  dtplyr::lazy_dt(df) |>
    dplyr::filter(triaged == triaged_jy) |>
    dplyr::distinct(client_random_id, n_jy, housing_grp_value) |>
    dplyr::mutate(housing_rank = match(housing_grp_value, HOUSING_ORDER)) |>
    dplyr::filter(!is.na(housing_rank)) |>
    dplyr::summarise(
      housing_rank = max(housing_rank),
      .by = c(client_random_id, n_jy)
    ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      client_random_id,
      n_jy,
      housing_baseline = HOUSING_ORDER[housing_rank]
    )
}

# Categorised variables -------------------------------------------------------

#' @param model_data tibble. One row per client with client_random_id, n_jy,
#'   exposure_group, age, sex and region: the summary deck's clients
#' @param df data.frame. Linked dataset
#' @return tibble. model_data with the columns in TABLE1_VARS added (factors)
add_table1_variables <- function(model_data, df) {
  journeys <- collapse_to_journeys(df, group_cols = TABLE1_SOURCE_COLS)
  extra <- journeys |>
    dplyr::select(client_random_id, n_jy, dplyr::all_of(TABLE1_SOURCE_COLS)) |>
    dplyr::left_join(
      treatment_history(journeys),
      by = c("client_random_id", "n_jy")
    )

  yes_no <- \(x) factor(dplyr::if_else(x == 1, "Yes", "No"), c("Yes", "No"))

  model_data |>
    dplyr::left_join(extra, by = c("client_random_id", "n_jy")) |>
    dplyr::left_join(
      baseline_housing(df),
      by = c("client_random_id", "n_jy")
    ) |>
    dplyr::mutate(
      age_band = cut(
        age,
        breaks = c(-Inf, 29, 39, 49, Inf),
        labels = c("18-29", "30-39", "40-49", "50+")
      ),
      sex_label = factor(
        dplyr::if_else(sex == "F", "Female", "Male"),
        c("Male", "Female")
      ),
      region = factor(region),
      housing_baseline = factor(housing_baseline, HOUSING_ORDER),
      crack_assessment = yes_no(drug_a.crack),
      benzo_assessment = yes_no(drug_a.benzo),
      injecting_any_top = yes_no(ever_injecting),
      prev_journeys_band = cut(
        n_prev_journeys,
        breaks = c(-Inf, 0, 1, 4, Inf),
        labels = c("None", "1", "2-4", "5+")
      ),
      # Time out not recorded on a journey with a previous one stays NA, so
      # it is shown as Missing (hence no .default)
      time_out_days = as.numeric(
        previous_time_out_of_tx_triage,
        units = "days"
      ),
      time_out_band = dplyr::case_when(
        n_prev_journeys == 0 ~ "No previous journey",
        time_out_days < 183 ~ "Under 6 months",
        time_out_days < 365 ~ "6-12 months",
        time_out_days < 730 ~ "1-2 years",
        time_out_days >= 730 ~ "2 years or more"
      ) |>
        factor(c(
          "No previous journey", "Under 6 months", "6-12 months", "1-2 years",
          "2 years or more"
        )),
      prior_years = prior_treatment_days / 365.25,
      prior_treatment_band = dplyr::case_when(
        n_prev_journeys == 0 ~ "No previous journey",
        prior_years < 1 ~ "Under 1 year",
        prior_years < 3 ~ "1-3 years",
        prior_years < 6 ~ "3-6 years",
        .default = "6 years or more"
      ) |>
        factor(c(
          "No previous journey", "Under 1 year", "1-3 years", "3-6 years",
          "6 years or more"
        )),
      triage_year = cut(
        lubridate::year(triaged_jy),
        breaks = c(-Inf, 2009, 2014, 2019, 2022, 2024, Inf),
        labels = c(
          "Before 2010", "2010-2014", "2015-2019", "2020-2022", "2023-2024",
          "2025 or later"
        )
      )
    ) |>
    dplyr::select(-time_out_days, -prior_years)
}

# Table -----------------------------------------------------------------------

#' @param data tibble. From add_table1_variables()
#' @param var character(1). Column to tabulate (factor)
#' @param label character(1). Row label for the variable
#' @param group character(1). Grouping column (factor)
#' @return tibble. variable, level (character), group (character), n
#'   (integer), pct (numeric, column percentage within group)
table1_counts <- function(data, var, label, group) {
  level_order <- c(levels(data[[var]]), "Missing")

  by_group <- data |>
    dplyr::count(group = .data[[group]], level = .data[[var]]) |>
    dplyr::mutate(group = as.character(group), level = as.character(level))

  overall <- by_group |>
    dplyr::summarise(n = sum(n), .by = level) |>
    dplyr::mutate(group = "Overall")

  dplyr::bind_rows(overall, dplyr::filter(by_group, !is.na(group))) |>
    dplyr::mutate(level = tidyr::replace_na(level, "Missing")) |>
    tidyr::complete(group, level, fill = list(n = 0L)) |>
    dplyr::mutate(pct = 100 * n / sum(n), .by = group) |>
    dplyr::mutate(variable = label, .before = 1) |>
    dplyr::arrange(match(level, level_order))
}

#' @param data tibble. From add_table1_variables()
#' @param vars named character vector. Labels = columns; defaults to
#'   TABLE1_VARS
#' @param group character(1). Grouping column (factor); its level order sets
#'   the column order. Rows where it is NA count in Overall only, so the
#'   groups need not sum to Overall
#' @return tibble. variable, level, then <group>_n (integer) and <group>_pct
#'   (numeric) for Overall and each group. The first row (Total) counts rows
#'   of data: journeys, or clients for the in-treatment population
make_table1 <- function(data, vars = TABLE1_VARS, group = "exposure_group") {
  group_levels <- c("Overall", levels(data[[group]]))

  by_group <- data |>
    dplyr::count(group = .data[[group]]) |>
    dplyr::mutate(group = as.character(group))

  totals <- dplyr::bind_rows(
    tibble::tibble(group = "Overall", n = sum(by_group$n)),
    dplyr::filter(by_group, !is.na(group))
  ) |>
    dplyr::mutate(variable = "Total", level = "", pct = 100, .before = 1)

  rows <- vars |>
    purrr::imap(\(var, label) table1_counts(data, var, label, group)) |>
    purrr::list_rbind()

  dplyr::bind_rows(totals, rows) |>
    dplyr::mutate(group = factor(group, group_levels)) |>
    tidyr::pivot_wider(
      names_from = group,
      values_from = c(n, pct),
      names_glue = "{group}_{.value}",
      names_sort = TRUE,
      names_vary = "slowest"
    )
}

#' @param table1 tibble. From make_table1()
#' @param digits integer(1). Decimal places for percentages
#' @return tibble. variable, level, and one "n (pct%)" character column per
#'   group. The Total row shows counts only
format_table1 <- function(table1, digits = 1) {
  groups <- sub("_n$", "", grep("_n$", names(table1), value = TRUE))

  formatted <- purrr::map(groups, \(g) {
    n <- table1[[paste0(g, "_n")]]
    pct <- table1[[paste0(g, "_pct")]]
    dplyr::if_else(
      table1$variable == "Total",
      format(n, big.mark = ",", trim = TRUE),
      paste0(
        format(n, big.mark = ",", trim = TRUE),
        " (", formatC(pct, format = "f", digits = digits), "%)"
      )
    )
  }) |>
    rlang::set_names(groups)

  dplyr::bind_cols(
    dplyr::select(table1, variable, level),
    tibble::as_tibble(formatted)
  )
}
