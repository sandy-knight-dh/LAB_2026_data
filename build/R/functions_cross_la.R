# -------------------------------------------------------------------------
# Project: Long-acting buprenorphine (LAB) deep dive
# Script: functions_cross_la.R
#
# Purpose:
# Treatment history across local authorities. Sourced by 01_setup.R; no data
# is read or written here.
#
# The journey file numbers journeys within attrbdat (client within a local
# authority), and 02_build_journey.R's time out of treatment (lag/lead of
# journeys within attrbdat) can't see a client's journeys in other local
# authorities. The standard re-presentation measure (modules/mod.scnr.R)
# looks across all of the client's (attrb) journeys in England, but works on
# one fixed reporting window with 6 months' follow-up, so it can't be used
# as it is for each financial year and 12 months. This function is the
# lag/lead code from 02_build_journey.R ("Calculated fields"), grouped by
# attrb instead of attrbdat and ordered by triage date instead of n_jy (n_jy
# isn't comparable across local authorities; within one it follows triage
# order).
#
# Journeys in different local authorities can overlap: the next journey by
# triage date can start before this one ends, so following time out of
# treatment can be negative. 07_link_analysis_dataset.R treats those
# discharges as not followed up (re-presentation NA), as mod.scnr.R treats a
# completion during another journey as ineligible.
# -------------------------------------------------------------------------

#' Previous and next journey in any English local authority
#' @param journeys data.frame. One row per journey, every English journey of
#'   the clients (attrb) concerned: attrb, dat, n_jy, triaged_jy, disd_jy
#' @return tibble. One row per journey: attrb, dat, n_jy,
#'   n_prev_journeys_any_la, disd_jy_previous_any_la, triaged_jy_next_any_la,
#'   next_dat_same (1 if the next journey is in the same local authority),
#'   previous_time_out_of_tx_triage_any_la,
#'   following_time_out_of_tx_triage_any_la (difftime, days)
journeys_any_la <- function(journeys) {
  journeys <- journeys %>%
    dplyr::as_tibble() %>%
    dplyr::select(attrb, dat, n_jy, triaged_jy, disd_jy)

  stopifnot(
    "more than one row per journey" =
      !anyDuplicated(journeys[c("attrb", "dat", "n_jy")])
  )

  journeys %>%
    dplyr::group_by(attrb) %>%
    dplyr::arrange(triaged_jy, dat, n_jy, .by_group = TRUE) %>%
    # lag/lead are previous/next row in group
    dplyr::mutate(
      n_prev_journeys_any_la = dplyr::row_number() - 1,
      disd_jy_previous_any_la = dplyr::lag(disd_jy),
      triaged_jy_next_any_la = dplyr::lead(triaged_jy),
      next_dat_same = as.numeric(dplyr::lead(dat) == dat)
    ) %>%
    dplyr::ungroup() %>%
    # -1 because e.g. leaving tx on the 1st and starting on the 2nd is 0
    # days without tx
    dplyr::mutate(
      previous_time_out_of_tx_triage_any_la =
        triaged_jy - disd_jy_previous_any_la - 1,
      following_time_out_of_tx_triage_any_la =
        triaged_jy_next_any_la - disd_jy - 1
    ) %>%
    dplyr::select(-triaged_jy, -disd_jy)
}
