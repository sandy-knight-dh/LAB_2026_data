# Synthetic NDTMS extracts: a stand-in Q: drive for running the build here.
#
# Run from the repo root, after parameters.R: Rscript synthetic/R/generate.R
#
# Writes synthetic/Q:/, laid out like the secure environment's Q: drive, so
# that the standard modules and the build, which read "Q:/..." paths, find
# it when R runs from a folder holding Q: (synthetic/ itself, or build/,
# which gets a link build/Q: to it):
# - Data/Datasets/<yyyy-mm>/Z-verified files/z_verified_{modality,top,cir,
#   submodality_str}.rds for every period in PERIODS. These are the files the
#   standard init scripts (init.modality.R ...) make from the NDEC extracts,
#   in the same shape (lower-case NDTMS field names, dates as Date, Y/N
#   recoded as the init scripts do), so the init step is skipped.
# - Resources/Agency Tables/<fyear>/Agency List <Month> <yyyy>.xlsx, read by
#   jy.exclusions.R.
# - R_standard_modules: a link to the reference copy in this repo, and
#   R_standard_modules_periodic: links to the same folders, except that
#   lookups/ holds links to the reference lookups plus m.dat.csv and
#   k.housing.csv, which the reference copy lacks (made here from the
#   parameters).
#
# What the data are: one history of people, local authorities, journeys,
# episodes, modalities and TOP, CIR and SIR records to 2026-03-31, drawn from
# the aggregate distributions in parameters.rds where v5 has them (sex, age,
# triage dates, journeys per client, journey length, time out of treatment,
# waiting time, discharge reason by journey length, crack and benzodiazepine
# use, injecting, LAB share, local authority, housing) and from the shares
# set below where it doesn't. Each period's extract is that history as it
# stood at the period end: later records are dropped and later discharges
# blanked. The figures it gives are not NDTMS figures and mean nothing; it
# exists to run the code end to end.

PARAMETERS_PATH <- "synthetic/parameters.rds"
Q_DRIVE <- "synthetic/Q:"
STANDARD_MODULES <- "R_standard_modules"

SEED <- 20261027
# Number of people; SYNTHETIC_PEOPLE overrides it (e.g. to measure memory at
# a larger scale)
N_PEOPLE <- as.integer(Sys.getenv("SYNTHETIC_PEOPLE", "40000"))
DATA_END <- as.Date("2026-03-31")
HISTORY_START <- as.Date("2005-04-01")
SIR_START <- as.Date("2017-04-01") # SIRs were introduced in 2017
LAB_START <- as.Date("2020-06-01") # first LAB SIRs in v5: 2020-04
USH_START <- as.Date("2024-04-01") # TOP housing items added April 2024

# Periods (month, year) whose extracts are written: the four March files 08
# reads, and September 2025 for validate_scnr.R
PERIODS <- list(c(3, 2023), c(3, 2024), c(3, 2025), c(9, 2025), c(3, 2026))

# Shares not in v5
DRUG_TYPES <- c(
  opiate = 0.6, alcohol = 0.2, non_opiate = 0.12, alcohol_non_opiate = 0.08
)
LAS_PER_PERSON <- c(0.92, 0.07, 0.01)
NON_ENGLAND_DATS <- c("W01B", "W02B", "W03B")
NON_ENGLAND_SHARE <- 0.01
AGENCIES_PER_DAT <- 3
TWO_EPISODE_SHARE <- 0.15
TRIAGE_ONLY_SHARE <- 0.02
# OST type of opiate clients other than LAB (LAB's share is from v5)
ORAL_OST <- c(methadone = 0.6, oral_buprenorphine = 0.27, none = 0.13)
LAB_SWITCH_SHARE <- 0.4 # LAB clients whose journey starts on oral OST
# Probability that each scheduled record exists
TOP_BASE <- 0.85
TOP_REVIEW <- 0.7
TOP_EXIT_COMPLETED <- 0.55
TOP_EXIT_OTHER <- 0.2
CIR_REVIEW <- 0.55
REVIEW_INTERVAL <- 182

set.seed(SEED)
parameters <- readRDS(PARAMETERS_PATH)

# Helpers ----

#' Draw from a distribution given by its percentiles
#'
#' @param percentiles Numeric, the 0th to 100th percentiles.
#' @param n Number of draws.
#' @return Numeric vector of length `n`, linear between percentiles.
draw_percentiles <- function(percentiles, n) {
  stats::approx(
    seq(0, 1, length.out = length(percentiles)), percentiles,
    xout = stats::runif(n)
  )$y
}

#' Draw from a frequency table
#'
#' @param shares Tibble with `value` and `share` (from parameters.R).
#' @param n Number of draws.
#' @return Vector of `value`s.
draw_shares <- function(shares, n) {
  shares$value[sample.int(nrow(shares), n, replace = TRUE, prob = shares$share)]
}

#' Draw names with probabilities
#'
#' @param probabilities Named numeric vector.
#' @param n Number of draws.
#' @return Character vector of names.
draw_named <- function(probabilities, n) {
  sample(names(probabilities), n, replace = TRUE, prob = probabilities)
}

#' Repeat each row a number of times and number the copies
#'
#' @param df Data frame.
#' @param times Name of the integer column giving each row's copies.
#' @param index Name of the new copy-number column (1, 2, ...).
#' @return `df` with `sum(df[[times]])` rows.
expand_rows <- function(df, times, index) {
  n <- df[[times]]
  df <- df[rep(seq_len(nrow(df)), n), ]
  df[[index]] <- sequence(n)
  df
}

#' Yes (1) or No (2) with a given probability of Yes
#'
#' @param n Number of draws.
#' @param yes Probability of Yes.
#' @return Numeric vector of 1 and 2.
yes_no <- function(n, yes) {
  ifelse(stats::runif(n) < yes, 1, 2)
}

# People and local authorities ----
# A person is an attrb; a client is an attrbdat (a person in a local
# authority). Ages and first triage dates are those of v5's first journeys.
# The substances a person uses are fixed for their whole history.

people <- tibble::tibble(
  attrb = sprintf("SYN%07d", seq_len(N_PEOPLE)),
  sex = ifelse(stats::runif(N_PEOPLE) < parameters$sex_male, "M", "F"),
  drug_type = draw_named(DRUG_TYPES, N_PEOPLE),
  first_triage = pmax(
    as.Date(round(draw_percentiles(parameters$first_triage, N_PEOPLE))),
    HISTORY_START
  ),
  age_first = draw_percentiles(parameters$age_first_triage, N_PEOPLE),
  u = stats::runif(N_PEOPLE)
) |>
  dplyr::mutate(
    opiate = drug_type == "opiate",
    non_opiate = drug_type %in% c("non_opiate", "alcohol_non_opiate"),
    alcohol = drug_type %in% c("alcohol", "alcohol_non_opiate") |
      (opiate & stats::runif(N_PEOPLE) < 0.15),
    crack = (opiate & stats::runif(N_PEOPLE) < parameters$crack) |
      (non_opiate & stats::runif(N_PEOPLE) < 0.1),
    benzo = opiate & stats::runif(N_PEOPLE) < parameters$benzo,
    cocaine = non_opiate & stats::runif(N_PEOPLE) < 0.5,
    cannabis = (non_opiate & !cocaine) |
      (opiate & stats::runif(N_PEOPLE) < 0.1),
    injector = opiate & stats::runif(N_PEOPLE) < parameters$injecting,
    ost = dplyr::case_when(
      !opiate ~ "none",
      u < parameters$lab_clients ~ "lab",
      TRUE ~ draw_named(ORAL_OST, N_PEOPLE)
    ),
    lab_first_oral = draw_named(
      c(methadone = 0.6, oral_buprenorphine = 0.4), N_PEOPLE
    ),
    dob = first_triage - round(age_first * 365.25) -
      sample(0:364, N_PEOPLE, replace = TRUE),
    # Days of use at the first TOP, and the factor it falls by at each TOP
    use_level = ifelse(
      stats::runif(N_PEOPLE) < 0.15,
      0,
      sample(1:28, N_PEOPLE, replace = TRUE, prob = 1:28)
    ),
    responder = sample(
      c(0.3, 0.8, 1), N_PEOPLE,
      replace = TRUE, prob = c(0.4, 0.35, 0.25)
    )
  ) |>
  dplyr::select(-u)

dat_codes <- parameters$dat$value

clients <- people |>
  dplyr::select(attrb, first_triage) |>
  dplyr::mutate(n_las = sample(
    seq_along(LAS_PER_PERSON), N_PEOPLE,
    replace = TRUE, prob = LAS_PER_PERSON
  )) |>
  expand_rows("n_las", "la_index") |>
  dplyr::mutate(
    dat = sample(
      dat_codes, dplyr::n(),
      replace = TRUE, prob = parameters$dat$share
    ),
    dat = ifelse(
      stats::runif(dplyr::n()) < NON_ENGLAND_SHARE,
      sample(NON_ENGLAND_DATS, dplyr::n(), replace = TRUE),
      dat
    ),
    # A second or third local authority starts later in the person's history
    start = dplyr::if_else(
      la_index == 1,
      first_triage,
      first_triage + round(
        stats::runif(dplyr::n()) * as.numeric(DATA_END - first_triage)
      )
    )
  ) |>
  dplyr::distinct(attrb, dat, .keep_all = TRUE) |>
  dplyr::select(attrb, dat, start)

agencies <- tidyr::expand_grid(
  dat = c(dat_codes, NON_ENGLAND_DATS),
  agency_index = seq_len(AGENCIES_PER_DAT)
) |>
  dplyr::mutate(agncy = paste0("AG", dat, agency_index))

# Journeys ----
# Journey lengths are v5's closed journeys; a journey still running at
# DATA_END is open. Time out of treatment is v5's, at least 22 days so that
# jy.sequencing.R keeps the journeys apart. Discharge reasons are v5's
# journey-level codes, by whether the journey lasted 12 weeks.

journeys <- clients |>
  dplyr::mutate(n_journeys = draw_shares(
    parameters$journeys_per_client, dplyr::n()
  )) |>
  expand_rows("n_journeys", "journey_index") |>
  dplyr::mutate(
    duration = round(draw_percentiles(parameters$duration, dplyr::n())),
    gap = pmax(round(draw_percentiles(parameters$gap, dplyr::n())), 22)
  ) |>
  dplyr::mutate(
    triaged = start + cumsum(dplyr::lag(duration + gap, default = 0)),
    .by = c(attrb, dat)
  ) |>
  dplyr::filter(triaged <= DATA_END) |>
  dplyr::mutate(
    disd = triaged + duration,
    disd = dplyr::if_else(disd > DATA_END, as.Date(NA), disd),
    disrsn = dplyr::case_when(
      is.na(disd) ~ NA_real_,
      duration < 84 ~ draw_shares(
        parameters$discharge_reason_early, dplyr::n()
      ),
      TRUE ~ draw_shares(parameters$discharge_reason_late, dplyr::n())
    ),
    split = triaged + round(duration * stats::runif(dplyr::n(), 0.3, 0.7)),
    two_episodes = stats::runif(dplyr::n()) < TWO_EPISODE_SHARE &
      duration > 60 & split < DATA_END
  ) |>
  dplyr::left_join(people, by = "attrb")

# Episodes ----
# Most journeys are one episode. The rest are a transfer between two of the
# local authority's agencies: the first episode ends with discharge reason
# 74 (transferred, not in custody) and the second starts within 10 days, so
# jy.sequencing.R makes them one journey.

episodes <- dplyr::bind_rows(
  journeys |>
    dplyr::mutate(
      episode = 1,
      agency_index = sample(AGENCIES_PER_DAT, dplyr::n(), replace = TRUE),
      disd = dplyr::if_else(two_episodes, split, disd),
      disrsn = dplyr::if_else(two_episodes, 74, disrsn)
    ),
  journeys |>
    dplyr::filter(two_episodes) |>
    dplyr::mutate(
      episode = 2,
      triaged = split + sample(0:10, dplyr::n(), replace = TRUE)
    )
) |>
  dplyr::arrange(attrb, dat, triaged, episode) |>
  dplyr::mutate(
    agency_index = dplyr::if_else(
      episode == 2,
      dplyr::lag(agency_index) %% AGENCIES_PER_DAT + 1L,
      agency_index
    ),
    agncy = paste0("AG", dat, agency_index),
    attrbdat = paste0(attrb, dat),
    end = dplyr::coalesce(disd, DATA_END),
    # The OST the episode's prescribing SIRs record; a LAB client who
    # switches has oral OST before switch_date
    episode_ost = dplyr::if_else(
      ost == "lab" & triaged < LAB_START, lab_first_oral, ost
    ),
    switch_date = dplyr::if_else(
      episode_ost == "lab" &
        stats::runif(dplyr::n()) < LAB_SWITCH_SHARE,
      triaged + sample(30:400, dplyr::n(), replace = TRUE),
      as.Date(NA)
    ),
    triage_only = stats::runif(dplyr::n()) < TRIAGE_ONLY_SHARE
  )

# Modalities ----
# Opiate clients mostly have prescribing (codes 2 and 3 before April 2019,
# 94 after) and often psychosocial (95); others mostly psychosocial. The
# modality starts after v5's waiting time and ends with the episode.

prescribing_share <- dplyr::case_when(
  episodes$opiate & episodes$ost != "none" ~ 0.9,
  episodes$opiate ~ 0.3,
  TRUE ~ 0.08
)
psychosocial_share <- ifelse(episodes$opiate, 0.6, 0.9)

modality_draws <- dplyr::bind_rows(
  episodes |>
    dplyr::filter(stats::runif(dplyr::n()) < prescribing_share) |>
    dplyr::mutate(modal = dplyr::if_else(
      triaged < as.Date("2019-04-01"),
      sample(c(2, 3), dplyr::n(), replace = TRUE, prob = c(0.75, 0.25)),
      94
    )),
  episodes |>
    dplyr::filter(stats::runif(dplyr::n()) < psychosocial_share) |>
    dplyr::mutate(modal = 95),
  episodes |>
    dplyr::filter(stats::runif(dplyr::n()) < 0.08) |>
    dplyr::mutate(modal = sample(c(12, 4, 73), dplyr::n(), replace = TRUE))
) |>
  dplyr::filter(!triage_only)

# An episode with no modality (triage only, or none drawn) is one row with
# no modality
modalities <- modality_draws |>
  dplyr::bind_rows(
    episodes |>
      dplyr::anti_join(
        modality_draws,
        by = c("attrbdat", "agncy", "triaged")
      ) |>
      dplyr::mutate(modal = NA_real_)
  ) |>
  dplyr::mutate(
    modst = triaged + round(draw_percentiles(parameters$wait, dplyr::n())) +
      dplyr::if_else(
        modal %in% 95, sample(0:14, dplyr::n(), replace = TRUE), 0
      ),
    modst = dplyr::if_else(is.na(disd), modst, pmin(modst, disd)),
    modst = dplyr::if_else(is.na(modal), as.Date(NA), modst),
    modend = dplyr::if_else(
      is.na(disd) | is.na(modal),
      as.Date(NA),
      pmax(disd - sample(0:3, dplyr::n(), replace = TRUE), modst)
    )
  )

# Fields recorded at triage, the same on each of an episode's rows. Drug
# codes are from lookups/k.substance_groups.csv: 1101 heroin, 1105
# methadone (illicit), 1204 buprenorphine, 3200 cocaine, 3201 crack, 2200
# benzodiazepines, 7000 alcohol, 5000 cannabis.
n_episodes <- nrow(episodes)
episode_fields <- episodes |>
  dplyr::transmute(
    attrbdat, agncy, triaged,
    refld = triaged - sample(0:14, n_episodes, replace = TRUE),
    drug1 = dplyr::case_when(
      opiate ~ sample(
        c(1101, 1105, 1204), n_episodes,
        replace = TRUE, prob = c(0.9, 0.06, 0.04)
      ),
      cocaine ~ 3200,
      cannabis ~ 5000,
      crack ~ 3201,
      TRUE ~ 7000
    ),
    drug2 = dplyr::case_when(
      opiate & crack ~ 3201,
      drug1 != 7000 & alcohol ~ 7000,
      TRUE ~ NA_real_
    ),
    drug3 = dplyr::case_when(
      benzo ~ 2200,
      opiate & cannabis ~ 5000,
      TRUE ~ NA_real_
    ),
    drug1age = pmax(10, floor(age_first - stats::runif(n_episodes, 1, 15))),
    ethnic = sample(
      c("A", "B", "C", "D", "J", "M", "N", "S", "Z"), n_episodes,
      replace = TRUE,
      prob = c(0.8, 0.02, 0.05, 0.02, 0.02, 0.02, 0.02, 0.02, 0.03)
    ),
    empstat = sample(c(1:6, 99), n_episodes, replace = TRUE),
    parent = sample(c(11:15, 99), n_episodes, replace = TRUE),
    dualdiag = sample(
      c("Y", "N", NA), n_episodes,
      replace = TRUE, prob = c(0.3, 0.6, 0.1)
    ),
    mthtn = sample(
      c(1, 2, 99), n_episodes,
      replace = TRUE, prob = c(0.4, 0.5, 0.1)
    ),
    crtmhn = dplyr::if_else(
      mthtn == 1, sample(c(1:6, 99), n_episodes, replace = TRUE), NA_real_
    ),
    crtmhn2 = dplyr::if_else(
      mthtn == 1 & stats::runif(n_episodes) < 0.2,
      sample(1:5, n_episodes, replace = TRUE),
      NA_real_
    ),
    crtmhn3 = NA_real_,
    housing = draw_shares(parameters$housing, n_episodes),
    homeless = sample(
      c("Y", "N"), n_episodes,
      replace = TRUE, prob = c(0.08, 0.92)
    ),
    hivtestdt = as.Date(NA),
    modset = 0,
    adltprov = "Y",
    ypprov = "N"
  )

modality <- modalities |>
  dplyr::select(
    attrb, dat, attrbdat, agncy, sex, dob, triaged, disd, disrsn,
    modal, modst, modend
  ) |>
  dplyr::left_join(episode_fields, by = c("attrbdat", "agncy", "triaged")) |>
  dplyr::mutate(refmoddt = modst - sample(0:7, dplyr::n(), replace = TRUE))

# Records through an episode ----

#' Dates every REVIEW_INTERVAL days through each episode
#'
#' @param df Episodes with `from` (the date reviews count from) and `end`.
#' @param jitter Integer vector of day offsets to sample from.
#' @return `df` with one row per date: `review` (1 = the first after
#'   `from`) and `date`, never after `end`.
schedule_reviews <- function(df, jitter) {
  df |>
    dplyr::mutate(n_reviews = pmax(
      0, floor(as.numeric(end - from) / REVIEW_INTERVAL)
    )) |>
    expand_rows("n_reviews", "review") |>
    dplyr::mutate(
      date = from + review * REVIEW_INTERVAL +
        sample(jitter, dplyr::n(), replace = TRUE),
      date = pmin(date, end)
    )
}

treated <- modalities |>
  dplyr::filter(!is.na(modst)) |>
  dplyr::summarise(
    modst = min(modst),
    prescribing = any(modal %in% c(2, 3, 94)),
    psychosocial = any(modal %in% 95),
    .by = c(attrbdat, agncy, triaged)
  ) |>
  dplyr::left_join(episodes, by = c("attrbdat", "agncy", "triaged"))

# TOP ----
# One at the start (around the modality start), at most one in each six
# months after it, and one at exit, each with the probability set above.
# Days of use of the person's own substances start at their use_level and
# fall at each TOP by their responder factor; other substances are 0. The
# yes/no items (acuthpbm, the acute housing problem, and the rest) are
# 1 = Yes, 2 = No; the six housing items (ush*) exist from April 2024.

top_dates <- dplyr::bind_rows(
  treated |>
    dplyr::filter(stats::runif(dplyr::n()) < TOP_BASE) |>
    dplyr::mutate(
      review = 0,
      date = pmax(modst + sample(-7:14, dplyr::n(), replace = TRUE), triaged)
    ),
  treated |>
    dplyr::mutate(from = modst) |>
    schedule_reviews(-30:30) |>
    dplyr::filter(stats::runif(dplyr::n()) < TOP_REVIEW),
  treated |>
    dplyr::filter(
      !is.na(disd),
      stats::runif(dplyr::n()) <
        ifelse(disrsn %in% c(80, 82), TOP_EXIT_COMPLETED, TOP_EXIT_OTHER)
    ) |>
    dplyr::mutate(
      review = floor(as.numeric(disd - modst) / REVIEW_INTERVAL) + 1,
      date = pmax(disd - sample(0:14, dplyr::n(), replace = TRUE), modst)
    )
) |>
  dplyr::filter(date <= DATA_END) |>
  dplyr::distinct(attrbdat, agncy, triaged, date, .keep_all = TRUE)

#' Days of use in the last 28 days at a TOP
#'
#' @param uses Logical, whether the person uses the substance.
#' @param use_level Numeric, the person's days of use at the first TOP.
#' @param responder Numeric, the factor use falls by at each TOP.
#' @param review Integer, the TOP's number in the episode (0 = start).
#' @return Numeric days, 0 to 28; 0 for non-users.
days_of_use <- function(uses, use_level, responder, review) {
  noise <- sample(-2:2, length(uses), replace = TRUE)
  days <- round(use_level * responder^review + noise)
  ifelse(uses, pmin(pmax(days, 0), 28), 0)
}

n_tops <- nrow(top_dates)
top <- top_dates |>
  dplyr::transmute(
    attrb, dat, agncy, triaged, attrbdat,
    topdate = date,
    top = 1, aor = 0, ypor = 0,
    opiuse = days_of_use(opiate, use_level, responder, review),
    crause = days_of_use(crack, use_level, responder, review),
    alcuse = days_of_use(alcohol, use_level, responder, review),
    cocause = days_of_use(cocaine, use_level, responder, review),
    cannuse = days_of_use(cannabis, use_level, responder, review),
    otdrguse = days_of_use(benzo, use_level, responder, review),
    amphuse = 0,
    ivdrguse = days_of_use(injector, use_level, responder, review),
    sharing = dplyr::if_else(injector, yes_no(n_tops, 0.1), NA_real_),
    consmp = dplyr::if_else(alcuse > 0, alcuse * 12, NA_real_),
    acuthpbm = yes_no(n_tops, 0.1),
    unsthse = yes_no(n_tops, 0.1),
    hrisk = yes_no(n_tops, 0.05),
    psyhstat = sample(0:20, n_tops, replace = TRUE),
    phstat = sample(0:20, n_tops, replace = TRUE),
    quallife = sample(0:20, n_tops, replace = TRUE),
    pwork = sample(c(0, 0, 0, 0, 5, 20), n_tops, replace = TRUE),
    educat = sample(c(0, 0, 0, 0, 0, 10), n_tops, replace = TRUE),
    canvapuse = dplyr::if_else(topdate >= USH_START, 0, NA_real_)
  )

USH_FIELDS <- c(
  "ushcond", "ushunsafe", "ushunsuit", "ushaff", "ushover", "ushneeds"
)
for (field in USH_FIELDS) {
  top[[field]] <- dplyr::if_else(
    top$topdate >= USH_START, yes_no(n_tops, 0.03), NA_real_
  )
}

# Fields of the young people's form (YPOR) and others that the standard
# modules expect as columns: all missing for adults
YPOR_FIELDS <- c(
  "tobuse", "shotheft", "drgsell", "oththeft", "assault", "causpd", "toanic",
  "ecstsyyp", "solvyp", "ketamnyp", "ghbyp", "lehigsyp", "tranyp", "othr2yp",
  "othr3yp", "afucan", "afualc", "afutobn", "afuool", "afucrack", "afucoc",
  "afuest", "afuamp", "afuslv", "afuket", "afughb", "afulhu", "afutql",
  "aausfwk", "linstus", "ypivdrguse", "ptedawdd", "ptedawde", "ptedawed",
  "ptedawee", "ptedoyo", "pteuswdd", "pteuswde", "pteuswed", "pteuswee",
  "pteusoyo", "lisredys", "ansts", "hapstys", "ratefam", "ratefri",
  "volnpwork", "daysvoln", "bingedrink"
)
top[YPOR_FIELDS] <- NA_real_

# CIR ----
# Reviews at most every six months through a treated episode. Housing is
# from v5's distribution; mental health treatment need (cirmthtn, 1 = Yes)
# and treatment received (circrtmhn) as on the modality record.

cir_dates <- treated |>
  dplyr::mutate(from = triaged) |>
  schedule_reviews(-20:20) |>
  dplyr::filter(stats::runif(dplyr::n()) < CIR_REVIEW, date <= DATA_END)

n_cirs <- nrow(cir_dates)
cir <- cir_dates |>
  dplyr::transmute(
    attrb, dat, agncy, triaged,
    cirdt = date,
    cirhousing = draw_shares(
      dplyr::filter(parameters$housing, !is.na(value)), n_cirs
    ),
    cirhomeless = sample(
      c("Y", "N"), n_cirs,
      replace = TRUE, prob = c(0.06, 0.94)
    ),
    cirmthtn = sample(
      c(1, 2, 99), n_cirs,
      replace = TRUE, prob = c(0.4, 0.5, 0.1)
    ),
    circrtmhn = dplyr::if_else(
      cirmthtn == 1, sample(c(1:6, 99), n_cirs, replace = TRUE), NA_real_
    ),
    circrtmhn2 = NA_real_,
    circrtmhn3 = NA_real_,
    circldwt = NA_real_,
    cirprtst = NA_real_,
    cirhepctd = NA_real_,
    cirhepbstat = NA_character_,
    cirhepbvac = NA_character_,
    cirhepcstat = NA_character_,
    cirhivtestdt = as.Date(NA)
  )

# SIR ----
# Prescribing SIRs from the first after the prescribing start, sooner for
# oral OST than for LAB (which is recorded later: a median of about 84 days
# in v5), then about every six months, none before April 2017. Each records
# the OST the episode is on at the time; prescribing for an opiate client
# with no OST type records other medication. Psychosocial episodes get one
# or two psychosocial SIRs. Field names are the standard ones (`new` in
# lookups/k.subinterventions.csv, plus s.ph.buprinj_opi, s.ph.bupr_opi,
# s.ph.meth_opi, s.psy.dayprog and s.ph.intent, which 05_build_sir.R reads
# but the reference lookup doesn't name), with 1 = Yes and 0 = No as
# init.submodality.R makes them.

lookup <- data.table::fread(
  file.path(STANDARD_MODULES, "lookups/k.subinterventions.csv")
)
SIR_VALUES <- c("s.ph.superv", "s.ph.dose.meth_opi", "s.ph.dose.bupr_opi")
SIR_FLAGS <- c(
  setdiff(grep("^s\\.(ph|psy|yp)\\.", lookup$new, value = TRUE), SIR_VALUES),
  "s.ph.buprinj_opi", "s.ph.bupr_opi", "s.ph.meth_opi", "s.psy.dayprog"
)
OST_FLAGS <- list(
  methadone = c(
    "s.ph.meth_opi_main", "s.ph.meth_opi_stbl", "s.ph.su.meth_opi",
    "s.ph.us.meth_opi"
  ),
  oral_buprenorphine = c(
    "s.ph.bupr_opi_main", "s.ph.buprwn_opi_main", "s.ph.su.bupr_opi",
    "s.ph.us.bupr_opi"
  ),
  lab = c(
    "s.ph.buprinj_opi_main", "s.ph.buprinj_opi_main",
    "s.ph.buprinj_opi_with"
  ),
  none = c("s.ph.oth", "s.ph.stbl")
)
PSYCHOSOCIAL_FLAGS <- c(
  "s.psy.mi", "s.psy.behav", "s.psy.counsel", "s.psy.family"
)

prescribing_sirs <- treated |>
  dplyr::filter(prescribing) |>
  dplyr::mutate(
    first_sir = modst + round(stats::rexp(
      dplyr::n(),
      rate = ifelse(episode_ost == "lab" & is.na(switch_date), 1 / 121, 1 / 20)
    )),
    first_sir = pmax(first_sir, SIR_START),
    n_sirs = pmax(
      0, floor(as.numeric(end - first_sir) / REVIEW_INTERVAL) + 1
    )
  ) |>
  expand_rows("n_sirs", "sir") |>
  dplyr::mutate(
    submoddt = first_sir + (sir - 1) * REVIEW_INTERVAL +
      dplyr::if_else(sir > 1, sample(-20:20, dplyr::n(), replace = TRUE), 0),
    submoddt = pmin(submoddt, end),
    drug = dplyr::if_else(
      !is.na(switch_date) & submoddt < switch_date, lab_first_oral, episode_ost
    )
  ) |>
  dplyr::filter(submoddt <= DATA_END)

prescribing_sirs <- prescribing_sirs |>
  dplyr::mutate(
    flag = vapply(
      drug, \(d) sample(OST_FLAGS[[d]], 1), character(1),
      USE.NAMES = FALSE
    ),
    s.ph.superv = dplyr::if_else(
      drug %in% c("methadone", "oral_buprenorphine"),
      yes_no(dplyr::n(), 0.6), NA_real_
    ),
    s.ph.dose.meth_opi = dplyr::if_else(
      drug == "methadone",
      sample(seq(20, 120, 5), dplyr::n(), replace = TRUE), NA_real_
    ),
    s.ph.dose.bupr_opi = dplyr::if_else(
      drug == "oral_buprenorphine",
      sample(seq(2, 24, 2), dplyr::n(), replace = TRUE), NA_real_
    ),
    s.ph.intent = sample(c("A", "B", "C"), dplyr::n(), replace = TRUE)
  )

psychosocial_sirs <- treated |>
  dplyr::filter(psychosocial) |>
  dplyr::mutate(n_sirs = sample(1:2, dplyr::n(), replace = TRUE)) |>
  expand_rows("n_sirs", "sir") |>
  dplyr::mutate(
    submoddt = pmin(modst + 30 + (sir - 1) * 90, end),
    flag = sample(PSYCHOSOCIAL_FLAGS, dplyr::n(), replace = TRUE)
  ) |>
  dplyr::filter(submoddt >= SIR_START, submoddt <= DATA_END)

sir <- dplyr::bind_rows(prescribing_sirs, psychosocial_sirs) |>
  dplyr::select(
    attrb, attrbdat, dat, agncy, triaged, submoddt, flag,
    dplyr::any_of(c(SIR_VALUES, "s.ph.intent"))
  )
for (field in SIR_FLAGS) {
  sir[[field]] <- as.numeric(sir$flag == field)
}
sir$flag <- NULL

# Extracts by period ----

#' The history as it stood at a period end
#'
#' Records dated after `end` are dropped, and discharges and modality ends
#' after it blanked. A modality starting after `end` is dropped, and an
#' episode left without one keeps a single row with no modality.
#'
#' @param end Date, the period end.
#' @return List of `modality`, `top`, `cir` and `sir` data.tables, sorted as
#'   the init scripts sort them.
as_of <- function(end) {
  late_start <- !is.na(modality$modst) & modality$modst > end
  modality_end <- modality |>
    dplyr::mutate(
      modal = dplyr::if_else(late_start, NA_real_, modal),
      modst = dplyr::if_else(late_start, as.Date(NA), modst),
      refmoddt = dplyr::if_else(late_start, as.Date(NA), refmoddt),
      modend = dplyr::if_else(late_start | modend > end, as.Date(NA), modend),
      disrsn = dplyr::if_else(disd > end, NA_real_, disrsn),
      disd = dplyr::if_else(disd > end, as.Date(NA), disd)
    ) |>
    dplyr::filter(triaged <= end) |>
    dplyr::filter(
      !is.na(modst) | !any(!is.na(modst)),
      .by = c(attrbdat, agncy, triaged)
    ) |>
    dplyr::distinct()
  list(
    modality = data.table::as.data.table(modality_end)[
      order(attrb, dat, triaged, modst, modend)
    ],
    top = data.table::as.data.table(dplyr::filter(top, topdate <= end))[
      order(attrb, dat, triaged, topdate)
    ],
    cir = data.table::as.data.table(dplyr::filter(cir, cirdt <= end))[
      order(attrb, dat, triaged, cirdt)
    ],
    sir = data.table::as.data.table(dplyr::filter(sir, submoddt <= end))
  )
}

#' Financial year of a period, as init.globals.R writes it ("2025-26")
#'
#' @param period Integer vector (month, year).
#' @return Character.
financial_year <- function(period) {
  start_year <- if (period[1] < 4) period[2] - 1 else period[2]
  paste(start_year, substr(start_year + 1, 3, 4), sep = "-")
}

for (period in PERIODS) {
  end <- lubridate::ceiling_date(
    as.Date(sprintf("%d-%02d-01", period[2], period[1])), "month"
  ) - 1
  folder <- file.path(
    Q_DRIVE, "Data/Datasets", sprintf("%d-%02d", period[2], period[1]),
    "Z-verified files"
  )
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  extract <- as_of(end)
  saveRDS(extract$modality, file.path(folder, "z_verified_modality.rds"))
  saveRDS(extract$top, file.path(folder, "z_verified_top.rds"))
  saveRDS(extract$cir, file.path(folder, "z_verified_cir.rds"))
  saveRDS(extract$sir, file.path(folder, "z_verified_submodality_str.rds"))

  agency_folder <- file.path(
    Q_DRIVE, "Resources/Agency Tables", financial_year(period)
  )
  dir.create(agency_folder, recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(
    agencies |>
      dplyr::transmute(
        agency_code = agncy, setting = 1, datedeactive = as.Date(NA)
      ),
    file.path(
      agency_folder,
      paste0("Agency List ", month.name[period[1]], " ", period[2], ".xlsx")
    )
  )
  message(
    "Period ", format(end), ": ", nrow(extract$modality), " modality rows, ",
    nrow(extract$top), " TOPs, ", nrow(extract$cir), " CIRs, ",
    nrow(extract$sir), " SIRs"
  )
}

# Standard modules and lookups ----
# Links, made once; the two lookups the reference copy lacks are rewritten
# each run.

#' Make a relative symbolic link unless the path already exists
#'
#' @param target Character, the link's target, relative to the link's folder.
#' @param path Character, the link to make.
#' @return Invisible NULL.
link <- function(target, path) {
  if (!file.exists(path)) file.symlink(target, path)
  invisible(NULL)
}

link("../../R_standard_modules", file.path(Q_DRIVE, "R_standard_modules"))
periodic <- file.path(Q_DRIVE, "R_standard_modules_periodic")
dir.create(file.path(periodic, "lookups"),
  recursive = TRUE,
  showWarnings = FALSE
)
for (entry in setdiff(list.files(STANDARD_MODULES), "lookups")) {
  link(
    file.path("../../../R_standard_modules", entry),
    file.path(periodic, entry)
  )
}
for (entry in list.files(file.path(STANDARD_MODULES, "lookups"))) {
  link(
    file.path("../../../../R_standard_modules/lookups", entry),
    file.path(periodic, "lookups", entry)
  )
}
data.table::fwrite(
  parameters$dat_lookup, file.path(periodic, "lookups/m.dat.csv")
)
data.table::fwrite(
  parameters$housing_lookup, file.path(periodic, "lookups/k.housing.csv")
)
link("../synthetic/Q:", "build/Q:")
message("Wrote ", Q_DRIVE)
