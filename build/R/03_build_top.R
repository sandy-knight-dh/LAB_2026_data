# -------------------------------------------------------------------------
# Script: 03_build_top.R
#
# Purpose:
# Builds TOP dataset for LAB analysis. Includes filtering, harmonisation,
# and selection of relevant variables.
# -------------------------------------------------------------------------
#setup and load
source("R/01_setup.R")

#add in attrbdats
data_attrbdats <- readRDS(
  paste0("data/interim/data_attrbdats_v", ver, ".rds")
)

#load data
top <- readRDS(
  file.path(data_path, "Z-verified files", "z_verified_top.rds")
) %>%
  filter(topdate <= mon.end &
           (top == 1 | aor == 1 | ypor == 1))

# Processing
# YPOR to TOP conversions and unsuitable housing

# Housing data is requested, does this include unsuitable housing on TOP
# XXX check
# If so, need this standard processing from mod.top
if(mon.end > as.Date("2024-03-31")){
  top <- top %>% 
    rename(unsthse_pure = unsthse) %>% 
    # Recode NAs to high number so the == doesn't give NAs
    mutate(across(c("ushcond", "ushunsafe", "ushunsuit", "ushaff",
                    "ushover", "ushneeds"),
                  ~replace_na(.x,999))) %>% 
    mutate(unsthse = if_else((ushcond == 1 |
                                ushunsafe == 1 |
                                ushunsuit == 1 |
                                ushaff == 1 |
                                ushover == 1 |
                                ushneeds == 1),
                             1,
                             unsthse_pure)) %>% 
    # Recode NAs back
    mutate(across(c("ushcond", "ushunsafe", "ushunsuit", "ushaff",
                    "ushover", "ushneeds"),
                  ~if_else(.x == 999, NA_real_, .x)))
}

# Request includes all substance misuse fields. These are different on
# YPOR and TOP, so use this standard processing from mod.composite

# TOP and YPOR have slightly different fields; YPOR is broken into more
# categories. If dealing with a YPOR, need to convert to a TOP by using highest
# within category; can then remove YPOR fields.
# Not ideal as may represent changing substances but that's true on a TOP too.
top <- top %>%
  mutate(amphuse = pmax(.$amphuse, .$ecstsyyp, na.rm = TRUE),
         otdrguse = do.call(pmax,
                            c(across(any_of(c("otdrguse", "solvyp", "ketamnyp", "ghbyp",
                                              "lehigsyp", "tranyp", "othr2yp", "othr3yp",
                                              "nitoxiuse", "benzouse"))),
                              na.rm = TRUE))) %>%
  select(-any_of(c(
    "ecstsyyp", "solvyp", "ketamnyp", "ghbyp",
    "lehigsyp", "tranyp", "othr2yp", "othr3yp",
    "nitoxiuse", "benzouse")))
# Cannabis vape use was added to TOPs, need to take the max with cannabis use
top <- top %>%
  mutate(cannuse = pmax(cannuse, canvapuse, na.rm = TRUE))

# TOP records describe the client's current state rather than a specific
# treatment journey/provider episode. Do not assign n_jy in this build script.
# TOPs are retained at record level and summarised during final linkage.

# Requested fields
top <- top %>% 
  select(attrbdat, attrb, dat, agncy,
         triaged, topdate,
         # TOP classification, might be useful?
         top, aor, ypor,topdate,
         # Drugs and injecting
         # Note benzo use is specifically mentioned in the spec but is only
         # collected on YPORs
         # Skipping tobacco
         alcuse, consmp, opiuse, crause, cocause, cannuse, otdrguse,
         ivdrguse, 
         # Housing related, needed?
         acuthpbm, starts_with(c("ush","uns")), hrisk
  ) 

# Filtering
# Reduce to just TOPs for the relevant attrbdats
# Also remove some duplicates
top <- top %>% 
  filter(attrbdat %in% (data_attrbdats %>% select(attrbdat) %>% t())) %>% 
  unique()

# TOP output
# THIS WILL OUTPUT PII
# RDS file is suitable here
           
saveRDS(
  top,
  paste0("outputs/extracts/top_data_with_identifiers_v", ver, ".rds")
)