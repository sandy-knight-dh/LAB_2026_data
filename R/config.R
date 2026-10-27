# The linked dataset, version 6, from the dataset build in build/
# (build/R/07_link_analysis_dataset.R writes
# final_linked_dataset_v6_t<timestamp>.feather; copy it here)
DATA_PATH <- "data/final_linked_dataset_v6.feather"
DATA_END <- as.Date("2026-03-31") # last triage and SIR date in the extract

# The Treatment Progress Measure and other standard-module measures for each
# client's latest journey in each financial year, from the standard monthly
# outputs (build/R/08_standard_by_year.R writes
# standard_by_year_v6_t<timestamp>.feather; copy it here)
STANDARD_BY_YEAR_PATH <- "data/standard_by_year_v6.feather"
