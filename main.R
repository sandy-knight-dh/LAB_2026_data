# Pipeline --------------------------------------------------------------------
# Builds whatever is out of date in the targets pipeline (_targets.R): the
# summary indicators, the matches and both decks,
# output/<YYYY_mm_dd>_lab_summary_data.pptx and
# output/<YYYY_mm_dd>_lab_matched_summary_data.pptx, each with a .rds and
# .xlsx of the same name beside it. tar_make() reports each target it runs or
# skips.

targets::tar_make()
