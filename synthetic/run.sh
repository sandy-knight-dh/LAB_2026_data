#!/usr/bin/env bash
# The whole chain on synthetic data, from the repo root:
#   synthetic/run.sh
# 1. parameters.R: aggregates from data/final_linked_dataset_v5.feather
# 2. generate.R: the synthetic Q: drive (synthetic/Q:, linked as build/Q:)
# 3. standard_process.R: the standard journey scripts and modules on it
# 4. build/master.R: the dataset build, as in the secure environment
# 5. the analysis pipeline of branch standard-modules-v6, exported to
#    synthetic/pipeline/ and run there on the build's two feathers, so that
#    this checkout's data/, output/ and _targets/ are never touched
# Takes about 20 minutes. Pass --from-build to start at step 4, or
# --pipeline-only to rerun step 5.
set -euo pipefail  # pipefail: a failing build fails the tee'd step too
cd "$(dirname "$0")/.."

step="${1:-all}"

if [[ "$step" == "all" ]]; then
  Rscript synthetic/R/parameters.R
  Rscript synthetic/R/generate.R
  (cd synthetic && Rscript R/standard_process.R)
fi

if [[ "$step" == "all" || "$step" == "--from-build" ]]; then
  (cd build && Rscript master.R 2>&1 | tee ../synthetic/build.log)
  mkdir -p synthetic/data
  cp "$(ls -t build/outputs/extracts/final_linked_dataset_v6_t*.feather | head -1)" \
    synthetic/data/final_linked_dataset_v6.feather
  cp "$(ls -t build/outputs/extracts/standard_by_year_v6_t*.feather | head -1)" \
    synthetic/data/standard_by_year_v6.feather
fi

rm -rf synthetic/pipeline
mkdir -p synthetic/pipeline/data
git archive standard-modules-v6 | tar -x -C synthetic/pipeline
for f in final_linked_dataset_v6.feather standard_by_year_v6.feather; do
  ln -s "../../data/$f" "synthetic/pipeline/data/$f"
done
(cd synthetic/pipeline && OPENBLAS_NUM_THREADS=1 Rscript -e 'targets::tar_make()')
echo "Outputs in synthetic/pipeline/output/"
