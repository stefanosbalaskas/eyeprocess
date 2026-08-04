args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else "eyeprocess-synthetic-benchmark"
library(eyeprocess)
set.seed(20260804)
dataset <- simulate_eye_dataset(
  n_person = 50,
  n_item = 20,
  sampling_rate = 60,
  trial_duration = 2,
  seed = 20260804
)
dataset <- derive_all_features(dataset)
path <- create_public_benchmark(
  dataset,
  output_dir,
  max_participants = 50,
  include_samples = TRUE,
  overwrite = TRUE
)
cat("Synthetic benchmark written to: ", path, "\n", sep = "")
