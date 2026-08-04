# Run the complete validation-release programme

Run the complete validation-release programme

## Usage

``` r
run_eyeprocess_validation_program(
  corpus,
  output_dir,
  model_jobs = list(),
  sbc_jobs = list(),
  engine_jobs = list(),
  reproduction_jobs = list(),
  grouped_jobs = list(),
  leakage_jobs = list(),
  multiverse_jobs = list(),
  benchmark_jobs = list(),
  reporting_dataset = NULL,
  public_benchmark_dataset = NULL,
  public_benchmark_include_samples = FALSE,
  advanced_evidence = list(),
  evidence_spec = advanced_model_evidence_spec(),
  overwrite = FALSE
)
```

## Arguments

- corpus:

  Validation corpus or manifest.

- output_dir:

  Output directory.

- model_jobs:

  Named list of model-validation job specifications. Each job supplies
  \`simulator\`, \`fitter\`, \`extractor\`, \`truth_extractor\`, and
  optional \`grid\` and \`spec\`.

- sbc_jobs:

  Named list of simulation-based-calibration job specifications.

- engine_jobs:

  Named list of equivalent-engine comparison jobs.

- reproduction_jobs:

  Named list of licensed empirical-reproduction jobs.

- grouped_jobs:

  Named list of grouped-validation jobs. Set \`crossed = TRUE\` in a job
  to call \`crossed_grouped_cv()\`.

- leakage_jobs:

  Named list of leakage-quantification jobs.

- multiverse_jobs:

  Named list of preprocessing-multiverse jobs.

- benchmark_jobs:

  Named list of zero-argument functions or benchmark argument lists.

- reporting_dataset:

  Optional \`eye_dataset\` for reporting-guideline coverage.

- public_benchmark_dataset:

  Optional \`eye_dataset\` from which to write a de-identified public
  benchmark bundle.

- public_benchmark_include_samples:

  Whether the public benchmark retains sample-level tables.

- advanced_evidence:

  Optional named evidence records keyed by model function.

- evidence_spec:

  Advanced-model promotion-gate specification.

- overwrite:

  Whether to replace the output directory.

## Value

An \`eye_validation_program\` object.
