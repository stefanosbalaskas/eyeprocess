# Run validation jobs with checkpointing and deterministic seeds

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
run_validation_jobs(plan, simulator, fitter, extractor, truth_extractor, output_dir,
  workers = 1L, backend = c("auto", "sequential", "future"), isolation = c("auto",
  "in_process", "callr"), timeout_seconds = Inf, memory_limit_mb = Inf,
  stale_lock_seconds = 3600, overwrite = FALSE, fail_fast = FALSE,
  progress = interactive(), job_ids = NULL, chunks = NULL, simulation_args = list(),
  fit_args = list(), diagnostics_extractor = NULL, draws_extractor = NULL,
  predictions_extractor = NULL, confidence = 0.95, run_metadata = list())
```

## Arguments

- plan:

  Validation plan or manifest directory.

- simulator:

  Simulation function.

- fitter:

  Fitting function receiving the simulated object first.

- extractor:

  Function extracting parameter estimates.

- truth_extractor:

  Function extracting named true parameter values.

- output_dir:

  Validation output directory.

- workers:

  Number of workers.

- backend:

  Sequential or optional \`future\` backend.

- isolation:

  In-process execution or optional \`callr\` isolation.

- timeout_seconds:

  Per-job timeout. Enforced only with \`callr\` isolation.

- memory_limit_mb:

  Best-effort per-job memory limit.

- stale_lock_seconds:

  Age after which an abandoned job lock may be reclaimed.

- overwrite:

  Re-run completed checkpoints.

- fail_fast:

  Stop after the first failed/nonconverged job.

- progress:

  Display progress in sequential mode.

- job_ids:

  Optional subset of job identifiers.

- chunks:

  Optional subset of chunk identifiers.

- simulation_args:

  Additional simulator arguments.

- fit_args:

  Additional fitter arguments.

- diagnostics_extractor:

  Optional diagnostics extractor.

- draws_extractor:

  Optional posterior-draw extractor.

- predictions_extractor:

  Optional prediction extractor.

- confidence:

  Confidence level used when standard errors are supplied.

- run_metadata:

  Named metadata included in the runner fingerprint; use it to record
  code, prior, or engine variants captured outside function bodies.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
