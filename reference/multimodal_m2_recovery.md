# Run repeated M2 estimator recovery

Repeatedly simulates from the M2 generating model, fits the M2 CmdStan
reference estimator, and computes bias, RMSE, posterior standard
deviation, and 95 dispersion, correlation, and hyperparameter families.

## Usage

``` r
multimodal_m2_recovery(
  n_rep = 10L,
  n_person = 100L,
  n_item = 10L,
  dropout = c(response = 0, rt = 0, gaze = 0),
  base_seed = 20260814L,
  chains = 4L,
  parallel_chains = chains,
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  prior_profile = c("regularized", "paper_centered"),
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
)
```

## Arguments

- n_rep:

  Number of simulation/fit replications.

- n_person, n_item:

  Simulation size.

- dropout:

  Channel dropout probabilities.

- base_seed:

  Base seed.

- chains, parallel_chains, iter_warmup, iter_sampling:

  CmdStan controls.

- prior_profile:

  Prior profile.

- adapt_delta, max_treedepth, refresh:

  CmdStan controls.

## Value

An \`eye_multimodal_m2_recovery\`.

## Details

This is intentionally computationally expensive and is not executed
during ordinary package tests.
