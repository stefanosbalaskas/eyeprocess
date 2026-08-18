# Run M3 parameter-recovery and stress evidence

Repeatedly simulates and fits M3 across pupil-signal and
pupil-missingness scenarios, summarizing bias, RMSE, posterior SD,
interval coverage and MCMC diagnostics. Small \`reps\` values are smoke
tests; scientific promotion requires a predeclared larger grid.

## Usage

``` r
multimodal_m3_recovery(
  reps = 3L,
  pupil_signal = c("informative", "weak", "null", "redundant", "confounded"),
  pupil_missingness = c("mcar", "quality", "device"),
  n_person = 80L,
  n_item = 10L,
  seed = 20260815L,
  fit_args = list(chains = 2L, parallel_chains = 2L, iter_warmup = 500L, iter_sampling =
    300L, refresh = 0L, init = 0)
)
```

## Arguments

- reps:

  Replications per design cell.

- pupil_signal, pupil_missingness:

  Scenario vectors.

- n_person, n_item:

  Design size.

- seed:

  Base seed.

- fit_args:

  Named sampling arguments for \`fit_multimodal_m3()\`.

## Value

An \`eye_multimodal_m3_recovery\` object.
