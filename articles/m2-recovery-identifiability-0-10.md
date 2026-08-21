# M2 Recovery and Identifiability

## Why recovery precedes promotion

A multimodal model is not validated because it compiles or produces
finite estimates. The M2 evidence program therefore separates structural
support, numerical diagnostics, parameter recovery, uncertainty
calibration, and later empirical reproduction.

``` r

library(eyeprocess)
#> eyeprocess 0.11.0: vendor-neutral eye/process data harmonization with first-class Gazepoint support.

sim <- simulate_multimodal_m2(
  n_person = 100,
  n_item = 10,
  dropout = c(response = .02, rt = .05, gaze = .10),
  seed = 101
)

audit_multimodal_m2_identifiability(sim$data)
#> <eye_multimodal_m2_identifiability>
#>   model: M2
#>   persons: 100
#>   items: 10
#>   supported: TRUE
#>   missing fractions: response=0.017, rt=0.055, gaze=0.107
#>   boundary: This audit is a conservative structural/data-support screen. It does not establish global identifiability, construct validity, or robustness to MNAR channel missingness.
```

The simulation retains complete latent and item truth even after
observed-channel dropout is applied.

## Recovery experiment

[`multimodal_m2_recovery()`](https://stefanosbalaskas.github.io/eyeprocess/reference/multimodal_m2_recovery.md)
repeatedly simulates and fits the complete M2 estimator. It summarizes
bias, RMSE, posterior SD, and 95% interval coverage across person latent
parameters, item locations, item dispersions, covariance parameters, and
hyperparameters.

``` r

rec <- multimodal_m2_recovery(
  n_rep = 25,
  n_person = 150,
  n_item = 15,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  base_seed = 20261001
)

rec
plot(rec, type = "truth_vs_estimate")
plot(rec, type = "coverage")
```

A publication-grade recovery grid should vary sample size, item count,
latent correlations, item correlations, count dispersion, RT
discrimination, and channel dropout rather than relying on one favorable
condition.

## What recovery does not show

Recovery under the generating model establishes that the estimator can
recover parameters when its assumptions are true. It does not show
robustness to misspecified count distributions, local dependence, device
artifacts, nonignorable missingness, or construct validity. Those are
distinct validation layers.
