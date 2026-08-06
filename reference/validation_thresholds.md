# Specify completion and scientific-promotion thresholds

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
validation_thresholds(required_replications = 100L, max_failure_rate = 0.05,
  max_absolute_bias = 0.10, max_rmse = Inf, min_coverage = 0.90, max_coverage = 0.99,
  max_rhat = 1.01, min_ess_bulk = 400, max_divergence_rate = 0.01, require_sbc = TRUE,
  require_empirical_reproduction = TRUE)
```

## Arguments

- required_replications:

  Required completed replications per scenario.

- max_failure_rate:

  Maximum tolerated job failure rate.

- max_absolute_bias:

  Maximum absolute mean bias.

- max_rmse:

  Maximum RMSE.

- min_coverage:

  Minimum interval coverage.

- max_coverage:

  Maximum interval coverage.

- max_rhat:

  Maximum acceptable R-hat.

- min_ess_bulk:

  Minimum bulk effective sample size.

- max_divergence_rate:

  Maximum divergence rate.

- require_sbc:

  Require passing SBC uniformity evidence.

- require_empirical_reproduction:

  Require empirical-reproduction evidence.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
