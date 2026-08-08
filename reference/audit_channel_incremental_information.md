# Audit out-of-sample incremental information from a process channel

Audit out-of-sample incremental information from a process channel

## Usage

``` r
audit_channel_incremental_information(
  data,
  fold,
  baseline_fitter,
  process_fitter,
  predictor,
  scorer,
  higher_is_better = TRUE
)
```

## Arguments

- data:

  Input data.

- fold:

  Group/fold column. Every unique value is held out once.

- baseline_fitter:

  Function fitted without the process channel.

- process_fitter:

  Function fitted with the process channel.

- predictor:

  Function \`(fit, test)\` returning predictions.

- scorer:

  Function \`(test, prediction)\` returning a scalar score.

- higher_is_better:

  Direction of the score.
