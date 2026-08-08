# Run posterior simulation-based calibration from an explicit contract

Run posterior simulation-based calibration from an explicit contract

## Usage

``` r
run_posterior_sbc(
  observed_data,
  contract,
  replications = 100L,
  seed = 20260808L
)
```

## Arguments

- observed_data:

  Observed dataset used by the calibration procedure.

- contract:

  Posterior-SBC or validation contract.

- replications:

  Number of simulation or validation replications.

- seed:

  Random-number seed.
