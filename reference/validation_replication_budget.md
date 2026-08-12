# Compute a replication budget from a target MCSE

Compute a replication budget from a target MCSE

## Usage

``` r
validation_replication_budget(
  pilot_sd,
  target_mcse,
  minimum = 20L,
  maximum = 10000L
)
```

## Arguments

- pilot_sd:

  Pilot estimate of the metric standard deviation.

- target_mcse:

  Target Monte Carlo standard error.

- minimum:

  Minimum permitted replication count.

- maximum:

  Maximum permitted replication count.
