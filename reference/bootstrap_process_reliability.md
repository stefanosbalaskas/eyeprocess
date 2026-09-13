# Bootstrap ICC reliability by resampling participants

Bootstrap ICC reliability by resampling participants

## Usage

``` r
bootstrap_process_reliability(
  data,
  person,
  session,
  measure,
  replications = 500L,
  seed = 1L
)
```

## Arguments

- data:

  Long data.

- person, session, measure:

  Column names.

- replications:

  Bootstrap replications.

- seed:

  Seed.

## Value

A data frame containing bootstrap ICC reliability by resampling
participants. Rows represent the analysis units and columns contain the
identifiers, estimates, or diagnostics defined by the function.
