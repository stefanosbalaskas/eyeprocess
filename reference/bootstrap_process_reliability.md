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
