# Inject timestamp jitter

Inject timestamp jitter

## Usage

``` r
inject_sampling_jitter(data, time = "timestamp_ms", sd, seed = 1L)
```

## Arguments

- data:

  Data.

- time:

  Timestamp column.

- sd:

  Jitter standard deviation in timestamp units.

- seed:

  Seed.

## Value

An R object containing inject timestamp jitter. The concrete class and
structure follow the selected method, engine, or input object and are
preserved as documented by that workflow.
