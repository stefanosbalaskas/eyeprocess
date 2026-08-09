# Extract AOI growth-curve/trajectory features

Converts AOI occupancy over binned time into orthogonal-polynomial
trajectory coefficients. Coefficients summarize temporal shape and are
not latent psychological traits by themselves.

## Usage

``` r
aoi_trajectory_features(
  data,
  person = "person_id",
  trial = "trial_id",
  time = "time_ms",
  aoi = "aoi",
  bin_ms = 100,
  degree = 3L,
  aois = NULL
)
```

## Arguments

- data:

  Sample-level data.

- person, trial, time, aoi:

  Column names.

- bin_ms:

  Temporal bin width.

- degree:

  Polynomial degree.

- aois:

  Optional AOIs to encode; defaults to observed AOIs.
