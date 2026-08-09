# Fit exploratory process profiles

Fit exploratory process profiles

## Usage

``` r
fit_process_profile_mixture(
  data,
  variables,
  k = 3L,
  id = "person_id",
  engine = c("auto", "tidyLPA", "kmeans_reference"),
  seed = 777
)
```

## Arguments

- data:

  Person-level process data.

- variables:

  Continuous process variables.

- k:

  Number of profiles.

- id:

  Optional person identifier.

- engine:

  \`auto\`, \`tidyLPA\`, or \`kmeans_reference\`.

- seed:

  Random seed.
