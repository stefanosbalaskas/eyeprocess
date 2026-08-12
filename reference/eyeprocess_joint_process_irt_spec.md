# Declare a response/process joint IRT specification

Declare a response/process joint IRT specification

## Usage

``` r
eyeprocess_joint_process_irt_spec(
  response_family = c("2pl", "rasch", "grm", "gpcm"),
  time_model = c("none", "lognormal", "custom"),
  process_channels = c("dwell", "pupil", "transitions"),
  person_covariates = character(),
  item_covariates = character(),
  missingness = c("ignorable", "modeled", "gated"),
  status = c("experimental", "reference", "gated")
)
```

## Arguments

- response_family:

  Response-model family.

- time_model:

  Response-time model specification.

- process_channels:

  Declared process-measure channels.

- person_covariates:

  Person-level covariates.

- item_covariates:

  Item-level covariates.

- missingness:

  Missing-data handling or missingness specification.

- status:

  Evidence, model, or governance status.
