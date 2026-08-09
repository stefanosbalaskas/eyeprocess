# Audit within-person pupil fatigue/trial-order drift

Audit within-person pupil fatigue/trial-order drift

## Usage

``` r
audit_pupil_fatigue_drift(
  data,
  pupil = "pupil_peak",
  trial_order = "trial_sequence",
  person = "person_id",
  luminance = NULL,
  difficulty = NULL,
  engine = c("auto", "plm", "lm_fixed_effects")
)
```

## Arguments

- data:

  Trial-level data.

- pupil, trial_order, person:

  Required columns.

- luminance, difficulty:

  Optional covariates.

- engine:

  \`auto\`, \`plm\`, or \`lm_fixed_effects\`.
