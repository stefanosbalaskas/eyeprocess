# Fit a standard 3PL and audit lower-asymptote alignment with process evidence

Fits a conventional 3PL response model with \`mirt\`, then aligns the
fitted lower-asymptote parameter with item-level gaze/pupil/RT
summaries. The alignment is descriptive and diagnostic. It must not be
interpreted as a confirmatory detector of guessing, rapid responding,
disengagement, or any other latent behavior without independent
validation.

## Usage

``` r
fit_gaze_anchored_3pl_audit(
  response_matrix,
  process_data = NULL,
  item = "item_id",
  process_features = c("ttff_ms", "dwell_ms", "pupil_bc", "pupil_peak", "rt_ms",
    "accuracy"),
  model = 1,
  SE = FALSE
)
```

## Arguments

- response_matrix:

  Person x item dichotomous response matrix.

- process_data:

  Optional trial/person-item process table.

- item:

  Item identifier in \`process_data\`.

- process_features:

  Candidate numeric process features.

- model:

  mirt model specification.

- SE:

  Request standard errors from \`mirt\`.

## Value

An \`eye_gaze_anchored_3pl_audit\` object.
