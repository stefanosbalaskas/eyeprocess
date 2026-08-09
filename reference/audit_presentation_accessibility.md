# Audit presentation/accessibility sensitivity without clinical inference

Creates a transparent presentation-review score from response
time/dwell, revisit/entropy, pupil-effort proxies, and gaze quality. It
is an experimental design/fairness audit only.

## Usage

``` r
audit_presentation_accessibility(
  data,
  person = "person_id",
  rt = "rt_ms",
  dwell = "dwell_ms",
  revisits = "revisits",
  entropy = "aoi_entropy",
  pupil = "pupil_peak",
  gaze_validity = "valid_gaze_prop",
  review_quantile = 0.9
)
```

## Arguments

- data:

  Process data.

- person:

  Person identifier.

- rt, dwell, revisits, entropy, pupil, gaze_validity:

  Optional column names.

- review_quantile:

  Quantile for a presentation-review flag.
