# Compute aDDM/GLAM-inspired gaze-evidence proxy features

Compute aDDM/GLAM-inspired gaze-evidence proxy features

## Usage

``` r
addm_glam_proxy_features(
  data,
  by = c("person_id", "trial_id"),
  time = "time_ms",
  aoi = "aoi",
  target_aoi = "target",
  distractor_aoi = "distractor",
  action_aoi = "button"
)
```

## Arguments

- data:

  Sample-level data.

- by:

  Grouping columns.

- time, aoi:

  Columns.

- target_aoi, distractor_aoi, action_aoi:

  AOI labels.
