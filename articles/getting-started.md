# Getting Started with eyeprocess

`eyeprocess` harmonizes heterogeneous eye-tracking, pupil, event,
response, and biometric streams without erasing their source semantics.
The core object is a relational `eye_dataset`, not a single wide data
frame.

## Simulate a complete project

``` r

library(eyeprocess)

x <- simulate_eye_dataset(n_person = 20, n_item = 8, seed = 42)
x
summary(x)
validate_eye_dataset(x)
provenance_manifest(x)
```

## Standard workflow

``` r

spec <- preprocess_spec(
  gaze_filter = "median",
  pupil_interpolation = "linear",
  pupil_filter = "median",
  fixation_algorithm = "ivt"
)

x <- preprocess_eye(x, spec)
x <- build_aoi_visits(x)
x <- derive_all_features(x)

analysis_readiness(x)
feature_dictionary(x)
```

## Inspect and visualize

``` r

trial <- x$intervals$trial_id[1]
plot_eye_overview(x)
plot_scanpath(x, trial_id = trial)
plot_pupil_timeseries(x, trial_id = trial)
plot_transition_matrix(x)
```

## Persist the canonical representation

``` r

write_eye_dataset(x, "analysis/eye-dataset.rds")
export_canonical(x, "analysis/canonical-folder")
report_eye_dataset(x, "analysis/eyeprocess-report.md", include_plots = TRUE)
```
