# Preprocessing, AOIs, and Feature Engineering

## Declare preprocessing

``` r

spec <- preprocess_spec(
  gaze_filter = "median",
  gaze_window = 5,
  pupil_interpolation = "linear",
  pupil_max_gap_ms = 150,
  pupil_filter = "median",
  pupil_window = 5,
  pupil_baseline = "subtract",
  fixation_algorithm = "ivt",
  fixation_parameters = list(velocity_threshold = 30)
)

x <- preprocess_eye(x, spec)
```

Vendor-produced episodes are retained separately from package-derived
episodes through `source_algorithm`, `source_parameters`, and
`derived_by`.

## Static and dynamic AOIs

``` r

x <- register_aois(
  x,
  new_aoi("prompt", x = 0.00, y = 0.00, width = 0.45, height = 1.00),
  new_aoi("options", x = 0.45, y = 0.00, width = 0.55, height = 1.00)
)

x <- assign_aois(x, component = "gaze_samples")
x <- build_aoi_visits(x)
```

A dynamic AOI is represented by one definition and multiple geometry
rows with explicit validity intervals or frame identifiers.

## Declared features

``` r

fspec <- feature_spec(
  level = "trial_aoi",
  include_post_response = FALSE,
  response_time = TRUE,
  biometrics = TRUE
)

x <- derive_all_features(x, fspec)
features_wide(x)
feature_dictionary(x)
```

## Sensitivity analysis

``` r

compare_preprocessing(x, list(raw = preprocess_spec(), filtered = spec))
compare_aoi_definitions(x, list(primary = primary_aois, alternative = alternative_aois))
check_process_leakage(x)
check_feature_level(x)
```
