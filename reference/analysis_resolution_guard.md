# Audit compatibility between measurement resolution and an analysis target

Audit compatibility between measurement resolution and an analysis
target

## Usage

``` r
analysis_resolution_guard(
  event_duration_ms,
  effective_hz,
  spatial_feature_size = NA_real_,
  radial_error = NA_real_,
  min_samples = 3,
  max_error_fraction = 0.5
)
```

## Arguments

- event_duration_ms:

  Smallest event duration the analysis intends to resolve.

- effective_hz:

  Empirical sampling frequency.

- spatial_feature_size:

  Optional smallest spatial feature/AOI dimension in coordinate units.

- radial_error:

  Optional empirical radial error in the same spatial units.

- min_samples:

  User-declared minimum samples per temporal feature.

- max_error_fraction:

  User-declared maximum spatial-error / feature-size ratio.
