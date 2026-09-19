# Standardized vendor-neutral gaze data-quality metrics

Compute and report separable eye-tracking quality dimensions:
target-referenced accuracy, RMS sample-to-sample precision, spatial
standard-deviation precision, BCEA, effective sampling behavior,
sampling jitter, valid-data fraction, and data loss. User thresholds
create review flags only and never trigger automatic exclusion.

## Usage

``` r
validate_gaze_quality_inputs(data, x = "gaze_x", y = "gaze_y", time = NULL,
  target_x = NULL, target_y = NULL, by = NULL, unit = "degrees",
  time_unit = "ms", unit_column = NULL)

compute_gaze_accuracy(data, x = "gaze_x", y = "gaze_y",
  target_x = "target_x", target_y = "target_y", by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL)

compute_rms_s2s(data, x = "gaze_x", y = "gaze_y", time = NULL, by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL,
  dimension = c("2d", "horizontal", "vertical"), time_unit = "ms",
  max_gap_ms = NULL)

compute_gaze_sd_precision(data, x = "gaze_x", y = "gaze_y", by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL)

compute_bcea(data, x = "gaze_x", y = "gaze_y", by = NULL,
  probability = 0.68, unit = "degrees", output_unit = NULL,
  geometry = NULL)

compute_gaze_precision(data, x = "gaze_x", y = "gaze_y", time = NULL,
  by = NULL, probability = 0.68, unit = "degrees", output_unit = NULL,
  geometry = NULL, dimension = "2d", time_unit = "ms", max_gap_ms = NULL)

estimate_sampling_interval(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms")

estimate_sampling_jitter(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms")

estimate_effective_sampling_rate(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms", nominal_sampling_hz = NULL,
  dropped_interval_factor = 1.5, x = NULL, y = NULL, valid = NULL)

compute_valid_sample_fraction(data, x = "gaze_x", y = "gaze_y",
  valid = NULL, by = NULL)

compute_gaze_data_loss(data, x = "gaze_x", y = "gaze_y",
  time = "timestamp_ms", valid = NULL, missing_reason = NULL,
  by = NULL, time_unit = "ms")

summarise_spatial_quality(data, x = "gaze_x", y = "gaze_y",
  target_x = "target_x", target_y = "target_y", time = NULL, by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL,
  dimension = "2d", time_unit = "ms", max_gap_ms = NULL,
  probability = 0.68)

summarise_sampling_quality(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms", nominal_sampling_hz = NULL,
  dropped_interval_factor = 1.5, x = NULL, y = NULL, valid = NULL)

create_gaze_quality_report(data, x = "gaze_x", y = "gaze_y",
  time = "timestamp_ms", target_x = "target_x", target_y = "target_y",
  valid = NULL, missing_reason = NULL, by = NULL, unit = "degrees",
  output_unit = NULL, geometry = NULL, time_unit = "ms",
  nominal_sampling_hz = NULL, bcea_probability = 0.68,
  max_gap_ms = NULL, thresholds = NULL, preprocessing_spec = NULL,
  event_detector = NULL, aoi_specification = NULL, quality_rules = NULL,
  model_specification = NULL, software_version = NULL,
  unit_column = NULL)

compare_gaze_quality_sessions(data, session = "session_id", by = NULL, ...)

compare_gaze_quality_conditions(data, condition = "condition", by = NULL, ...)

plot_gaze_accuracy(report, metric = "accuracy_mean", ...)
plot_gaze_precision(report, metric = "precision_rms_s2s", ...)
plot_bcea(report, ...)
plot_sampling_intervals(data, time = "timestamp_ms", time_unit = "ms", ...)
plot_gaze_quality_dashboard(report, ...)
report_gaze_quality(report, digits = 3L)

simulate_gaze_quality_calibration(seed = 20260918L,
  samples_per_target = 18L, nominal_sampling_hz = 60)
```

## Arguments

- data:

  A data frame or object coercible to a data frame.

- x, y:

  Columns containing horizontal and vertical gaze coordinates.

- time:

  Timestamp column.

- target_x, target_y:

  Known validation-target coordinate columns.

- by:

  Optional grouping columns defining the quality-analysis unit.

- unit:

  Input coordinate unit: `"degrees"`, `"pixels"`, or `"normalized"`.

- output_unit:

  Optional explicit output coordinate unit. No conversion occurs when
  omitted.

- geometry:

  Named screen/viewing geometry used only for explicit coordinate
  conversion.

- time_unit:

  Timestamp unit: seconds, milliseconds, microseconds, or nanoseconds.

- unit_column:

  Optional column used to reject mixed or conflicting coordinate units.
  For the canonical report, an existing `coordinate_unit` column is
  detected automatically when this argument is omitted.

- dimension:

  RMS-S2S dimension: two-dimensional, horizontal, or vertical.

- max_gap_ms:

  Optional explicit maximum interval permitted for an adjacent-sample
  RMS pair.

- probability, bcea_probability:

  Probability region used for BCEA.

- nominal_sampling_hz:

  Optional nominal hardware rate used for comparison and
  dropped-interval diagnostics.

- dropped_interval_factor:

  Multiple of the nominal interval used to count long intervals.

- valid:

  Optional explicit validity column. Missing gaze coordinates remain
  unavailable regardless of this flag.

- missing_reason:

  Optional loss-reason column such as blink, tracker invalidity, or
  off-screen.

- thresholds:

  Optional named study-specific review thresholds; these never exclude
  data automatically.

- preprocessing_spec, event_detector, aoi_specification, quality_rules,
  model_specification, software_version:

  Optional provenance fields preserved on the canonical report.

- session, condition:

  Grouping column used for convenience comparisons.

- report:

  A quality-report-like data frame.

- metric:

  Metric column plotted by a focused quality plot.

- digits:

  Number of digits in the compact reporting string.

- seed:

  Deterministic synthetic-data seed.

- samples_per_target:

  Synthetic samples generated at each of nine validation targets.

- ...:

  Additional arguments forwarded to the canonical report or base
  plotting function.

## Details

Accuracy is target-referenced error. RMS-S2S precision quantifies
adjacent-sample fluctuation and never bridges a missing sample. Spatial
SD and BCEA quantify point-cloud spread around a stable target and use
population standard deviations (denominator \\n\\). BCEA always records
its probability level.

Effective sampling frequency is estimated from the effective sample
count divided by an estimated recording duration (timestamp span plus
one median positive inter-sample interval). When gaze coordinates and
validity are supplied, the effective sample count contains valid gaze
observations with finite timestamps; otherwise the metric describes the
timestamp stream. With a nominal rate, `long_interval_count` counts
intervals exceeding the declared nominal-gap multiple, whereas
`dropped_interval_count` estimates the number of missing nominal samples
represented by those long intervals.

The package deliberately does not impose universal acceptability
cutoffs. A `gaze_quality_report` stores review flags and provenance
while preserving all analysis units for downstream filtering, weighting,
stratification, sensitivity analysis, and transparent reporting.

## Value

Metric functions return data frames with explicit units and diagnostics.
`create_gaze_quality_report()` returns a data frame of class
`gaze_quality_report` with a `gaze_quality_provenance` attribute. Plot
functions draw base-R diagnostics and invisibly return the plotted data.
`report_gaze_quality()` returns a compact character summary.
`simulate_gaze_quality_calibration()` returns a deterministic synthetic
nine-point validation data set spanning six quality profiles.

## References

Dunn, M. J., et al. (2024). A minimal reporting guideline for
eye-tracking studies. *Behavior Research Methods*.

Niehorster, D. C., et al. (2026). How to determine the quality of
eye-tracking data: A tutorial. *Behavior Research Methods*.
doi:10.3758/s13428-026-03039-4.

## Examples

``` r
d <- simulate_gaze_quality_calibration(samples_per_target = 4)

q <- create_gaze_quality_report(
  d,
  by = c("profile", "target_id"),
  valid = "valid",
  missing_reason = "missing_reason",
  nominal_sampling_hz = 60
)

head(q[c(
  "profile", "target_id", "accuracy_mean", "precision_rms_s2s",
  "bcea", "effective_sampling_hz", "data_loss_fraction"
)])
#>       profile target_id accuracy_mean precision_rms_s2s       bcea
#> 1 missingness         1     0.3385511         0.4586771 0.32314661
#> 2 missingness         2     0.2567571         0.4603552 0.17342363
#> 3 missingness         3     0.2884298         0.3774378 0.20977189
#> 4 missingness         4     0.2610593         0.4301536 0.19541697
#> 5 missingness         5     0.2288653         0.2539276 0.09689068
#> 6 missingness         6     0.3642338         0.3385071 0.07407507
#>   effective_sampling_hz data_loss_fraction
#> 1                    60                  0
#> 2                    60                  0
#> 3                    60                  0
#> 4                    60                  0
#> 5                    60                  0
#> 6                    60                  0

report_gaze_quality(q)
#> [1] "accuracy_mean: mean 0.643, range 0.094-1.969; precision_rms_s2s: mean 0.597, range 0.112-2.220; precision_sd: mean 0.368, range 0.080-1.229; bcea: mean 0.578, range 0.004-3.320; effective_sampling_hz: mean 60.180, range 45.174-89.438; valid_sample_fraction: mean 1.000, range 1.000-1.000; data_loss_fraction: mean 0.000, range 0.000-0.000. Review required for 0/54 analysis units. Thresholds, when supplied, are study-specific review rules and never trigger automatic exclusion."
```
