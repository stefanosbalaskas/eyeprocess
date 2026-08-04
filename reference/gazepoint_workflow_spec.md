# Specify an integrated Gazepoint downstream workflow

Creates a declarative specification for the end-to-end Gazepoint
workflow. The defaults preserve vendor fixations, interpolate only short
pupil gaps, apply a small median pupil filter, and avoid automatic pupil
baseline correction when no pre-stimulus baseline is available.

## Usage

``` r
gazepoint_workflow_spec(
  expected_sampling_rate = 60,
  sampling_tolerance_hz = 5,
  minimum_valid_gaze = 0.8,
  minimum_valid_pupil = 0.7,
  pupil_interpolation = "linear",
  pupil_max_gap_ms = 150,
  pupil_filter = "median",
  pupil_window = 5L,
  pupil_baseline = "none",
  pupil_baseline_window = c(0, 0.5),
  detect_blinks = TRUE,
  biometric_channels = c("eda", "skin_conductance_level", "skin_conductance_response",
    "heart_rate", "interbeat_interval", "engagement_dial"),
  create_plots = TRUE,
  create_html_report = TRUE,
  retain_raw = TRUE
)
```

## Arguments

- expected_sampling_rate:

  Expected gaze sampling rate in hertz.

- sampling_tolerance_hz:

  Allowed absolute sampling-rate deviation.

- minimum_valid_gaze:

  Minimum acceptable valid-gaze fraction.

- minimum_valid_pupil:

  Minimum acceptable valid-pupil fraction.

- pupil_interpolation:

  Pupil interpolation method.

- pupil_max_gap_ms:

  Maximum pupil gap eligible for interpolation.

- pupil_filter:

  Pupil smoothing method.

- pupil_window:

  Smoothing window in samples.

- pupil_baseline:

  Baseline correction method. The default is \`"none"\`.

- pupil_baseline_window:

  Baseline window relative to media/trial onset.

- detect_blinks:

  Whether to derive blink episodes from missing pupil data.

- biometric_channels:

  Channels to retain in workflow plots and tables.

- create_plots:

  Whether to create the complete plot suite.

- create_html_report:

  Whether to render an HTML copy of the report when \`rmarkdown\` and
  Pandoc are available.

- retain_raw:

  Whether imported native exports are retained in the object.

## Value

An \`eye_gazepoint_workflow_spec\` object.
