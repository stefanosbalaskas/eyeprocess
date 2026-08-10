# Define synthetic measurement corruptions for stress testing

Define synthetic measurement corruptions for stress testing

## Usage

``` r
synthetic_corruption_plan(
  missingness = 0,
  pupil_dropout = 0,
  gaze_offset_x = 0,
  gaze_offset_y = 0,
  sampling_jitter_sd = 0,
  aoi_label_noise = 0,
  device_shift = 0,
  trial_drop = 0,
  seed = 1L
)
```

## Arguments

- missingness:

  Generic missingness proportion.

- pupil_dropout:

  Pupil dropout proportion.

- gaze_offset_x, gaze_offset_y:

  Additive gaze-coordinate offsets.

- sampling_jitter_sd:

  Timestamp jitter SD in timestamp units.

- aoi_label_noise:

  Proportion of AOI labels randomly reassigned.

- device_shift:

  Additive shift for a declared device-sensitive numeric column.

- trial_drop:

  Proportion of rows/trials removed.

- seed:

  Seed.
