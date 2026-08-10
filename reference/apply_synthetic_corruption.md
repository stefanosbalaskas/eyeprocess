# Apply a synthetic corruption plan

Apply a synthetic corruption plan

## Usage

``` r
apply_synthetic_corruption(
  data,
  plan,
  gaze_columns = c("gaze_x", "gaze_y"),
  pupil = "pupil",
  time = "timestamp_ms",
  aoi = NULL,
  device_column = NULL
)
```

## Arguments

- data:

  Data frame.

- plan:

  Corruption plan.

- gaze_columns:

  Gaze columns receiving generic missingness/offset.

- pupil:

  Pupil column.

- time:

  Timestamp column.

- aoi:

  Optional AOI column.

- device_column:

  Optional numeric column receiving device shift.
