# Extract standardized sliding-window process features

Extract standardized sliding-window process features

## Usage

``` r
extract_process_windows(
  data,
  person = "person_id",
  trial = "trial_id",
  time = "time_ms",
  spec = process_window_spec(),
  align_time = NULL,
  pupil = "pupil_bc",
  pupil_tonic = "pupil_tonic",
  pupil_phasic = "pupil_phasic",
  gaze_x = "x",
  gaze_y = "y",
  aoi = "aoi",
  valid_gaze = "valid_gaze_prop",
  valid_pupil = "valid_pupil_prop",
  blink = "blink",
  trackloss = "trackloss"
)
```

## Arguments

- data:

  Sample-level eye-tracking/pupil data.

- person, trial, time:

  Identifier/time columns.

- spec:

  Window specification.

- align_time:

  Optional column containing the alignment event time in the same units
  as \`time\`. Required for response/custom alignment unless \`time\` is
  already relative to the desired origin.

- pupil:

  Optional pupil signal column.

- pupil_tonic, pupil_phasic:

  Optional decomposed pupil columns.

- gaze_x, gaze_y:

  Optional gaze coordinates.

- aoi:

  Optional AOI-state column.

- valid_gaze, valid_pupil:

  Optional validity columns/indicators.

- blink, trackloss:

  Optional blink/trackloss indicators.

## Value

An \`eye_process_windows\` object.
