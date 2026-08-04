# Export Eye-Tracking-BIDS physiological recordings

Writes BIDS-compatible \`\_physio.tsv.gz\` and JSON pairs with
\`PhysioType = "eyetrack"\`. Continuous files contain no header;
\`Columns\` is stored in the sidecar. Each recorded eye is written
separately.

## Usage

``` r
export_eye_bids(
  x,
  path,
  task = "task",
  dataset_name = "eyeprocess eye-tracking dataset",
  overwrite = FALSE,
  screen_distance_m = NULL,
  screen_size_m = NULL
)
```

## Arguments

- x:

  An \`eye_dataset\`.

- path:

  BIDS dataset root.

- task:

  Task label.

- dataset_name:

  Dataset name for \`dataset_description.json\`.

- overwrite:

  Whether to replace existing files.

- screen_distance_m:

  Optional screen distance in metres.

- screen_size_m:

  Optional two-element screen size in metres.

## Value

A manifest of written files.
