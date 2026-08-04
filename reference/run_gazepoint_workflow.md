# Run the complete Gazepoint downstream workflow

Imports a real Gazepoint folder, constructs a canonical \`eye_dataset\`,
runs QC, reconstructs media trials, processes pupil and biometric
streams, derives gaze/AOI/pupil/biometric features, creates analysis and
IRT tables, generates plots, exports every stage, and writes a
reproducible report.

## Usage

``` r
run_gazepoint_workflow(
  path,
  output_dir = file.path(getwd(), "eyeprocess-gazepoint-workflow"),
  responses = NULL,
  score_key = NULL,
  item_map = NULL,
  spec = gazepoint_workflow_spec(),
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- path:

  Gazepoint export folder.

- output_dir:

  Destination directory.

- responses:

  Optional response data frame or CSV path.

- score_key:

  Optional named vector of correct responses by item id.

- item_map:

  Optional stimulus-to-item map.

- spec:

  Workflow specification from \`gazepoint_workflow_spec()\`.

- overwrite:

  Replace an existing output directory.

- quiet:

  Suppress progress messages.

## Value

An \`eye_gazepoint_workflow\` object.
