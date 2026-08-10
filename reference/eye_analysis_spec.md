# Define explicit analysis decisions for an eyeprocess workflow

Define explicit analysis decisions for an eyeprocess workflow

## Usage

``` r
eye_analysis_spec(
  blink_correction = NULL,
  pupil_baseline = NULL,
  fixation_algorithm = NULL,
  aoi_rule = NULL,
  exclusions = NULL,
  model = NULL,
  sensitivity = NULL,
  ...
)
```

## Arguments

- blink_correction:

  Named blink-correction rule or \`NULL\`.

- pupil_baseline:

  Numeric baseline window or \`NULL\`.

- fixation_algorithm:

  Named fixation algorithm or \`NULL\`.

- aoi_rule:

  Named AOI assignment rule or \`NULL\`.

- exclusions:

  Explicit exclusion specification or \`NULL\`.

- model:

  Explicit model specification or \`NULL\`.

- sensitivity:

  Sensitivity specification or \`NULL\`.

- ...:

  Additional named decisions.

## Value

An \`eye_analysis_spec\` object.
