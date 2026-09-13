# Create a gated persistence-augmented gaze-diffusion IRT interface

Create a gated persistence-augmented gaze-diffusion IRT interface

## Usage

``` r
fit_persistence_gaze_diffusion_irt(data, engine = NULL, ...)
```

## Arguments

- data:

  Response/RT/process data.

- engine:

  Optional externally validated estimator function.

- ...:

  Passed to \`engine\`.

## Value

An object of class "eye_gated_process_model", stored as a named list,
with components "id", "purpose", "required_evidence", "engine", "fit",
"status", "notes", "caveat". It contains a gated persistence-augmented
gaze-diffusion IRT interface and associated metadata or diagnostics
needed to interpret the result.
