# Run parameter-recovery, coverage, and misspecification validation

The fitter may return a model or throw an error. The extractor must
return a data frame with \`parameter\`, \`estimate\`, and optionally
\`std_error\`, \`lower\`, and \`upper\`. The truth extractor must return
a named numeric vector.

## Usage

``` r
run_model_validation(
  simulator,
  fitter,
  extractor,
  truth_extractor,
  grid = NULL,
  spec = model_validation_spec(),
  seed = 1L,
  continue_on_error = TRUE
)
```

## Arguments

- simulator:

  Simulation function.

- fitter:

  Estimation function receiving the simulation result.

- extractor:

  Parameter extraction function.

- truth_extractor:

  Truth extraction function.

- grid:

  Scenario grid.

- spec:

  Validation specification.

- seed:

  Random seed.

- continue_on_error:

  Record rather than stop on estimation errors.

## Value

An \`eye_model_validation\` object.
