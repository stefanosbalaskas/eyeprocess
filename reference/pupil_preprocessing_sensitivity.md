# Run functional pupil preprocessing sensitivity analysis

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
pupil_preprocessing_sensitivity(x, grid = pupil_preprocessing_grid(),
  base_spec = functional_pupil_irt_spec(engine = "two_stage_glm"), fit = TRUE,
  extractor = extract_functional_pupil_parameters, continue_on_error = TRUE, ...)
```

## Arguments

- x:

  Eye dataset or long pupil data.

- grid:

  Sensitivity grid.

- base_spec:

  Base functional pupil specification.

- fit:

  Whether to fit each specification.

- extractor:

  Optional result extractor.

- continue_on_error:

  Record errors rather than stopping.

- ...:

  Passed to model fitting.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
