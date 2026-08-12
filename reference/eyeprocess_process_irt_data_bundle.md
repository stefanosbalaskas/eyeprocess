# Prepare a sparse response/process bundle for external joint engines

Prepare a sparse response/process bundle for external joint engines

## Usage

``` r
eyeprocess_process_irt_data_bundle(
  data,
  person,
  item,
  response,
  response_time = NULL,
  process = character(),
  covariates = character()
)
```

## Arguments

- data:

  Input data frame, matrix, or compatible analysis object.

- person:

  Name of the person identifier column.

- item:

  Name of the item identifier column.

- response:

  Observed item response or response variable.

- response_time:

  Response-time variable or values.

- process:

  Process-measure columns or process object.

- covariates:

  Optional covariate columns or covariate data.
