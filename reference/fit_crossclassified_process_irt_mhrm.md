# Create a gated scalable cross-classified MH-RM process IRT interface

Create a gated scalable cross-classified MH-RM process IRT interface

## Usage

``` r
fit_crossclassified_process_irt_mhrm(data, engine = NULL, ...)
```

## Arguments

- data:

  Data supplied to an optional external engine.

- engine:

  Optional estimator implementing the intended scalable MH-RM model.

- ...:

  Passed to engine.

## Value

An object of class "eye_gated_process_model", stored as a named list,
with components "id", "purpose", "required_evidence", "engine", "fit",
"status", "notes", "caveat". It contains a gated scalable
cross-classified MH-RM process IRT interface and associated metadata or
diagnostics needed to interpret the result.
