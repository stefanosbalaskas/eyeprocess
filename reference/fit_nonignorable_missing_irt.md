# Create a gated Bayesian nonignorable-missing IRT interface

Create a gated Bayesian nonignorable-missing IRT interface

## Usage

``` r
fit_nonignorable_missing_irt(data, engine = NULL, ...)
```

## Arguments

- data:

  Response/missingness data.

- engine:

  Optional externally validated Bayesian estimator.

- ...:

  Passed to \`engine\`.

## Value

An object of class "eye_gated_process_model", stored as a named list,
with components "id", "purpose", "required_evidence", "engine", "fit",
"status", "notes", "caveat". It contains a gated Bayesian
nonignorable-missing IRT interface and associated metadata or
diagnostics needed to interpret the result.
