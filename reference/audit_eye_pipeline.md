# Audit a pipeline definition or completed run

Audit a pipeline definition or completed run

## Usage

``` r
audit_eye_pipeline(x)
```

## Arguments

- x:

  Pipeline or pipeline run.

## Value

An object of class "eye_pipeline_audit", stored as a named list, with
components "table", "undeclared_decisions", "valid", "pipeline_hash". It
contains a pipeline definition or completed run and associated metadata
or diagnostics needed to interpret the result.
