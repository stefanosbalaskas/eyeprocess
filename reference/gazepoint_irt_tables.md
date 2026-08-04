# Create IRT-ready response and process tables

This function does not manufacture responses. When no responses have
been supplied, it returns a complete response template and marks the
outcome as process-ready but response-pending.

## Usage

``` r
gazepoint_irt_tables(x, process_table = NULL)
```

## Arguments

- x:

  A processed \`eye_dataset\`.

- process_table:

  Optional process table from \`gazepoint_analysis_tables()\`.

## Value

A list containing a response template, process covariates, long IRT
table, optional matrices, and a readiness assessment.
