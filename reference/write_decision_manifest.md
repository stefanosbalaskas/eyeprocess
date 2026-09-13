# Write a decision manifest

Write a decision manifest

## Usage

``` r
write_decision_manifest(x, path, format = c("rds", "dput", "json"))
```

## Arguments

- x:

  Manifest.

- path:

  Output path.

- format:

  \`rds\`, \`dput\`, or \`json\`.

## Value

An R object containing a decision manifest. The concrete class and
structure follow the selected method, engine, or input object and are
preserved as documented by that workflow.
