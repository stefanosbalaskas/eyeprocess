# Execute a licensed published-model reproduction

Execute a licensed published-model reproduction

## Usage

``` r
run_raven_reproduction(spec, importer, fitter, extractor, tolerance = 0.05)
```

## Arguments

- spec:

  Reproduction specification.

- importer:

  Function receiving \`spec\$data_path\`.

- fitter:

  Function receiving imported data and \`spec\`.

- extractor:

  Function returning named estimates or a parameter/estimate data frame.

- tolerance:

  Absolute target tolerance.

## Value

An \`eye_empirical_reproduction\` object.
