# Validate benchmark integrity and relational constraints

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
validate_benchmark_study(study = eyeprocess_benchmark_study(), verify_hashes = TRUE)
```

## Arguments

- study:

  Benchmark object or path.

- verify_hashes:

  Whether to verify MD5 hashes.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
