# Build the declared/fixture/empirical compatibility matrix

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
build_compatibility_matrix(x, required_vendors = c("gazepoint", "tobii", "pupillabs",
  "eyelink", "smi"), min_empirical_cases = 2L)
```

## Arguments

- x:

  Corpus path or registry data frame.

- required_vendors:

  Required vendors.

- min_empirical_cases:

  Minimum independent empirical cases per vendor.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
