# Fit an optional CmdStan dynamic-transition model

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fit_dynamic_irtree_stan(design, spec, seed = 1L, refresh = 0L, output_dir = NULL, ...)
```

## Arguments

- design:

  Transition design.

- spec:

  Dynamic IRTree specification.

- seed:

  Random seed.

- refresh:

  CmdStan refresh interval.

- output_dir:

  Optional CmdStan output directory.

- ...:

  Additional arguments to \`CmdStanModel\$sample()\`.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
