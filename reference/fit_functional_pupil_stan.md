# Fit the bundled joint functional pupil-IRT Stan model

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fit_functional_pupil_stan(prepared, basis_matrix = NULL, seed = 1L, refresh = 0L,
  output_dir = NULL, ...)
```

## Arguments

- prepared:

  Prepared functional pupil data.

- basis_matrix:

  Functional basis matrix.

- seed:

  Random seed.

- refresh:

  CmdStan refresh interval.

- output_dir:

  Optional CmdStan output directory.

- ...:

  Additional sampling arguments.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
