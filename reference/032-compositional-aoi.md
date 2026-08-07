# Compositional analysis of AOI attention

Compositional analysis of AOI attention. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
derive_aoi_composition(x, aois, denominator = c("total_aoi_dwell",
  "trial_duration"), zero_method = c("multiplicative", "bayesian"), id_cols = NULL,
  aoi_col = "aoi", value_col = "dwell_ms", trial_duration_col = NULL)
transform_aoi_composition(x, method = c("ilr", "clr", "alr"), reference = NULL)
fit_aoi_compositional_model(composition, formula, random = NULL, data = NULL,
  method = "ilr")
compare_aoi_compositions(x, group, method = c("permanova", "compositional_manova"),
  permutations = 499, seed = 20260807)
aoi_balance_coordinates(x, balances)
plot_aoi_ternary(x, ...)
plot_aoi_balance_biplot(x, ...)
plot_aoi_variation_matrix(x, ...)
plot_compositional_group_difference(x, ...)
plot_aoi_composition_trajectory(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- aois:

  Argument controlling \`aois\`; see the function usage and returned
  audit metadata.

- denominator:

  Argument controlling \`denominator\`; see the function usage and
  returned audit metadata.

- zero_method:

  Argument controlling \`zero_method\`; see the function usage and
  returned audit metadata.

- id_cols:

  Argument controlling \`id_cols\`; see the function usage and returned
  audit metadata.

- aoi_col:

  Argument controlling \`aoi_col\`; see the function usage and returned
  audit metadata.

- value_col:

  Argument controlling \`value_col\`; see the function usage and
  returned audit metadata.

- trial_duration_col:

  Argument controlling \`trial_duration_col\`; see the function usage
  and returned audit metadata.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- reference:

  Argument controlling \`reference\`; see the function usage and
  returned audit metadata.

- composition:

  Argument controlling \`composition\`; see the function usage and
  returned audit metadata.

- formula:

  Argument controlling \`formula\`; see the function usage and returned
  audit metadata.

- random:

  Argument controlling \`random\`; see the function usage and returned
  audit metadata.

- data:

  Argument controlling \`data\`; see the function usage and returned
  audit metadata.

- group:

  Argument controlling \`group\`; see the function usage and returned
  audit metadata.

- permutations:

  Argument controlling \`permutations\`; see the function usage and
  returned audit metadata.

- seed:

  Argument controlling \`seed\`; see the function usage and returned
  audit metadata.

- balances:

  Argument controlling \`balances\`; see the function usage and returned
  audit metadata.

- ...:

  Additional arguments passed to the underlying method or plotting
  function.

## Details

The APIs return auditable S3 objects. Plot wrappers call registered
base-graphics methods. Experimental or approximate engines are labelled
in object status fields and should be validated before confirmatory or
operational use.

## Value

An eyeprocess result object, data frame, model object, plot, or audit
table as documented by the individual function.

## See also

[`plot_diagnostics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
[`plot_evidence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
and
[`plot_sensitivity()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md).
