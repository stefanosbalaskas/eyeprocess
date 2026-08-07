# Representative scanpaths and scanpath distributions

Representative scanpaths and scanpath distributions. These functions
form the eyeprocess 0.6.0.9000 measurement-intelligence programme and
use dependency-free reference implementations with explicit evidence
limits.

## Usage

``` r
representative_scanpath(x, method = c("medoid", "barycenter", "consensus"),
  id_col = "person_id", aoi_col = "aoi", x_col = "x", y_col = "y",
  distance = c("multimatch", "edit", "transport"))
scanpath_dispersion(x)
compare_scanpath_distributions(x, group, distance = c("multimatch", "edit",
  "transport"), permutations = 499)
bootstrap_representative_scanpath(x, draws = 250, seed = 20260807)
plot_scanpath_atlas(x, ...)
plot_representative_scanpath(x, ...)
plot_scanpath_dispersion(x, ...)
plot_group_scanpath_transport(x, ...)
plot_scanpath_similarity_matrix(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- method:

  Argument controlling \`method\`; see the function usage and returned
  audit metadata.

- id_col:

  Argument controlling \`id_col\`; see the function usage and returned
  audit metadata.

- aoi_col:

  Argument controlling \`aoi_col\`; see the function usage and returned
  audit metadata.

- x_col:

  Argument controlling \`x_col\`; see the function usage and returned
  audit metadata.

- y_col:

  Argument controlling \`y_col\`; see the function usage and returned
  audit metadata.

- distance:

  Argument controlling \`distance\`; see the function usage and returned
  audit metadata.

- group:

  Argument controlling \`group\`; see the function usage and returned
  audit metadata.

- permutations:

  Argument controlling \`permutations\`; see the function usage and
  returned audit metadata.

- draws:

  Argument controlling \`draws\`; see the function usage and returned
  audit metadata.

- seed:

  Argument controlling \`seed\`; see the function usage and returned
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
