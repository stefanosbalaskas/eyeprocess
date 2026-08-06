# Extract diffusion parameter summaries

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
extract_diffusion_parameters(object, variables = c("beta_drift", "beta_boundary",
  "beta_nondecision", "beta_starting", "person_drift", "item_difficulty", "boundary",
  "nondecision", "starting", "contaminant_probability"))
```

## Arguments

- object:

  Diffusion fit.

- variables:

  Optional variable prefixes.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
