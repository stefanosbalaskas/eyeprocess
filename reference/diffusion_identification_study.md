# Construct a simulation-based identification study

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
diffusion_identification_study(conditions = list(n_person = c(50L, 150L),
  n_item = c(10L, 30L), gaze_effect = c(0, 0.35), contaminant_fraction = c(0, 0.05)),
  replications = 20L, base_seed = 20260805L,
  spec = gaze_diffusion_spec(drift_features = "gaze_balance", engine = "stan"))
```

## Arguments

- conditions:

  Named list or data frame of design conditions.

- replications:

  Replications per condition.

- base_seed:

  Base seed.

- spec:

  Confirmatory diffusion specification used for each fit.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
