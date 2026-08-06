# Specify evidence gates for model promotion

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
model_promotion_spec(model_families = c("dynamic_irtree", "functional_pupil_irt",
  "theory_strategy_irt", "gaze_diffusion_irt"), require_completion = TRUE,
  require_sbc = TRUE, require_misspecification = TRUE,
  require_grouped_validation = TRUE, require_engine_equivalence = TRUE,
  require_empirical_reproduction = TRUE, require_preprocessing_sensitivity = TRUE,
  require_multi_vendor = FALSE)
```

## Arguments

- model_families:

  Model families to audit.

- require_completion:

  Require complete Monte Carlo evidence.

- require_sbc:

  Require simulation-based calibration.

- require_misspecification:

  Require declared misspecification studies.

- require_grouped_validation:

  Require grouped out-of-sample validation.

- require_engine_equivalence:

  Require external-engine comparison.

- require_empirical_reproduction:

  Require an empirical reproduction.

- require_preprocessing_sensitivity:

  Require preprocessing/AOI sensitivity.

- require_multi_vendor:

  Require independent multi-vendor evidence.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
