# Specify evidence required to promote advanced model interfaces

Specify evidence required to promote advanced model interfaces

## Usage

``` r
advanced_model_evidence_spec(
  models = c("fit_joint_process_model", "fit_shared_process_factor",
    "fit_strategy_mixture", "fit_process_irt", "fit_pupil_informed_irt",
    "fit_multimodal_irt", "fit_dynamic_aoi_model", "fit_gaze_weighted_choice",
    "fit_dynamic_irtree", "fit_joint_functional_pupil_irt", "fit_theory_strategy_irt",
    "fit_gaze_diffusion_irt"),
  require_recovery = TRUE,
  require_calibration = TRUE,
  require_misspecification = TRUE,
  require_grouped_validation = TRUE,
  require_engine_equivalence = TRUE,
  require_empirical_reproduction = TRUE,
  require_sensitivity = TRUE
)
```

## Arguments

- models:

  Advanced model function names.

- require_recovery:

  Require passing parameter-recovery evidence.

- require_calibration:

  Require simulation-based calibration evidence.

- require_misspecification:

  Require evidence that prespecified failure scenarios are detected.

- require_grouped_validation:

  Require grouped out-of-sample validation.

- require_engine_equivalence:

  Require comparison with a benchmark engine.

- require_empirical_reproduction:

  Require a licensed empirical reproduction.

- require_sensitivity:

  Require a multi-specification preprocessing/AOI sensitivity analysis.

## Value

An \`eye_advanced_evidence_spec\`.
