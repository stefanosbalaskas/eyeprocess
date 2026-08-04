# Advanced model programme and validation

The advanced functions are model families with explicit validation
obligations. They are not automatically confirmatory because they
execute.

## Dynamic gaze-state IRTree baseline

``` r

dynamic <- fit_dynamic_irtree(
  dataset,
  dynamic_irtree_spec(source = "samples", include_response = TRUE)
)
plot(dynamic)
```

## Functional pupil-informed IRT

``` r

pupil_fit <- fit_joint_functional_pupil_irt(
  dataset,
  functional_pupil_irt_spec(df = 5, engine = "two_stage_lme4")
)
plot(pupil_fit)
```

## Theory-defined strategies

``` r

prototypes <- rbind(
  constructive = c(matrix_dwell = 0.8, toggling = -0.5),
  elimination = c(matrix_dwell = -0.4, toggling = 0.9)
)
strategy_fit <- fit_theory_strategy_irt(
  dataset,
  theory_strategy_spec(prototypes)
)
plot(strategy_fit)
```

## Gaze-informed diffusion

``` r

diffusion <- fit_gaze_diffusion_irt(
  dataset,
  gaze_diffusion_spec(
    engine = "ez_regression",
    gaze_features = c("dwell_time_ms", "first_fixation_latency_ms")
  )
)
plot(diffusion)
```

Each model should undergo parameter recovery, coverage,
misspecification, grouped validation, preprocessing sensitivity, and
empirical reproduction before confirmatory use.

## Monte Carlo design

The package supplies a declared design grid rather than hiding
validation conditions inside scripts. The screening grid varies sample
size, item count, ability–speed correlation, process effects, feature
reliability, process missingness, AOI-state error, pupil
autocorrelation, luminance confounding, DIF, and local dependence.

``` r

grid <- advanced_validation_grid(quick = TRUE)
head(grid)

simulation <- do.call(
  simulate_advanced_process_data,
  c(as.list(grid[1, ]), list(seed = 20260804L))
)
str(simulation, max.level = 1)
```

A production validation run should use the full grid or a preregistered
subset, sufficient replications, confidence intervals, explicit
expected-failure scenarios, and grouped person/item validation. A fitted
object without interval coverage or a reproduction object without
published targets cannot satisfy the promotion gate.

## Evidence promotion gate

``` r

evidence <- list(
  fit_process_irt = list(
    recovery = recovery_result,
    calibration = sbc_result,
    misspecification = misspecification_result,
    grouped_validation = grouped_result,
    engine_equivalence = engine_result,
    empirical_reproduction = reproduction_result,
    sensitivity = multiverse_result
  )
)
model_audit <- audit_advanced_model_evidence(evidence)
plot(model_audit)
write_advanced_model_evidence_report(model_audit, "validation/advanced-model-evidence.md")
```

A model is promoted only when all evidence gates declared in
[`advanced_model_evidence_spec()`](https://stefanosbalaskas.github.io/eyeprocess/reference/advanced_model_evidence_spec.md)
are satisfied.
