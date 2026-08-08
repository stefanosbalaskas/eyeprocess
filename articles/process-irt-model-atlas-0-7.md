# Process-IRT Model Atlas: What to Fit, What to Validate, What Not to Claim

## Purpose

The process-IRT layer is deliberately organized by *measurement
question*, not by estimator novelty. Eye-tracking, pupillometry,
response time, omissions, and sequences become explicit measurement
channels only when their role and validation evidence are stated.

``` r

validation_evidence_levels()
#>   rank                         level
#> 1    1                      declared
#> 2    2             synthetic-fixture
#> 3    3                vendor-example
#> 4    4       independent-public-real
#> 5    5 multisession-multidevice-real
#> 6    6  semantic-roundtrip-validated
#>                                                                                          requirement
#> 1                                                             Adapter or schema support is declared.
#> 2                           Deterministic synthetic or package fixture passes the declared contract.
#> 3                                A vendor-provided example export passes import and semantic checks.
#> 4                        An independently produced public real recording passes the declared checks.
#> 5                        Evidence spans repeated sessions and/or more than one device/model context.
#> 6 Native-to-canonical-to-interchange-to-canonical round trip has field-level semantic-loss evidence.
list_irt_models()
#>                            id       status                           latent
#> 1  bounded_continuous_process experimental                    process_trait
#> 2                   flow_mirt        gated             ability_1, ability_2
#> 3           gpirt_shape_audit        gated                          ability
#> 4           graded_rt_process experimental          ability, speed, process
#> 5               joint_gaze_rt    reference       ability, speed, engagement
#> 6        latent_space_process experimental       ability, interaction_space
#> 7           manyfacet_process    reference            person, item, process
#> 8   multiple_response_process experimental          ability, option_process
#> 9                nominal_gaze experimental          ability, option_process
#> 10          omission_survival experimental ability, speed, omission_process
#> 11                process_hmm experimental           ability, process_state
#>                 channels requirements
#> 1                process             
#> 2               response             
#> 3               response             
#> 4  response, rt, process             
#> 5     response, rt, gaze             
#> 6               response       LSMjml
#> 7         response, gaze             
#> 8      response, process             
#> 9         response, gaze             
#> 10        response, time             
#> 11     response, process             
#>                                                                                                 description
#> 1                                 Conditional censored-normal calibration for bounded process measurements.
#> 2   Normalizing-flow MIRT research gate; no production claim without external engine and recovery evidence.
#> 3                        Flexible item-response-curve audit; exact GP engine requires an external callback.
#> 4                                  Mixed/graded response extension with response-time and process channels.
#> 5                                                Response + RT + gaze-count joint measurement architecture.
#> 6                                      Person-item latent-space adapter for residual interaction structure.
#> 7                                                 Crossed person/item/device/session/algorithm facet model.
#> 8  Multiple-response option/process reference model; exact MRM/MRM-LD requires a validated external engine.
#> 9                                      Nominal response choices integrated with option-level gaze evidence.
#> 10                                   Separates observed responses, omissions, and not-reached observations.
#> 11                                  Two-stage process-state HMM plus response measurement reference engine.
```

## Core model families

| Question | Primary API | Default scientific status |
|----|----|----|
| Do response, time, and gaze share person/item structure? | [`fit_joint_gaze_rt_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_joint_gaze_rt_irt.md) | reference/experimental |
| Do graded scores and time/process co-vary? | [`fit_joint_graded_rt_process_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_joint_graded_rt_process_irt.md) | experimental |
| Which option was chosen and inspected? | [`fit_nominal_gaze_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_nominal_gaze_irt.md) | reference/experimental |
| Does visual exposure inform missingness? | [`fit_gaze_informed_missingness_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_gaze_informed_missingness_irt.md) | diagnostic |
| Are omissions and not-reached items time processes? | [`fit_omission_survival_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_omission_survival_irt.md) | reference/experimental |
| Are process measures transportable across device/session/algorithm? | [`fit_manyfacet_process_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_manyfacet_process_irt.md) | reference |
| Does the response process change within a session? | [`fit_changepoint_multimodal_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_changepoint_multimodal_irt.md) | experimental |
| Do latent sequence states relate to measurement? | [`fit_process_hmm_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_process_hmm_irt.md) | experimental |
| Do process features explain DIF nuisance variation? | [`audit_process_adjusted_dif()`](https://stefanosbalaskas.github.io/eyeprocess/reference/audit_process_adjusted_dif.md) | diagnostic |
| Is there residual person-item geometry? | [`fit_latent_space_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_latent_space_irt.md) | external engine |
| Are logistic IRFs too restrictive? | [`fit_gpirt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_gpirt.md) | model criticism/gated |
| Does a bounded process outcome pile up at 0/1? | [`fit_censored_normal_process_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_censored_normal_process_irt.md) | conditional calibration |
| Are event times informative conditional on theta? | [`fit_event_time_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_event_time_irt.md) | diagnostic/gated |
| Are multiple selected options informative beyond a total score? | [`fit_multiple_response_process_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_multiple_response_process_irt.md) | reference/external gated |
| Is there residual inter-option/process dependence? | [`audit_process_local_dependence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/audit_process_local_dependence.md) | diagnostic |
| Do revisits/RT/gaze add evidence to cognitive diagnosis? | [`fit_revisit_process_cdm()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_revisit_process_cdm.md) | adapter/experimental |
| Does a process channel add held-out information? | [`audit_channel_incremental_information()`](https://stefanosbalaskas.github.io/eyeprocess/reference/audit_channel_incremental_information.md) | validation |

## A process channel must earn its place

The preferred comparison is not “model with gaze has a lower in-sample
AIC.” Instead, compare held-out performance and run a negative control.

``` r

inc <- audit_channel_incremental_information(
  data = trials,
  fold = "participant_id",
  baseline_fitter = fit_without_gaze,
  process_fitter = fit_with_gaze,
  predictor = predict_model,
  scorer = score_model,
  higher_is_better = TRUE
)
plot(inc)

neg <- negative_control_process_test(
  data = trials,
  process = "dwell_time",
  fold = "participant_id",
  fitter = fit_with_gaze,
  predictor = predict_model,
  scorer = score_model
)
plot(neg)
```

## Missingness: separate exposure from response

``` r

miss <- classify_item_missingness(
  trials,
  response = "response",
  reached = "reached",
  inspected = "inspected",
  started = "response_started"
)

fit <- fit_gaze_informed_missingness_irt(
  trials,
  response = "response",
  person = "participant_id",
  item = "item_id",
  gaze_exposure = "item_dwell_ms",
  theta = "theta"
)
plot(fit)
```

A fitted association between gaze exposure and omission is not evidence
that missingness is ignorable, nor is it a behavioral diagnosis. The
two-part reference model is intended to expose this dependency before a
fully joint missingness model is claimed.

## Cross-device measurement is an estimand

``` r

facets <- fit_manyfacet_process_irt(
  trials,
  response = "correct",
  process = "dwell_ms",
  person = "participant_id",
  item = "item_id",
  device = "device",
  session = "session",
  algorithm = "fixation_algorithm"
)

device_facet_effects(facets, channel = "process")
session_facet_effects(facets, channel = "process")
algorithm_facet_effects(facets, channel = "process")
audit_process_measurement_invariance(facets)
```

A small device variance component is not enough for interchangeability.
It should be accompanied by semantic round-trip evidence,
unit/coordinate audits, and held-device/session validation.

## Latent distribution and IRF stress tests

``` r

audit_latent_distribution(theta)
compare_latent_distribution_models(theta)
latent_distribution_stress_test(validation_runner)

shape <- fit_gpirt(response_matrix, engine = "spline_reference")
plot_irf_uncertainty(shape, item = 1)
cmp <- compare_parametric_nonparametric_irf(response_matrix, shape)
audit_irf_shape(cmp)
```

The spline-reference route is intentionally called a shape audit, not
GPIRT. Exact GPIRT, dynamic GPIRT, flow-MIRT, variational IRT, and full
continuous-time IRT remain behind explicit external-engine gates until
validated implementations are supplied.

## Promotion is evidence-based

``` r

spec <- irt_validation_spec("joint_gaze_rt", replications = 500)

# retained recovery/SBC/PPC/transport results are combined into an evidence bundle
grade_model_evidence(evidence_bundle)
```

At minimum retain recovery, bias/RMSE, interval coverage,
convergence/failure classification, misspecification stress tests,
preprocessing sensitivity, and grouped/external validation. Bayesian
models additionally require SBC and posterior predictive checks;
posterior SBC is appropriate when calibration near the observed-data
regime matters and the model-specific self-consistency contract has been
implemented.
