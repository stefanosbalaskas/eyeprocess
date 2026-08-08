# Multimodal Process IRT: Responses, Time, Gaze, and Missingness

## Measurement channels, not feature dumping

The 0.7 architecture treats response-process observations as explicit
measurement channels. A process variable is not automatically useful
merely because it predicts an outcome. It should have a declared role,
latent target, family, provenance, and validation programme.

``` r

spec <- irt_model_spec(
  id = "accuracy_time_gaze",
  latent = c("ability", "speed", "engagement"),
  channels = list(
    response = irt_response_channel("2pl"),
    rt       = irt_rt_channel("lognormal"),
    gaze     = irt_count_channel("negative_binomial")
  ),
  status = "experimental"
)
spec
#> <eye_irt_model_spec> accuracy_time_gaze 
#>  status: experimental 
#>  latent: ability, speed, engagement 
#>  channels: response, rt, gaze
```

Other channels include nominal choices, survival/event time,
compositional AOI measurements, process sequences, functional
trajectories, and bounded continuous process measures.

``` r

irt_continuous_channel("censored_normal", value = "evidence_dwell_proportion")
#> $type
#> [1] "continuous"
#> 
#> $family
#> [1] "censored_normal"
#> 
#> $role
#> [1] "process"
#> 
#> $link
#> NULL
#> 
#> $variables
#> [1] "evidence_dwell_proportion"
#> 
#> $latent
#> [1] "process"
#> 
#> $options
#> $options$lower
#> [1] 0
#> 
#> $options$upper
#> [1] 1
#> 
#> 
#> attr(,"class")
#> [1] "eye_irt_continuous_channel" "eye_irt_channel"
irt_sequence_channel("scanpath", family = "hmm")
#> $type
#> [1] "sequence"
#> 
#> $family
#> [1] "hmm"
#> 
#> $role
#> [1] "process"
#> 
#> $link
#> NULL
#> 
#> $variables
#> [1] "scanpath"
#> 
#> $latent
#> [1] "strategy"
#> 
#> $options
#> list()
#> 
#> attr(,"class")
#> [1] "eye_irt_sequence_channel" "eye_irt_channel"
```

## Registry

``` r

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

Models can be registered and later promoted only after their validation
evidence passes an explicit gate.

``` r

register_irt_model(spec)
validate_irt_model("accuracy_time_gaze", validation_data)
promote_irt_model("accuracy_time_gaze", evidence = evidence_object)
```

## Response + response time + gaze

[`fit_joint_gaze_rt_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_joint_gaze_rt_irt.md)
supports two roles:

- `engine = "reference"` gives a transparent crossed-effects
  decomposition for development and validation;
- `engine = "brms"` builds a multivariate Bayesian model with shared
  grouping identifiers, which is the preferred route when a full
  Bayesian joint model is scientifically required.

``` r

fit <- fit_joint_gaze_rt_irt(
  data = trials,
  response = "correct",
  rt = "rt_ms",
  gaze = "fixation_count",
  person = "person_id",
  item = "item_id",
  gaze_family = "negative_binomial",
  engine = "brms"
)
plot(fit)
```

The function does not claim that a convenient reference engine is
identical to the published three-way Bayesian model. That distinction is
kept in the fit metadata.

## Graded responses

The same idea extends to ordinal/graded outcomes:

``` r

fit_joint_graded_rt_process_irt(
  data = trials,
  response = "rating",
  rt = "rt_ms",
  process = "fixation_count",
  person = "person_id",
  item = "item_id",
  engine = "brms"
)
```

This is experimental until parameter recovery and external validation
are completed.

## Nominal distractors + option gaze

Binary correct/incorrect scoring discards which alternative was
selected. A nominal process model can retain both the selected option
and visual consideration of each option.

``` r

fit <- fit_nominal_gaze_irt(
  data = option_trials,
  response_option = "choice",
  option_gaze = c("dwell_A", "dwell_B", "dwell_C", "dwell_D"),
  item = "item_id",
  person = "person_id"
)

option_process_information(fit)
distractor_process_map(fit)
audit_distractor_attention(fit)
plot(fit)
```

Interpretation should stay process-based: an option attracted or
retained more visual processing. This does not establish why.

## Missingness as a process

``` r

missing <- classify_item_missingness(
  trials,
  response = "response",
  reached = "reached",
  inspected = "inspected_response_region",
  started = "started_response"
)

audit <- fit_omission_survival_irt(
  data = missing,
  response = "correct",
  response_time = "rt",
  omission_time = "elapsed",
  reached = "reached",
  person = "person_id",
  item = "item_id"
)
plot(audit)
```

The classification separates not reached, reached but not inspected,
inspected omission, and started-but-unanswered cases instead of
converting them all to `NA`.

## Device and algorithm facets

``` r

facet_fit <- fit_manyfacet_process_irt(
  data = trials,
  response = "correct",
  process = "fixation_count",
  person = "person_id",
  item = "item_id",
  device = "device",
  session = "session",
  algorithm = "fixation_algorithm"
)

facet_effects(facet_fit)
audit_process_measurement_invariance(facet_fit)
plot(facet_fit)
```

A complementary
[`generalizability_process_study()`](https://stefanosbalaskas.github.io/eyeprocess/reference/generalizability_process_study.md)
decomposes variance before a full measurement model is attempted.

## Bounded gaze measures

AOI proportions and similar process quantities often have real mass at 0
and 1. The conditional censored-normal calibration helper respects those
bounds rather than silently applying ordinary Gaussian regression.

``` r

cn <- fit_censored_normal_process_irt(
  response_matrix = aoi_proportion_matrix,
  theta = calibration_theta,
  lower = 0,
  upper = 1
)
predict(cn, theta = seq(-2, 2, length.out = 9))
```

This is conditional calibration given supplied `theta`; it is not
labelled as the full marginal EM estimator from the 2026 CNRM paper.
