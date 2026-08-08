# Experimental Process-IRT Methods and Evidence Gates

## Why these functions are gated

Some methods are scientifically attractive but too new, too
estimator-specific, or too computationally demanding to present as
production estimators without a validated implementation. In those cases
`eyeprocess` provides one of three things:

- a transparent reference model;
- an adapter to an established external package;
- an explicit external-engine gate that refuses to fake the estimator.

## Process-state HMM + IRT

``` r

hmm <- fit_process_hmm_irt(
  data = events,
  sequence_id = "person_item",
  process_features = c("stem_dwell", "option_dwell", "transition_rate"),
  response = "correct",
  person = "person_id",
  item = "item_id",
  n_states = 3
)
process_state_occupancy(hmm)
process_state_transition_summary(hmm)
plot(hmm)
```

The internal HMM is an interpretable two-stage reference engine. State
labels are statistical summaries and should not be named as unobserved
mental states without independent validation.

## Cognitive diagnosis and latent process classes

``` r

cdm <- fit_cognitive_diagnosis_process(
  response_matrix = response_matrix,
  q_matrix = q_matrix,
  process_data = process_data,
  process_features = process_features
)

mix <- fit_latent_class_process_irt(
  data,
  process_features = c("fixation_count", "rt", "transition_entropy"),
  response = "correct",
  person = "person_id",
  item = "item_id",
  n_classes = 3
)
```

## Latent-space IRT

[`fit_latent_space_irt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_latent_space_irt.md)
is an adapter to `LSMjml`, avoiding a home-grown approximation when a
current R implementation exists.

``` r

ls <- fit_latent_space_irt(response_matrix, dimensions = 2)
map <- process_residual_map(ls)
plot(ls)

validate_latent_space_process_similarity(
  ls,
  process_matrix = scanpath_feature_matrix
)
```

This allows a new validation question: do person-item residual
proximities agree with independently measured process similarity?

## Process-adjusted DIF

``` r

surrogate <- process_dif_nuisance_surrogate(
  data,
  process_features = c("rt", "fixation_count", "stem_revisits")
)

audit_process_adjusted_dif(
  data,
  response = "correct",
  ability = "theta",
  group = "group",
  item = "item_id",
  process_features = c("rt", "fixation_count", "stem_revisits"),
  person = "participant_id"
)
```

Process adjustment should be reported transparently: which nuisance
surrogate was used, whether conclusions changed, and whether the process
channel itself may be group-dependent.

## Sequence representations

``` r

ngrams <- process_ngram_features(sequences, n = 2:4)
emb <- process_sequence_embedding(sequences, dimensions = 8)
fit_response_process_embedding_irt(
  data,
  sequences = sequences,
  response = "correct",
  person = "person_id",
  item = "item_id"
)
```

## Flexible item-response curves

``` r

gp <- fit_gpirt(response_matrix, engine = "spline_reference")
compare_parametric_nonparametric_irf(gp)
audit_irf_shape(gp)
plot(gp)
```

The spline reference is a **shape audit**, not a Gaussian-process
posterior. Exact GPIRT remains behind `external_engine` until a
validated engine is chosen.

``` r

fit_dynamic_gpirt(data, external_engine = my_validated_dynamic_gpirt)
fit_continuous_time_irt(data, external_engine = my_validated_ct_irt)
fit_flow_mirt(response_matrix, external_engine = my_validated_flow_mirt)
fit_variational_irt(response_matrix, external_engine = my_validated_vi_engine)
```

A missing engine produces a deliberate error instead of silently
substituting a different model.

## Linking and person fit

``` r

link <- equate_irt_scales(reference_parameters, new_parameters,
                          method = "stocking_lord")
plot(link)

pf <- process_person_fit(
  joint_fit,
  data = trials,
  person = "person_id"
)
plot(pf)
```

Person-fit output describes model-process inconsistency. It must not be
relabelled as cheating, deception, disengagement, or pathology without
separate evidence.

## Process-aware adaptive testing

``` r

info <- process_item_information(theta, a, b,
                                 process_information = process_information,
                                 rt_information = rt_information,
                                 weights = c(response = 1, rt = .25, process = .25))
expected_process_information(info)
select_next_item_process(theta, item_bank)
simulate_process_cat(item_bank, true_theta = 0, n_items = 10)
```

The adaptive functions are research utilities. They should not be
deployed in a high-stakes adaptive assessment until item-selection bias,
exposure, fairness, measurement invariance, and stopping rules have been
separately validated.

## Promotion rule

Experimental methods should remain experimental until they pass the same
simulation, calibration, misspecification, preprocessing, and
external-validation contract as the simpler models. Novelty is not
evidence.
