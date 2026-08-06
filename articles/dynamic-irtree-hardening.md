# Dynamic IRTree and transition-model hardening

## Scope

The dynamic-state layer models transitions among explicitly declared AOI
or process states. Observed states may be used directly, or an optional
hidden-state model may separate noisy observations from latent states. A
hidden state is not automatically a cognitive state; substantive
interpretation requires theory and external validation.

## Simulation and observed-state models

``` r

sim <- simulate_dynamic_irtree_data(
  n_person = 100,
  n_item = 20,
  transitions_per_trial = 10,
  state_misclassification = 0.05,
  missing_state = 0.10,
  seed = 42
)

spec <- dynamic_irtree_spec(
  engine = "multinomial",
  include_person = TRUE,
  include_item = TRUE,
  condition_columns = "condition",
  transition_predictors = c("time_gap", "score"),
  structural_zeros = data.frame(from = "submit", to = "prompt")
)

fit <- fit_dynamic_irtree(sim$transitions, spec)
decode_dynamic_states(fit)
transition_residual_diagnostics(fit)
```

[`dynamic_transition_design()`](https://stefanosbalaskas.github.io/eyeprocess/reference/dynamic_transition_design.md)
exposes the exact design matrix, transition mask, state coding, scaling,
participant/item indices, and uncertainty weights before estimation.

## Hidden states with Stan

``` r

hidden_spec <- dynamic_irtree_spec(
  engine = "stan",
  hidden_states = 3L,
  missing_state = "marginalize",
  person_effect = "random",
  item_effect = "random",
  chains = 4L,
  iter_warmup = 1000L,
  iter_sampling = 1000L
)

hidden_fit <- fit_dynamic_irtree(sim$transitions, hidden_spec, seed = 42)
probability <- decode_dynamic_states(hidden_fit, method = "probability")
```

The hidden engine uses a forward algorithm and estimates an emission
matrix. The returned probabilities are filtered state probabilities, not
claims about named cognition.

## Model comparison and recovery

``` r

baseline <- fit_dynamic_irtree(sim$transitions, dynamic_irtree_spec(engine = "baseline"))
multinomial <- fit_dynamic_irtree(sim$transitions, dynamic_irtree_spec(engine = "multinomial"))
compare_dynamic_transition_models(list(baseline = baseline, multinomial = multinomial))

programme <- dynamic_irtree_recovery(
  grid = expand.grid(
    state_misclassification = c(0, 0.05, 0.15),
    missing_state = c(0, 0.10)
  ),
  replications = 200L
)
```

Promotion requires recovery, coverage, state-error sensitivity,
misspecification studies, grouped validation, engine comparison, and
empirical reproduction.
