# Specify a hardened dynamic gaze-state IRTree

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
dynamic_irtree_spec(source = c("samples", "visits", "fixations"),
  collapse_consecutive = TRUE, engine = c("baseline", "multinomial", "stan"),
  hidden_states = 0L, include_response = TRUE, include_person = FALSE,
  include_item = TRUE, condition_columns = character(),
  transition_predictors = character(), interactions = character(),
  include_time_gap = TRUE, person_effect = c("none", "fixed", "random"),
  item_effect = c("none", "fixed", "random"), structural_zeros = NULL,
  allowed_transitions = NULL, hidden_structural_zeros = NULL,
  hidden_allowed_transitions = NULL, missing_state = c("drop", "unknown",
  "marginalize"), uncertain_state_probability = NULL, misclassification_matrix = NULL,
  ridge = 1e-4, standardize = TRUE, reference_state = NULL, chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  adapt_delta = 0.95, max_treedepth = 12L)
```

## Arguments

- source:

  Sequence source for an \`eye_dataset\`.

- collapse_consecutive:

  Collapse consecutive identical states.

- engine:

  Estimation engine: auditable binary baseline, penalized

- hidden_states:

  Number of latent states. Zero fits observed-state

- include_response:

  Include item response as a predictor.

- include_person:

  Include person effects.

- include_item:

  Include item effects.

- condition_columns:

  Condition-level predictors.

- transition_predictors:

  Additional transition-level predictors.

- interactions:

  Optional interaction terms supplied as formula strings.

- include_time_gap:

  Include log time gap for irregular observations.

- person_effect:

  Person effect type for Stan.

- item_effect:

  Item effect type for Stan.

- structural_zeros:

  Optional forbidden observed-state transition pairs.

- allowed_transitions:

  Optional allowed observed-state transition pairs.

- hidden_structural_zeros:

  Optional forbidden hidden-state pairs named \`state1\`, \`state2\`,
  and so forth.

- hidden_allowed_transitions:

  Optional explicitly allowed hidden-state pairs.

- missing_state:

  Treatment of missing observed states.

- uncertain_state_probability:

  Optional column containing probability of

- misclassification_matrix:

  Optional observed-state misclassification matrix.

- ridge:

  Penalization for the multinomial baseline.

- standardize:

  Standardize numeric design columns.

- reference_state:

  Optional reference destination state.

- chains:

  CmdStan controls.

- parallel_chains:

  CmdStan controls.

- iter_warmup:

  CmdStan controls.

- iter_sampling:

  CmdStan controls.

- adapt_delta:

  CmdStan sampler controls.

- max_treedepth:

  CmdStan sampler controls.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
