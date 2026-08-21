# Specify M4 trait-conditioned latent response-process states

Defines the M4 sequential extension of the validated M3 response + RT +
gaze + pupil model. The reference implementation keeps the
scored-response Rasch equation state-independent and lets latent states
shift selected RT, gaze, and pupil process channels. State labels are
statistical identification labels only and do not imply psychological
constructs.

## Usage

``` r
multimodal_m4_spec(
  n_states = 2L,
  state_channels = c("rt", "gaze", "pupil"),
  transition_structure = c("markov", "iid"),
  trait_conditioning = c("theta", "tau"),
  initial_trait_conditioning = TRUE,
  min_sequence_length = 2L,
  identification = c("ordered_rt_effect"),
  prior_profile = c("regularized", "paper_centered"),
  missingness = "ignorable",
  nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
  backend = "cmdstanr"
)
```

## Arguments

- n_states:

  Number of latent states, from 1 through 4. \`1\` is the formal null
  state model and should be treated as scientifically meaningful.

- state_channels:

  Subset of \`"rt"\`, \`"gaze"\`, and \`"pupil"\` receiving
  state-dependent deviations.

- transition_structure:

  \`"markov"\` for first-order transitions or \`"iid"\` for independent
  state membership over ordered trials.

- trait_conditioning:

  Subset of \`theta\`, \`tau\`, \`omega\`, and \`rho\` used to condition
  transition logits. The conservative default is \`theta + tau\`.

- initial_trait_conditioning:

  Whether the same selected traits condition initial-state
  probabilities.

- min_sequence_length:

  Minimum sequence length required by the structural audit for fitting.
  No rows are silently removed when sequences are shorter.

- identification:

  State-label identification policy. The reference policy orders
  centered RT state deviations; this is a label convention only.

- prior_profile:

  Prior profile inherited from M3.

- missingness:

  Currently \`"ignorable"\` only.

- nuisance:

  Named pupil-nuisance selection vector inherited from M3.

- backend:

  Currently \`"cmdstanr"\` only.

## Value

An \`eye_multimodal_m4_spec\` inheriting the canonical eyeprocess
multimodal/IRT specification classes.
