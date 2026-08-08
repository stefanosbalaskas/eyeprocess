# Process-state HMM with an IRT response layer

Fits a diagonal-Gaussian HMM to standardized process features within
each sequence, then uses state occupancy as explicit process evidence in
a response model. This two-stage reference engine is deliberately
interpretable and should be distinguished from a fully joint HMM-IRT
likelihood.

## Usage

``` r
fit_process_hmm_irt(
  data,
  sequence_id = "trial_id",
  order = "timestamp",
  process_features = c("x", "y"),
  response = "response",
  person = "participant_id",
  item = "item_id",
  n_states = 3L,
  max_iter = 100L,
  tol = 1e-05,
  seed = 1
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- sequence_id:

  Sequence identifier.

- order:

  Within-sequence ordering variable.

- process_features:

  Names of process-derived features.

- response:

  Response variable or response-column name.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- n_states:

  Number of latent process states.

- max_iter:

  Maximum number of iterations.

- tol:

  Numerical convergence tolerance.

- seed:

  Random-number seed.
