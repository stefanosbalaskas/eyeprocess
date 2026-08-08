# Cognitive-diagnosis model with process indicators

Uses GDINA for the response layer when available and retains process
features as a parallel diagnostic channel. Process/mastery associations
are reported descriptively and do not redefine the Q-matrix or skill
labels.

## Usage

``` r
fit_cognitive_diagnosis_process(
  response_matrix,
  q_matrix,
  process_data = NULL,
  process_features = NULL,
  person_id = NULL,
  engine = c("GDINA", "external"),
  external_engine = NULL,
  ...
)
```

## Arguments

- response_matrix:

  Person-by-item response matrix.

- q_matrix:

  Q-matrix for cognitive-diagnosis modeling.

- process_data:

  Process-data input used by the model.

- process_features:

  Names of process-derived features.

- person_id:

  Person or participant identifier.

- engine:

  Estimation engine.

- external_engine:

  Validated external fitting function.

- ...:

  Additional arguments passed to the selected model, engine, or method.
