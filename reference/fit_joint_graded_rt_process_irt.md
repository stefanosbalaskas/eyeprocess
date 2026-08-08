# Joint graded-response, RT, and process reference model

Extends the 2026 graded-response/RT direction with an optional
gaze/process channel. The bundled reference engine uses
proportional-odds plus crossed RT and process submodels; it is
explicitly experimental rather than a claim to reproduce the published
SAEM estimator.

## Usage

``` r
fit_joint_graded_rt_process_irt(
  data,
  response = "response",
  rt = "rt",
  process = "fixation_count",
  person = "participant_id",
  item = "item_id",
  engine = c("reference", "brms"),
  process_family = c("negative_binomial", "poisson", "gaussian"),
  iter = 2000,
  chains = 4,
  cores = 1,
  seed = 1,
  ...
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- rt:

  Response-time variable or column name.

- process:

  Process variable or column name.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- engine:

  Estimation engine.

- process_family:

  Distributional family for the process channel.

- iter:

  Number of estimation iterations.

- chains:

  Number of Bayesian chains.

- cores:

  Number of processor cores.

- seed:

  Random-number seed.

- ...:

  Additional arguments passed to the selected model, engine, or method.
