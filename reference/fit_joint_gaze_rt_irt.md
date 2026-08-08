# Joint response, response-time, and gaze-process IRT

The reference engine fits crossed person/item submodels for accuracy,
log-response-time, and a gaze process, then returns person- and
item-side latent-score covariance summaries. The \`brms\` engine uses
multivariate formulas with shared group-level IDs so person/item random
effects can be correlated across channels.

## Usage

``` r
fit_joint_gaze_rt_irt(
  data,
  response = "response",
  rt = "rt",
  gaze = "fixation_count",
  person = "participant_id",
  item = "item_id",
  gaze_family = c("negative_binomial", "poisson"),
  engine = c("reference", "brms"),
  iter = 2000,
  chains = 4,
  cores = 1,
  seed = 1,
  ...
)
```

## Arguments

- data:

  Long person-by-item data.

- response:

  Binary response variable.

- rt:

  Positive response-time variable.

- gaze:

  Gaze process variable, usually fixation count or dwell count.

- person, item:

  Person and item identifiers.

- gaze_family:

  Poisson or negative-binomial reference channel.

- engine:

  \`reference\` or \`brms\`.

- iter, chains, cores:

  Passed to brms.

- seed:

  Random seed.

- ...:

  Additional arguments to the selected engine.

## Value

An \`eye_joint_gaze_rt_irt\` object.
