# Simulate M4 multimodal sequential measurement data

Generates deterministic synthetic response, RT, gaze, pupil, nuisance,
and ordered latent-state data for software validation and methodological
stress testing. Synthetic state labels are known truth for recovery only
and are not psychological constructs.

## Usage

``` r
simulate_multimodal_m4(
  n_person = 80L,
  n_item = 12L,
  n_session = 1L,
  n_states = 2L,
  scenario = c("clear", "weak", "null", "persistent", "rapid_switch",
    "trait_conditioned", "rt_redundant", "gaze_redundant", "pupil_redundant",
    "nuisance_confounded", "device_confounded"),
  missingness = c("none", "mcar", "quality", "gaze", "pupil_quality", "device",
    "state_dependent"),
  missing_rate = 0.08,
  seed = 20260820L
)
```

## Arguments

- n_person, n_item:

  Number of persons and total item trials per person.

- n_session:

  Number of non-overlapping ordered sessions per person.

- n_states:

  True latent-state count. May be 1 through 4.

- scenario:

  State/data-generating scenario: \`clear\`, \`weak\`, \`null\`,
  \`persistent\`, \`rapid_switch\`, \`trait_conditioned\`,
  \`rt_redundant\`, \`gaze_redundant\`, \`pupil_redundant\`,
  \`nuisance_confounded\`, or \`device_confounded\`.

- missingness:

  Missingness stress mechanism.

- missing_rate:

  Base channel-missingness probability.

- seed:

  Deterministic random seed.

## Value

An \`eye_multimodal_m4_simulation\` containing \`data\`, full generating
\`truth\`, scenario metadata, and interpretation boundary.
