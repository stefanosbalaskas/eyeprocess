# Simulate advanced response-process data

Generates accuracy, response time, process features, theory-defined
strategy, dynamic AOI states, pupil trajectories, measurement error,
missing process data, DIF, and local dependence for validation studies.

## Usage

``` r
simulate_advanced_process_data(
  n_person = 100L,
  n_item = 20L,
  n_time = 30L,
  n_states = 3L,
  ability_speed_correlation = -0.3,
  gaze_effect = 0.35,
  feature_reliability = 0.7,
  missing_process = 0,
  state_misclassification = 0,
  pupil_ar1 = 0.6,
  luminance_effect = 0,
  dif_effect = 0,
  local_dependence = 0,
  seed = 1L
)
```

## Arguments

- n_person:

  Number of persons.

- n_item:

  Number of items.

- n_time:

  Pupil time bins.

- n_states:

  Number of AOI states.

- ability_speed_correlation:

  Correlation between ability and speed.

- gaze_effect:

  Process-feature coefficient in the response model.

- feature_reliability:

  Reliability of observed gaze features.

- missing_process:

  Fraction of process observations set missing.

- state_misclassification:

  Probability of AOI-state misclassification.

- pupil_ar1:

  AR(1) coefficient for pupil noise.

- luminance_effect:

  Effect of simulated luminance on pupil size.

- dif_effect:

  Logit-scale DIF effect for the focal group on flagged items.

- local_dependence:

  Shared testlet-effect standard deviation.

- seed:

  Random seed.

## Value

A list with trial data, state data, pupil data, and truth.
