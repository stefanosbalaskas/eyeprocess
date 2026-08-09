# Specify a biometric process pre-flight gate

Defines transparent, review-oriented thresholds for incoming gaze/pupil
data. The specification is a data-quality governance object, not a
behavioral or clinical classifier.

## Usage

``` r
process_preflight_spec(
  min_gaze_validity = 0.8,
  min_pupil_validity = 0.7,
  max_gaze_missingness = 0.25,
  max_pupil_missingness = 0.3,
  min_valid_trial_fraction = 0.7,
  trial_gaze_validity_threshold = 0.75,
  min_rt_ms = 200,
  max_rt_ms = 10000,
  sampling_rate_tolerance = 0.2,
  blink_quantile = 0.95,
  caution_flags = 1L,
  review_flags = 2L
)
```

## Arguments

- min_gaze_validity:

  Minimum mean gaze-validity proportion.

- min_pupil_validity:

  Minimum mean pupil-validity proportion.

- max_gaze_missingness:

  Maximum mean gaze-missingness proportion.

- max_pupil_missingness:

  Maximum mean pupil-missingness proportion.

- min_valid_trial_fraction:

  Minimum fraction of trials meeting the trial-level gaze-validity
  threshold.

- trial_gaze_validity_threshold:

  Gaze-validity threshold used to count an acceptable trial.

- min_rt_ms, max_rt_ms:

  Plausible mean response-time bounds in milliseconds.

- sampling_rate_tolerance:

  Fractional deviation from the cohort median sampling rate that
  triggers review.

- blink_quantile:

  Cohort quantile used for an extreme blink-cluster flag.

- caution_flags:

  Number of flags yielding \`use_with_caution\`.

- review_flags:

  Number of flags yielding \`review_or_exclude_from_biometric_models\`.

## Value

An \`eye_process_preflight_spec\` object.
