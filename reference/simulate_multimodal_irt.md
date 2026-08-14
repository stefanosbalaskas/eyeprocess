# Simulate multimodal IRT process data

Generates deterministic synthetic person-item observations for staged
M0-M3 development. Gaze is a non-negative count process and pupil is a
neutral pupil-responsivity channel with explicit nuisance effects.

## Usage

``` r
simulate_multimodal_irt(
  n_person = 120L,
  n_item = 20L,
  seed = 42L,
  latent_cor = diag(4L),
  rt_sd = 0.3,
  gaze_size = 8,
  pupil_sd = 0.2,
  pupil_luminance = -0.2,
  gaze_x_effect = 0.08,
  gaze_y_effect = -0.05,
  missing_fraction = 0
)
```

## Arguments

- n_person, n_item:

  Positive integers.

- seed:

  Random seed.

- latent_cor:

  4x4 correlation matrix for ability, speed, gaze process, and pupil
  responsivity.

- rt_sd:

  Residual log-RT SD.

- gaze_size:

  Negative-binomial size.

- pupil_sd:

  Residual pupil-channel SD.

- pupil_luminance, gaze_x_effect, gaze_y_effect:

  Nuisance coefficients.

- missing_fraction:

  Independent channel dropout fraction used for a baseline stress
  condition.

## Value

An \`eye_multimodal_simulation\`.
