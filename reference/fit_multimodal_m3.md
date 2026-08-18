# Fit the M3 response + RT + gaze + pupil reference model

Fits the four-channel reference likelihood using CmdStanR. Pupil
summaries are standardized by default for a scale-stable reference
parameterization; the transformation is retained in the returned data
object. No missing nuisance values are silently imputed when the
corresponding nuisance column is supplied.

## Usage

``` r
fit_multimodal_m3(
  x,
  person = "person_id",
  item = "item_id",
  response = "response",
  rt = "rt",
  gaze = "gaze",
  pupil = "pupil",
  baseline = "pupil_baseline",
  luminance = "luminance",
  gaze_x = "gaze_x",
  gaze_y = "gaze_y",
  quality = "pupil_quality",
  time_on_task = "time_on_task",
  blink = "pupil_blink",
  interpolated = "pupil_interpolated",
  device = "device",
  session = "session",
  sampling_rate = "sampling_rate_hz",
  pupil_scale = c("z", "raw"),
  prior_profile = c("regularized", "paper_centered"),
  nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
  chains = 4L,
  parallel_chains = chains,
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  seed = 20260815L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 100L,
  quiet_compile = TRUE,
  init = 0
)
```

## Arguments

- x:

  Data frame, M3 simulation, or compatible measurement object.

- person, item, response, rt, gaze, pupil:

  Column names.

- baseline, luminance, gaze_x, gaze_y, quality, time_on_task:

  Optional nuisance columns.

- blink, interpolated:

  Optional pupil nuisance/audit indicators.

- device, session, sampling_rate:

  Optional measurement-context audit columns.

- pupil_scale:

  \`"z"\` or \`"raw"\`.

- prior_profile:

  Prior profile.

- nuisance:

  Named logical vector selecting the eight explicit pupil measurement
  nuisance terms.

- chains, parallel_chains, iter_warmup, iter_sampling:

  CmdStan sampling controls.

- seed, adapt_delta, max_treedepth, refresh, quiet_compile:

  CmdStan controls.

- init:

  CmdStan initialization; default zero initializes Cholesky factors at
  an interior identity point.

## Value

An \`eye_multimodal_m3_fit\`.
