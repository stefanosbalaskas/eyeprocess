# Fit the M2 response + RT + gaze reference model

Fits the likelihood-faithful M2 reference model with CmdStanR. There is
no silent fallback. Missing observations are omitted from their
channel-specific likelihood under an explicit ignorable missingness
assumption; the person and item structural layers remain joint across
the observed channels.

## Usage

``` r
fit_multimodal_m2(
  x,
  person = "person_id",
  item = "item_id",
  response = "response",
  rt = "rt",
  gaze = "gaze",
  prior_profile = c("regularized", "paper_centered"),
  chains = 4L,
  parallel_chains = chains,
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  seed = 20260814L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 100L,
  quiet_compile = TRUE
)
```

## Arguments

- x:

  Data frame, \`eye_multimodal_m2_simulation\`, or compatible eyeprocess
  multimodal measurement object.

- person, item, response, rt, gaze:

  Column names.

- prior_profile:

  Prior profile.

- chains, parallel_chains, iter_warmup, iter_sampling:

  CmdStan sampling controls.

- seed:

  Reproducibility seed.

- adapt_delta, max_treedepth, refresh:

  CmdStan controls.

- quiet_compile:

  Suppress CmdStan compilation messages.

## Value

An \`eye_multimodal_m2_fit\`.
