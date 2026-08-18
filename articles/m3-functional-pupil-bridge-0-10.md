# M3 functional pupil bridge: from trajectories to joint measurement

## Why the first M3 likelihood is scalar

`eyeprocess` already contains trajectory-level pupil machinery,
including functional pupil specifications, event deconvolution, confound
modelling and signal-quality workflows. M3 does not replace those tools
with a new parallel implementation. Instead, the first four-channel
reference likelihood uses a trial-level scalar pupil measurement so that
the four-dimensional person/item covariance architecture can be
validated cleanly.

The functional bridge is explicit: an analyst first derives a
scientifically justified trial-level score from the existing pupil
workflow, then records that score as the M3 pupil representation.

``` r

sim <- simulate_multimodal_m3(n_person = 40, n_item = 8, seed = 20260815)
d <- sim$data

# Demonstration only. In a real workflow this should be an output from the
# package's functional/deconvolution pipeline with its provenance retained.
d$functional_score <- as.numeric(scale(d$pupil_baseline))

bridge <- multimodal_m3_functional_bridge(
  d,
  score = "functional_score",
  provenance = "demonstration score; replace with validated functional-pupil derivation"
)
print(bridge)
#> <eye_multimodal_m3_functional_bridge>
#>   rows: 320
#>   score source: functional_score
#>   pupil column: pupil
#>   boundary: The bridge records an externally justified scalar trajectory representation. It does not claim that the scalar preserves all functional pupil information or identify a psychological construct.
```

``` r

spec <- multimodal_m3_spec(pupil_representation = "functional_score")
print(spec)
#> <eye_multimodal_m3_spec>
#>   model: M3 response + RT + gaze + pupil
#>   backend: cmdstanr
#>   pupil representation: functional_score
#>   likelihood: Rasch + lognormal RT + NB gaze + Gaussian pupil
#>   missingness: ignorable
#>   lifecycle: experimental
#>   boundary: pupil responsivity is a neutral process dimension
```

## What the bridge does not do

The bridge does not silently select a time window, smooth a signal,
interpolate blinks, deconvolve events, baseline-correct, or decide
whether a trajectory component is psychologically meaningful. Those
choices belong to the upstream pupil workflow and should remain
inspectable.

It also does not claim that a scalar functional score preserves all
information in the original trajectory. M3 therefore distinguishes three
evidence questions:

1.  Is the raw/processed pupil trajectory measured with defensible
    quality and nuisance control?
2.  Is the scalar representation reproducible and stable enough to enter
    a joint model?
3.  Does that representation add response-target psychometric
    information beyond response, RT and gaze?

Only the third question is answered by M3 ablation and
[`multimodal_m3_process_information()`](https://stefanosbalaskas.github.io/eyeprocess/reference/multimodal_m3_process_information.md).

## Future full functional likelihood

A later extension can place a basis-coefficient or functional trajectory
likelihood directly inside the joint model. It should only be promoted
after basis choice, temporal correlation,
baseline/luminance/gaze-position adjustment, missing trajectories and
parameter recovery are validated. The scalar bridge is intentionally
conservative groundwork for that extension rather than a claim that
functional modelling has already been solved.
