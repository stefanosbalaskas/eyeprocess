# M3 process information: ablation, redundancy, and sensor value

## Why eight models?

The central 0.10 question is not whether adding sensors makes a model
more complicated. It is whether a process channel provides **additional
measurement information for a defined target**. Because RT, gaze and
pupil may be correlated, their contributions are not assumed to add
linearly.

M3 therefore defines the complete response-anchored lattice:

``` r

eyeprocess:::.ep10_m3_ablation_definitions()
#>          model    rt  gaze pupil                     channels
#> 1            R FALSE FALSE FALSE                     response
#> 2         R_RT  TRUE FALSE FALSE                response + RT
#> 3       R_GAZE FALSE  TRUE FALSE              response + gaze
#> 4      R_PUPIL FALSE FALSE  TRUE             response + pupil
#> 5    R_RT_GAZE  TRUE  TRUE FALSE         response + RT + gaze
#> 6   R_RT_PUPIL  TRUE FALSE  TRUE        response + RT + pupil
#> 7 R_GAZE_PUPIL FALSE  TRUE  TRUE      response + gaze + pupil
#> 8         FULL  TRUE  TRUE  TRUE response + RT + gaze + pupil
```

The eight models are response only; response + RT; response + gaze;
response + pupil; each two-process-channel combination; and the full
four-channel model.

## Fit the ablation lattice

``` r

sim <- simulate_multimodal_m3(n_person = 100, n_item = 12, seed = 20260815)

ab <- multimodal_m3_ablation(
  sim,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 750,
  iter_sampling = 750,
  refresh = 0
)

info <- multimodal_m3_process_information(ab)
print(info)
```

[`multimodal_m3_process_information()`](https://stefanosbalaskas.github.io/eyeprocess/reference/multimodal_m3_process_information.md)
uses response-target PSIS-LOO and posterior variance of person ability.
It does not sum channel Fisher information under a correlated joint
model.

## Incremental pupil evidence

Pupil is compared with and without the channel in four contexts:
response only, response + RT, response + gaze, and response + RT + gaze.
The paired response-ELPD contrast is accompanied by a standard error and
a descriptive evidence classification. The classification can return
`no_clear_incremental_pupil_information`; this is an intended scientific
outcome, not a failure of the package.

``` r

info$incremental_pupil
plot(info, type = "incremental_pupil")
```

## Redundancy and complementarity

The non-additivity table contrasts the full model with the sum of
single-channel additions and asks whether the incremental pupil gain is
attenuated or amplified after RT and gaze are already included.

``` r

info$nonadditivity
plot(info, type = "redundancy")
```

These are model-conditional predictive contrasts. “Synergy” in this
table means non-additivity on the response ELPD scale; it does not
establish a causal interaction among psychological processes.

## Sensor value and channel conflict

M3 adds two deliberately practical diagnostics. `sensor_value` reports
pupil response-target gain per usable pupil observation and per
analyst-supplied relative sensor cost. `channel_conflict` places
predictive performance beside convergence/stability diagnostics. A
sensor can improve an in-sample latent representation while worsening
response prediction or computational geometry; M3 surfaces that conflict
rather than hiding it behind one scalar rank.

``` r

info$sensor_value
info$channel_conflict
plot(info, type = "sensor_value")
plot(info, type = "conflict")
```

These diagnostics are not economic cost-effectiveness analyses and not
causal estimates. Their role is to prevent “more modalities” from
becoming an automatic conclusion of “more information.”
