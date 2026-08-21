# Trait-Conditioned Latent Response-Process States

M4 extends the M3 response + response-time + gaze + pupil measurement
backbone with a sequential latent response-process state. The state is a
statistical configuration of process measurements, not a psychological
construct.

``` r

sim <- simulate_multimodal_m4(n_person = 30, n_item = 8, seed = 20260820)
spec <- multimodal_m4_spec(n_states = 2, trait_conditioning = c("theta", "tau"))
print(spec)
#> <eye_multimodal_m4_spec>
#>   model: M4 response + RT + gaze + pupil + latent process state
#>   states: 2
#>   state channels: rt, gaze, pupil
#>   transition structure: markov
#>   trait conditioning: theta, tau
#>   identification: ordered_rt_effect
#>   boundary: scored-response ability equation is state-independent
#>   interpretation: statistical response-process states; no psychological label implied
```

The reference scored-response model remains state-independent. States
shift RT, gaze, and pupil process channels only, and posterior state
probabilities are the primary inferential object.
