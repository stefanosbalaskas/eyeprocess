# Specifying and Fitting the M4 Reference Model

The reference model uses a marginalized finite-state Markov process over
explicitly ordered participant/session sequences. K=1 is a formal null
and K=2 is the conservative default.

``` r

sim <- simulate_multimodal_m4(n_person = 30, n_item = 8, seed = 20260820)
audit <- audit_multimodal_m4_identifiability(sim, include_posterior = FALSE)
audit
#> <eye_multimodal_m4_identifiability>
#>   overall: REVIEW
#>            domain                        criterion status severity value
#>          sequence                   sequence_count   PASS     none    30
#>          sequence          minimum_sequence_length   PASS     none     8
#>          sequence                 transition_count   PASS     none   210
#>              data                response_observed   PASS     none   240
#>              data                      rt_observed   PASS     none   240
#>              data                    gaze_observed   PASS     none   240
#>              data                   pupil_observed   PASS     none   240
#>              data maximum_process_missing_fraction   PASS     none     0
#>  model_complexity         trait_conditioned_markov REVIEW moderate     2
#>                                     threshold
#>                                 >=2 preferred
#>                                           >=2
#>                                          >=20
#>                                            >0
#>                                            >0
#>                                            >0
#>                                            >0
#>                              <=0.50 preferred
#>  0 transition traits for unconditional Markov
#>                                                                                                                                               message
#>                                                                                                              Number of independent ordered sequences.
#>                                                                                           Shortest sequence relative to declared fitting requirement.
#>                                                                                                                Available within-sequence transitions.
#>                                                                                                                       Observed response measurements.
#>                                                                                                                             Observed rt measurements.
#>                                                                                                                           Observed gaze measurements.
#>                                                                                                                          Observed pupil measurements.
#>                                                                                                             Largest process-channel missing fraction.
#>  Trait-conditioned Markov dynamics add person-level process slopes whose posterior identification cannot be established from structural counts alone.
#>                                                                                                                                                           recommendation
#>                                                                                                     Use multiple persons/sequences for population-level state inference.
#>                                                                                            Do not silently discard short sequences; revise design or explicit threshold.
#>                                                                                          Transition parameters may be prior-dominated when few transitions are observed.
#>                                                                                            M4 cannot identify the requested response contribution without observed data.
#>                                                                                                  M4 cannot identify the requested rt contribution without observed data.
#>                                                                                                M4 cannot identify the requested gaze contribution without observed data.
#>                                                                                               M4 cannot identify the requested pupil contribution without observed data.
#>                                                                                       Study missingness sensitivity; M4 reference fitting assumes ignorable missingness.
#>  Treat this specification as gated: require satisfactory posterior R-hat/ESS, stable state occupancy across chains, and separated state emissions before interpretation.
```

A deliberate CmdStanR fit is required for posterior inference;
documentation does not compile or sample automatically.

``` r

fit <- fit_multimodal_m4(sim, spec = multimodal_m4_spec())
summary(fit)
fitted(fit, type = "state")
```
