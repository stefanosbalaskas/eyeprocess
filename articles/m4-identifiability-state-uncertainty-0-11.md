# Identifiability, State Separation, and Label Uncertainty

State labels are anchored through ordered RT deviations only as an
identification convention. This ordering does not order psychological
meaning.

``` r

sim <- simulate_multimodal_m4(n_person = 30, n_item = 8, scenario = "weak", seed = 20260820)
audit_multimodal_m4_identifiability(sim, include_posterior = FALSE)
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
states <- multimodal_m4_state_diagnostics(sim)
states
#> <eye_multimodal_m4_states>
#>   source: synthetic_truth
#>   states: 2
#>   mean entropy: 8.003e-15
#>   mean MAP run length: 3.478
#>   boundary: MAP labels are secondary summaries; posterior probabilities carry uncertainty
```

Posterior entropy, occupancy, transition structure, emission separation,
and contextual associations should be inspected together before any
substantive interpretation.
