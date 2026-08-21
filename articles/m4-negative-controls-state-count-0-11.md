# Negative Controls and State-Count Sensitivity

A credible state model should weaken when temporal structure is
destroyed or when apparent states are generated only by nuisance/device
structure.

``` r

sim <- simulate_multimodal_m4(n_person = 30, n_item = 8, seed = 20260820)
controls <- multimodal_m4_negative_controls(sim, seed = 20260821)
controls
#> <eye_multimodal_m4_negative_controls>
#>   controls: 6
#>   fitted: FALSE
#>   - order_shuffle: Destroys temporal alignment while preserving within-sequence joint process values.
#>   - process_shuffle: Destroys cross-channel and temporal alignment while preserving person-level marginals.
#>   - state_independent: Removes ordered state dependence while retaining person-level multichannel bundles.
#>   - nuisance_pseudostate: Creates block-like pupil structure fully explainable by recorded luminance nuisance.
#>   - device_session_pseudostate: Creates device-linked process structure that should trigger context/confounding review.
#>   - overfit_state_count: Fits one more state than the reference candidate to test state proliferation/overfitting.
sensitivity <- multimodal_m4_sensitivity(sim, n_states = 1:3)
sensitivity
#> <eye_multimodal_m4_sensitivity>
#>   executed K fits: FALSE
#>   evidence preference: NOT_EVALUATED
#> 
#>             dimension                          setting run_by_default
#>           state_count                              K=1           TRUE
#>           state_count                              K=2           TRUE
#>           state_count                              K=3           TRUE
#>                 prior    regularized vs paper_centered          FALSE
#>    trait_conditioning       theta+tau vs none/extended          FALSE
#>  transition_structure                    markov vs iid          FALSE
#>        state_channels         full vs channel omission          FALSE
#>              nuisance adjusted vs justified exclusions          FALSE
#>   min_sequence_length          2 vs stricter threshold          FALSE
```

Neither K nor substantive state meaning is selected automatically.
