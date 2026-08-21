# Does Latent State Structure Add Measurement Information?

M4 is evaluated incrementally against M3 rather than assumed to be
useful. The core comparison is M3 versus M4, with K=1 and
no-trait-conditioning models used as focused diagnostic references.

``` r

sim <- simulate_multimodal_m4(n_person = 30, n_item = 8, seed = 20260820)
multimodal_m4_ablation(sim)
#> <eye_multimodal_m4_ablation>
#>   executed: FALSE
#>   target: response-target predictive evidence
#> 
#>           model  K transition    traits state_channels
#>              M3 NA       <NA>      <NA>           <NA>
#>           M4_K1  1     markov      none  rt+gaze+pupil
#>           M4_K2  2     markov theta+tau  rt+gaze+pupil
#>  M4_K2_NO_TRAIT  2     markov      none  rt+gaze+pupil
#>       M4_K2_IID  2        iid theta+tau  rt+gaze+pupil
#>                            question
#>               validated M3 baseline
#>                     formal K=1 null
#>              reference M4 increment
#>     does trait conditioning matter?
#>  does sequential dependence matter?
```

``` r

abl <- multimodal_m4_ablation(sim, run = TRUE)
info <- multimodal_m4_process_information(abl)
info
```

Response-target ELPD and uncertainty changes are interpreted as
predictive/inferential evidence, not causal effects.
