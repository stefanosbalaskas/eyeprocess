# Recovery and Validation of Latent Response-Process States

Development recovery is deliberately bounded to five deterministic
scenarios: clear K=2, weak K=2, true K=1, trait-conditioned K=2, and
nuisance/confounded structure.

``` r

rec <- multimodal_m4_recovery()
rec
#> <eye_multimodal_m4_recovery>
#>   status: DESIGN ONLY - no backend fits executed
#>   scenarios: 5
#>             scenario true_K                                 purpose
#>                clear      2         basic state/transition recovery
#>                 weak      2 uncertainty rather than false certainty
#>                 null      1                     formal K=1 behavior
#>    trait_conditioned      2        transition-conditioning recovery
#>  nuisance_confounded      1           confounding should be exposed
#>   boundary: state recovery validates model behavior under synthetic truth, not substantive construct validity
```

The default returns the recovery design without fitting. Real recovery
requires an explicit `run = TRUE`. State labels are aligned before
evaluating state effects and transition structure; posterior probability
calibration is preferred to MAP accuracy alone.

``` r

rec_fit <- multimodal_m4_recovery(run = TRUE)
plot(rec_fit, type = "probability_calibration")
```
