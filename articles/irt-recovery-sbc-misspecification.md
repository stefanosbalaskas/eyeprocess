# IRT recovery, SBC, and misspecification evidence

Simulation-based calibration (SBC) and parameter recovery answer
different software-validation questions. Recovery checks whether fitted
estimates reproduce known generating quantities under declared
scenarios; SBC checks calibration of posterior computation under the
declared generative model.

``` r

design <- eyeprocess_irt_recovery_design(sample_size = 250L, n_items = 12L,
                                          missing_rate = c(0,.15), testlet_sd = c(0,.35),
                                          replications = 5L, seed = 20260811L)
head(design)
#>   scenario_id sample_size n_items missing_rate testlet_sd replications     seed
#> 1   IRTREC001         250      12         0.00       0.00            5 20260811
#> 2   IRTREC002         250      12         0.15       0.00            5 20260811
#> 3   IRTREC003         250      12         0.00       0.35            5 20260811
#> 4   IRTREC004         250      12         0.15       0.35            5 20260811
eyeprocess_irt_misspecification_suite()
#>                       scenario                perturbation
#> 1                    reference                        none
#> 2             local_dependence       testlet random effect
#> 3                  missingness               MCAR omission
#> 4 discrimination_heterogeneity    wider log-discrimination
#> 5              lower_asymptote    non-zero lower asymptote
#> 6               latent_mixture two-component theta mixture
#>                            target
#> 1            calibration baseline
#> 2              local independence
#> 3         missing-data robustness
#> 4              item heterogeneity
#> 5            guessing sensitivity
#> 6 latent distribution sensitivity
```

Exact recovery fitting currently requires `mirt`; absence produces a
gated result. SBC rank summaries reuse eyeprocess’s rank-diagnostic
infrastructure.

The SBC workflow follows the logic of Talts et al., *Validating Bayesian
Inference Algorithms with Simulation-Based Calibration*:
<https://arxiv.org/abs/1804.06788>.

## Simulation-based calibration view

The figure below is generated from a small deterministic known-item
simulation. It is a computational calibration diagnostic under the
declared generative model, not evidence of empirical model adequacy.

``` r

viz_items <- data.frame(
  item_id = paste0('I', 1:8),
  a = seq(0.8, 1.5, length.out = 8),
  b = seq(-1.5, 1.5, length.out = 8),
  c = 0,
  d = 1
)

viz_sbc <- eyeprocess::run_eyeprocess_irt_ability_sbc(
  items = viz_items,
  replications = 20L,
  posterior_draws = 19L,
  theta_grid = seq(-5, 5, length.out = 201L),
  seed = 902L
)

stopifnot(
  inherits(viz_sbc, 'eye_irt_sbc_evidence')
)

plot(viz_sbc)
```

![Simulation-based calibration diagnostic for the deterministic
known-item
example.](irt-recovery-sbc-misspecification_files/figure-html/m2-visual-sbc-1.png)

Simulation-based calibration diagnostic for the deterministic known-item
example.
