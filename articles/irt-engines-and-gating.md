# External IRT engines and exact-method gating

The engine registry makes estimation boundaries inspectable.

``` r

eyeprocess_irt_engine_registry()
#>      engine                                                   capability
#> 1      mirt multidimensional/polytomous/testlet/multiple-group/mixed IRT
#> 2       TAM        Rasch/2PL/3PL/GPCM/latent regression/plausible values
#> 3     GDINA                   cognitive diagnosis and Q-matrix workflows
#> 4     LNIRT               joint response and lognormal response-time IRT
#> 5       eRm                       conditional Rasch/PCM/LLTM diagnostics
#> 6 equateIRT            IRT linking/equating and transformation stability
#> 7      catR                   unidimensional adaptive-testing simulation
#> 8   mirtCAT          multidimensional CAT and constrained/shadow testing
#>     package available
#> 1      mirt      TRUE
#> 2       TAM      TRUE
#> 3     GDINA      TRUE
#> 4     LNIRT      TRUE
#> 5       eRm      TRUE
#> 6 equateIRT      TRUE
#> 7      catR      TRUE
#> 8   mirtCAT      TRUE
```

The current registry covers `mirt`, `TAM`, `GDINA`, `LNIRT`, `eRm`,
`equateIRT`, `catR`, and `mirtCAT`. Wrappers return an
`eye_external_irt_fit` when the requested package is available;
otherwise they return an `eye_gated_irt_engine` with `fit = NULL`. They
never switch to another estimator.

``` r

fit <- fit_eyeprocess_mirt(response_matrix, model = 1, itemtype = "2PL")
validate_eyeprocess_external_irt_fit(fit, engine = "mirt")
```

This is the same governance principle used for eyeprocess’s existing
frontier estimators: estimator identity is part of the scientific
specification.
