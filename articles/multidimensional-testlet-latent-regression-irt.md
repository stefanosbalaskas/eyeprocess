# Multidimensional, testlet, and latent-regression IRT

Milestone \#2 adds design and diagnostic support for multidimensional
IRT without duplicating mature estimation engines.

``` r

L <- matrix(c(1,0, 1,0, 0,1, 0,1, 1,0, 0,1), ncol = 2, byrow = TRUE)
loading_spec <- eyeprocess_mirt_loading_spec(paste0("I",1:6), L, c("accuracy","process"), simple_structure = TRUE)
eyeprocess_mirt_loading_audit(loading_spec, min_items_per_dimension = 2)
#>          dimension n_loading_items meets_minimum
#> accuracy  accuracy               3          TRUE
#> process    process               3          TRUE
eyeprocess_mirt_information_matrix(c(0,0), c(1,.5))
#>       [,1]   [,2]
#> [1,] 0.250 0.1250
#> [2,] 0.125 0.0625
```

Testlet declarations, directional information, latent-regression design
matrices, and identification audits are native. Exact multidimensional,
bifactor/two-tier, multiple-group, mixed, and polytomous estimation is
delegated to `mirt` or `TAM` where requested.

Primary package sources: <https://cran.r-project.org/package=mirt> and
<https://cran.r-project.org/package=TAM>.
