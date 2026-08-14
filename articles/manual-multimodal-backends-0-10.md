# Manual Installation of Multimodal Backends

## Backend status

``` r

multimodal_backend_status()
#>             backend installed version engine_ready
#> mirt           mirt      TRUE  1.46.1           NA
#> cmdstanr   cmdstanr      TRUE   0.9.0         TRUE
#> posterior posterior      TRUE   1.7.0           NA
#> loo             loo      TRUE  2.10.0           NA
#> TMB             TMB      TRUE  1.9.21           NA
#> brms           brms      TRUE  2.23.0           NA
```

Custom M2/M3 engines require CmdStanR plus CmdStan. Missing engines are
gated; `eyeprocess` does not silently substitute a simpler model.
