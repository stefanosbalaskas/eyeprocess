# Fit a penalized multinomial transition model

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fit_multinomial_transition(design, ridge = 1e-4, reference_state = NULL,
  control = list(maxit = 1000L, reltol = 1e-9))
```

## Arguments

- design:

  Transition design.

- ridge:

  Ridge penalty.

- reference_state:

  Reference destination state.

- control:

  Passed to \`optim()\`.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
