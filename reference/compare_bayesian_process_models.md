# Compare Bayesian process models by LOO or Bayes factor

Compare Bayesian process models by LOO or Bayes factor

## Usage

``` r
compare_bayesian_process_models(..., method = c("loo", "bayes_factor"))
```

## Arguments

- ...:

  Fitted brms models.

- method:

  \`loo\` or \`bayes_factor\`.

## Value

An R object containing bayesian process models by LOO or Bayes factor.
The concrete class and structure follow the selected method, engine, or
input object and are preserved as documented by that workflow.
