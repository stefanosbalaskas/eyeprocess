# Fit a dynamic gaze-state response-tree model

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fit_dynamic_irtree(x, spec = dynamic_irtree_spec(), min_transitions = 10L, seed = 1L,
  ...)
```

## Arguments

- x:

  An \`eye_dataset\` or transition/long-state data frame.

- spec:

  Dynamic IRTree specification.

- min_transitions:

  Minimum support per destination for baseline logits.

- seed:

  Random seed for probabilistic engines.

- ...:

  Engine-specific arguments.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
