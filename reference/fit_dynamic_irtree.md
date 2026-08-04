# Fit a dynamic gaze-state response-tree model

Fits one-vs-rest transition logits for each observed destination state.
The model preserves transition order, person/item structure, and
optional item response predictors. It is an auditable dynamic baseline
rather than a claim that observed AOI states are latent cognitive
states.

## Usage

``` r
fit_dynamic_irtree(x, spec = dynamic_irtree_spec(), min_transitions = 10L)
```

## Arguments

- x:

  An \`eye_dataset\`.

- spec:

  Dynamic IRTree specification.

- min_transitions:

  Minimum transitions required per destination state.

## Value

An \`eye_dynamic_irtree\` object.
