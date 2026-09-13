# Explain local person-item latent-space interactions

Returns the closest person-item pairs in the fitted residual latent
space. Closeness is descriptive residual structure, not a causal
explanation.

## Usage

``` r
explain_latent_interaction(object, person = NULL, item = NULL, top = 10L)
```

## Arguments

- object:

  Fitted \`eye_latent_space_irt\` object.

- person:

  Optional person row/index/name to restrict.

- item:

  Optional item row/index/name to restrict.

- top:

  Number of closest pairs to return.

## Value

An R object containing explain local person-item latent-space
interactions. The concrete class and structure follow the selected
method, engine, or input object and are preserved as documented by that
workflow.
