# Register a multimodal IRT model

Register a multimodal IRT model

## Usage

``` r
register_irt_model(spec, overwrite = FALSE)
```

## Arguments

- spec:

  An \`irt_model_spec()\`.

- overwrite:

  Whether to replace an existing model with the same id.

## Value

An R object containing a multimodal IRT model. The concrete class and
structure follow the selected method, engine, or input object and are
preserved as documented by that workflow.
