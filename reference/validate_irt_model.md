# Validate a registered multimodal IRT model

Validate a registered multimodal IRT model

## Usage

``` r
validate_irt_model(spec, validation = NULL, ...)
```

## Arguments

- spec:

  IRT model or validation specification.

- validation:

  Validation results or validation specification.

- ...:

  Additional arguments passed to the selected model, engine, or method.

## Value

An object of class "eye_irt_evidence_grade", stored as a named list,
with components "model_id", "grade", "checks", "recovery", "contract",
"warning". It contains a registered multimodal IRT model and associated
metadata or diagnostics needed to interpret the result.
