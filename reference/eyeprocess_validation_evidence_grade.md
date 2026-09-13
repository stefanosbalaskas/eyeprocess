# Grade the completeness of validation evidence

Grade the completeness of validation evidence

## Usage

``` r
eyeprocess_validation_evidence_grade(
  components,
  required = c("design", "execution", "summary", "provenance", "hash")
)
```

## Arguments

- components:

  Named evidence components.

- required:

  Required evidence components or requirements.

## Value

An object of class "eye_validation_evidence_grade", stored as a named
list, with components "grade", "required", "present", "coverage". It
contains grade the completeness of validation evidence and associated
metadata or diagnostics needed to interpret the result.
