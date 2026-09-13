# DINA response probabilities from slip and guess parameters

DINA response probabilities from slip and guess parameters

## Usage

``` r
eyeprocess_cdm_dina_probability(ideal_response, slip = 0.1, guess = 0.2)
```

## Arguments

- ideal_response:

  Ideal-response indicator or matrix implied by the cognitive-diagnosis
  model.

- slip:

  DINA slip parameter or vector of slip parameters.

- guess:

  DINA guessing parameter or vector of guessing parameters.

## Value

An object of class "matrix", stored as an R object, containing dINA
response probabilities from slip and guess parameters and associated
metadata needed to interpret the result.
