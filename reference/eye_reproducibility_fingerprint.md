# Construct a reproducibility fingerprint

Construct a reproducibility fingerprint

## Usage

``` r
eye_reproducibility_fingerprint(
  data = NULL,
  analysis_spec = NULL,
  model_spec = NULL,
  decisions = NULL,
  result = NULL,
  files = NULL,
  seeds = NULL,
  label = "eyeprocess_analysis"
)
```

## Arguments

- data:

  Input data or hashable object.

- analysis_spec:

  Analysis specification.

- model_spec:

  Model specification.

- decisions:

  Decision manifest.

- result:

  Optional result object.

- files:

  Optional input file paths.

- seeds:

  Optional seeds.

- label:

  Fingerprint label.
