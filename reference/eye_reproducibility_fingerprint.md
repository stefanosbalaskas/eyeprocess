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

## Value

A named list with components "schema_version", "label",
"eyeprocess_version", "data_hash", "analysis_spec_hash",
"model_spec_hash", "decisions_hash", "result_hash", "file_manifest",
"seeds", "environment", containing a reproducibility fingerprint and
associated metadata or diagnostics.
