# Construct a software-paper evidence bundle

Construct a software-paper evidence bundle

## Usage

``` r
software_paper_evidence_bundle(
  claims = NULL,
  validation = NULL,
  examples = NULL,
  articles = NULL,
  benchmarks = NULL,
  reproducibility = NULL,
  metadata = list()
)
```

## Arguments

- claims:

  Claim table or list.

- validation:

  Validation evidence table/list.

- examples:

  Optional example inventory.

- articles:

  Optional article inventory.

- benchmarks:

  Optional benchmark evidence.

- reproducibility:

  Optional reproducibility fingerprint.

- metadata:

  Optional metadata.

## Value

A named list with components "schema_version", "claims", "validation",
"examples", "articles", "benchmarks", "reproducibility", "metadata",
"created_utc", containing a software-paper evidence bundle and
associated metadata or diagnostics.
