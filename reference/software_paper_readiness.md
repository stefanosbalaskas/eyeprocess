# Descriptive software-paper readiness audit

Readiness is defined only against caller-specified requirements; it is
not a journal acceptance prediction.

## Usage

``` r
software_paper_readiness(
  x,
  required_statuses = c("supported", "qualified"),
  require_validation = TRUE,
  require_reproducibility = TRUE,
  require_examples = TRUE,
  require_articles = TRUE
)
```

## Arguments

- x:

  Evidence bundle.

- required_statuses:

  Statuses allowed for claims.

- require_validation:

  Require non-empty validation evidence.

- require_reproducibility:

  Require a reproducibility fingerprint.

- require_examples:

  Require examples.

- require_articles:

  Require articles.
