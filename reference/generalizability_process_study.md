# Generalizability-style variance decomposition for a process measure

Useful before many-facet IRT: quantifies how much variance comes from
person, item, device, session, algorithm, and residual sources.

## Usage

``` r
generalizability_process_study(data, outcome, facets, REML = TRUE)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- outcome:

  Outcome variable.

- facets:

  Facet variables included in the analysis.

- REML:

  Whether restricted maximum likelihood is used.
