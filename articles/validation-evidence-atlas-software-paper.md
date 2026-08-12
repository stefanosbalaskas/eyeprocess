# Validation evidence atlas and software-paper reporting

The evidence atlas links claims to executed outputs, tables, figures,
hashes, provenance, and limitations while preserving the distinction
between software behavior and substantive validity.

``` r

claims <- eyeprocess_validation_claim_matrix(
  c("C1","C2"),
  c("Scenario expansion is deterministic", "Exact engines are not silently substituted"),
  c("E1","E2"), c("test","engine-contract"), c("supported","qualified")
)
atlas <- eyeprocess_validation_evidence_atlas(claims, recovery = data.frame(id="demo"))
atlas
#> eyeprocess validation evidence atlas
#>   claims     : 2 
#>   components : 1 / 8 
#>   hash       : bfeba4a52e1e0ddc54b37b141f515485
```

[`freeze_eyeprocess_validation_atlas()`](https://stefanosbalaskas.github.io/eyeprocess/reference/freeze_eyeprocess_validation_atlas.md)
creates an integrity-checked snapshot.
[`write_eyeprocess_validation_report()`](https://stefanosbalaskas.github.io/eyeprocess/reference/write_eyeprocess_validation_report.md)
emits a compact Markdown report suitable for archival review.
Paper-ready table helpers convert recovery, SBC, stress, reliability,
negative-control, IRT precision, and external-engine status into compact
data frames.
