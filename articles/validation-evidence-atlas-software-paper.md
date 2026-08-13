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

## Visual evidence atlas

The next views show how declared claims and evidence are represented
before and after freezing. They describe evidence organisation and
provenance, not substantive construct validity.

``` r

viz_claims <- eyeprocess::eyeprocess_validation_claim_matrix(
  'C1',
  'software behaviour is reproducible',
  'E1',
  'test',
  'supported'
)

viz_atlas <- eyeprocess::eyeprocess_validation_evidence_atlas(
  viz_claims,
  recovery = data.frame(x = 1)
)

stopifnot(
  inherits(viz_atlas, 'eye_validation_evidence_atlas')
)

plot(viz_atlas)
```

![Illustrative validation-evidence atlas linking a declared software
claim to recorded
evidence.](validation-evidence-atlas-software-paper_files/figure-html/m2-visual-atlas-1.png)

Illustrative validation-evidence atlas linking a declared software claim
to recorded evidence.

``` r

viz_atlas_freeze <- eyeprocess::freeze_eyeprocess_validation_atlas(
  viz_atlas
)

stopifnot(
  eyeprocess::verify_eyeprocess_validation_atlas(
    viz_atlas_freeze
  )
)

plot(viz_atlas_freeze)
```

![Frozen form of the illustrative validation-evidence
atlas.](validation-evidence-atlas-software-paper_files/figure-html/m2-visual-atlas-freeze-1.png)

Frozen form of the illustrative validation-evidence atlas.
