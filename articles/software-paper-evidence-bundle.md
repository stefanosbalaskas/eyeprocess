# Building a software-paper evidence bundle

The paper-evidence layer links manuscript claims to explicit evidence
IDs, evidence types, qualifications, validation outputs, examples,
articles, benchmarks, and reproducibility fingerprints.

``` r

claims <- software_paper_claim_matrix(
  claim=c("Pipeline transformations are traceable", "Recovery is characterized under declared simulation regimes"),
  evidence_id=c("PIPE-01","VAL-01"),
  evidence_type=c("integration test","simulation"),
  status=c("supported","qualified")
)
b <- software_paper_evidence_bundle(claims=claims, validation=validation, examples=examples, articles=articles, reproducibility=fp)
software_paper_readiness(b)
software_paper_gap_analysis(b)
```

Readiness is a completeness audit against declared requirements. It is
not a prediction of peer-review acceptance.
