test_that("0.9 fingerprints detect changed results and paper evidence is auditable", {
  f1 <- eye_reproducibility_fingerprint(data=1:5, result=10)
  f2 <- eye_reproducibility_fingerprint(data=1:5, result=11)
  expect_s3_class(f1, "eye_reproducibility_fingerprint")
  expect_true(verify_reproducibility_fingerprint(f1))
  cmp <- compare_reproducibility_fingerprints(f1,f2)
  expect_false(cmp$identical)
  claims <- software_paper_claim_matrix("claim", "E1", "test", "qualified")
  b <- software_paper_evidence_bundle(claims=claims, validation=data.frame(id="E1"), examples="x", articles="y", reproducibility=f1)
  expect_true(software_paper_readiness(b)$ready)
  vt <- software_paper_validation_table(list(recovery = structure(list(evidence=list()), class="eye_validation_bundle")))
  expect_true(is.data.frame(vt))
})

test_that("0.9 provenance and paper-evidence contracts reject ambiguous metadata", {
  expect_error(provenance_lineage_table(c("n", "n")), "unique")
  expect_error(provenance_edge_table("", "b"), "endpoints")
  expect_error(software_paper_claim_matrix("claim", status = NA_character_), "status")
  bad <- software_paper_evidence_bundle(claims = data.frame(status = NA_character_))
  expect_false(isTRUE(software_paper_readiness(bad, require_validation = FALSE, require_reproducibility = FALSE,
                                                require_examples = FALSE, require_articles = FALSE)$ready))
})
