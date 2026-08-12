test_that("validation atlas freezes and verifies", {
  claims <- eyeprocess_validation_claim_matrix(c("C1","C2"),c("deterministic","stress behavior"),c("E1","E2"),c("test","stress"),c("supported","qualified"))
  a <- eyeprocess_validation_evidence_atlas(claims,recovery=data.frame(x=1),sbc=data.frame(x=1),stress=data.frame(x=1),
                                             reliability=data.frame(x=1),negative_controls=data.frame(x=1),irt=data.frame(x=1),provenance=list(hash="x"),artifacts=data.frame(path="x"))
  expect_s3_class(a,"eye_validation_evidence_atlas")
  f <- freeze_eyeprocess_validation_atlas(a)
  expect_true(verify_eyeprocess_validation_atlas(f))
  expect_true(eyeprocess_validation_atlas_gaps(a)$complete)
})

test_that("Milestone 2 plot methods draw on a graphics device", {
  tf <- tempfile(fileext=".pdf"); grDevices::pdf(tf); on.exit({grDevices::dev.off(); unlink(tf)}, add=TRUE)
  items <- data.frame(item_id=paste0("I",1:6),a=1,b=seq(-1,1,length.out=6),c=0,d=1)
  expect_silent(plot(eyeprocess_irt_bank_coverage(items,target_information=1)))
  expect_silent(plot(eyeprocess_irt_targeting_gap(rnorm(50),items)))
  claims <- eyeprocess_validation_claim_matrix("C1","claim","E1","test","supported")
  at <- eyeprocess_validation_evidence_atlas(claims,recovery=data.frame(x=1))
  expect_silent(plot(at))
})
