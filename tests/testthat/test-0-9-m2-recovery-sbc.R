test_that("IRT simulation is deterministic and SBC ranks are bounded", {
  items <- data.frame(item_id=paste0("I",1:5),a=1,b=seq(-1,1,length.out=5),c=0,d=1)
  s1 <- simulate_eyeprocess_irt_binary(50, items, seed=99)
  s2 <- simulate_eyeprocess_irt_binary(50, items, seed=99)
  expect_identical(s1$responses, s2$responses)
  set.seed(4)
  truth <- rnorm(30); draws <- matrix(rnorm(30*19), nrow=30)
  before <- .Random.seed
  ranks <- eyeprocess_irt_sbc_ranks(truth, draws, seed=8)
  expect_identical(.Random.seed, before)
  expect_true(all(ranks >= 0 & ranks <= 19))
  ev <- eyeprocess_irt_sbc_summary(ranks, n_draws=19, bins=10)
  expect_s3_class(ev, "eye_irt_sbc_evidence")
  ability_sbc <- run_eyeprocess_irt_ability_sbc(items, replications=20, posterior_draws=19, theta_grid=seq(-5,5,length.out=201), seed=19)
  expect_s3_class(ability_sbc, "eye_irt_sbc_evidence")
  expect_true(is.finite(ability_sbc$coverage))
  expect_equal(nrow(ability_sbc$details), 20)
})

test_that("recovery designs are explicit and mirt remains gated when absent", {
  d <- eyeprocess_irt_recovery_design(sample_size=50,n_items=5,missing_rate=0,testlet_sd=0,replications=1,seed=1)
  expect_s3_class(d,"eye_irt_recovery_design")
  if (!requireNamespace("mirt", quietly=TRUE)) expect_s3_class(run_eyeprocess_irt_recovery(d, verbose=FALSE), "eye_gated_irt_engine")
})
