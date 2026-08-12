test_that("Milestone 2 validation plans are deterministic and auditable", {
  p <- eyeprocess_validation_plan(sample_size = 100L, n_items = 6L, missing_rate = c(0, .1),
                                  noise_level = "reference", specification = c("correct", "misspecified"),
                                  replications = 3L, seed = 123L)
  g1 <- expand_eyeprocess_validation_plan(p)
  g2 <- expand_eyeprocess_validation_plan(p)
  expect_identical(g1, g2)
  expect_true(nrow(g1) > 1L)
  expect_true(all(g1$scenario_seed > 0L))
  r <- validation_acceptance_rule("rmse", "max", .2)
  a <- validation_acceptance_matrix(data.frame(id = "A", rmse = .15), list(rmse = r), id_cols = "id")
  expect_true(a$pass[[1L]])
  expect_true(is.finite(validation_mcse_profile(data.frame(value = rbinom(100, 1, .5)), metric = "value")$mcse_mean))
})
