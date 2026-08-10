test_that("0.9 sensitivity summaries and ranks are stable", {
  g <- process_sensitivity_grid(method = c("a", "b", "c"))
  x <- structure(list(grid = g,
                      results = data.frame(specification_id = g$specification_id, effect = c(.2,.3,.1), p_value = c(.04,.01,.2), method = c("a","b","c")),
                      failures = data.frame(), grid_hash = object_hash(g)), class = "eye_process_sensitivity")
  s <- summarise_process_sensitivity(x, p_value = "p_value")
  expect_equal(s$specifications, 3)
  expect_true(is.finite(sensitivity_sign_stability(x)))
  expect_s3_class(decision_stability(x, p_value = "p_value"), "eye_decision_stability")
  expect_equal(sensitivity_rank_stability(list(c(1,2,3), c(1,3,2))), 0.5)
})
