test_that("bounded continuous conditional calibration respects dimensions", {
  set.seed(11)
  theta <- seq(-2, 2, length.out = 60)
  X <- cbind(
    item1 = pmin(1, pmax(0, .5 + .12 * theta + rnorm(60, 0, .08))),
    item2 = pmin(1, pmax(0, .4 + .18 * theta + rnorm(60, 0, .10)))
  )
  fit <- fit_censored_normal_process_irt(X, theta)
  expect_s3_class(fit, "eye_censored_normal_process_irt")
  expect_equal(nrow(fit$coefficients), 2)
  expect_true(all(is.finite(fit$coefficients$discrimination)))
  expect_true(all(is.finite(fit$coefficients$intercept)))
  expect_true(all(is.finite(fit$coefficients$sigma)))
  expect_true(all(fit$coefficients$sigma > 0))
  expect_true(all(fit$coefficients$convergence == 0L))
  pr <- predict(fit, theta = c(-1, 0, 1))
  expect_equal(dim(pr), c(3, 2))
  expect_true(all(pr >= 0 & pr <= 1))
})

test_that("channel ablation is directional and explicit", {
  d <- data.frame(y = 1:6, a = 2:7, b = 3:8)
  evaluator <- function(data, active_columns, channel_name) length(active_columns)
  z <- process_channel_ablation(d, list(a = "a", b = "b"), evaluator,
                                baseline = "y", higher_is_better = TRUE)
  expect_equal(nrow(z), 2)
  expect_true(all(z$information_loss == 1))
})
