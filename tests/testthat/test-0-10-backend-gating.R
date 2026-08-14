test_that("backend status is explicit", {
  x <- multimodal_backend_status()
  expect_true(is.data.frame(x))
  expect_true(all(c("backend","installed","version") %in% names(x)))
  expect_true("cmdstanr" %in% x$backend)
})

test_that("0.10 preserves the established public multimodal fitter", {
  expect_true(is.function(fit_multimodal_irt))
  expect_true(is.function(.fit_multimodal_irt_cmdstan_0_10))

  sim <- simulate_multimodal_irt(n_person=20,n_item=6,seed=5)
  ch <- irt_response_channel()
  s <- multimodal_irt_spec(response=ch, model="M0")

  expect_error(
    .fit_multimodal_irt_cmdstan_0_10(sim$measurement, s),
    "M0/M1 should use the package.s established response/RT estimators"
  )
})
