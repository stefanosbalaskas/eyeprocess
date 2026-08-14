test_that("M2 negative-control and validation plots return ggplot objects", {
  skip_if_not_installed("ggplot2")

  s <- simulate_multimodal_m2(n_person = 40, n_item = 8, seed = 24)
  nc <- multimodal_m2_negative_controls(s, seed = 7)
  val <- validate_multimodal_m2(s)

  expect_s3_class(plot(s, type = "channel_distributions"), "ggplot")
  expect_s3_class(plot(s, type = "person_latent_correlations"), "ggplot")
  expect_s3_class(plot(s, type = "item_truth"), "ggplot")
  expect_s3_class(plot(nc), "ggplot")
  expect_s3_class(plot(val), "ggplot")
})

test_that("M2 information and recovery plot contracts are stable", {
  skip_if_not_installed("ggplot2")

  info <- structure(
    list(
      table = data.frame(
        model = c("M0", "M1", "M2"),
        channels = c("response", "response + RT", "response + RT + gaze"),
        response_elpd_loo = c(-100, -98, -96),
        response_elpd_se = c(5, 5, 5),
        delta_response_elpd_vs_M0 = c(0, 2, 4),
        delta_response_elpd_se_vs_M0 = c(0, .8, 1.0),
        max_pareto_k = c(.4, .5, .6),
        mean_theta_posterior_variance = c(.50, .42, .35),
        theta_variance_reduction_vs_M0 = c(0, .16, .30)
      )
    ),
    class = c("eye_multimodal_m2_information", "list")
  )

  rec <- structure(
    list(
      raw = data.frame(
        family = c("person_latent", "person_latent"),
        truth = c(-1, 1),
        estimate = c(-.9, .95)
      ),
      summary = data.frame(
        family = "person_latent",
        coverage95 = .94
      )
    ),
    class = c("eye_multimodal_m2_recovery", "list")
  )

  expect_s3_class(plot(info, type = "theta_variance"), "ggplot")
  expect_s3_class(plot(info, type = "response_elpd"), "ggplot")
  expect_s3_class(plot(rec, type = "truth_vs_estimate"), "ggplot")
  expect_s3_class(plot(rec, type = "coverage"), "ggplot")
})
