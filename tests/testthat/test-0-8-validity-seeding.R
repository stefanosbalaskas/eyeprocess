testthat::test_that("external validity separates baseline and process contribution", {
  set.seed(9)
  n <- 80
  d <- data.frame(age = rnorm(n, 35, 8), theta = rnorm(n), pupil = rnorm(n), dwell = rnorm(n))
  d$criterion <- .2 * scale(d$age)[,1] + .5 * d$theta + .3 * d$pupil + rnorm(n, 0, .5)
  a <- audit_process_external_validity(d, "criterion", predictors = c("theta", "pupil", "dwell"),
                                       baseline_predictors = "age")
  testthat::expect_s3_class(a, "eye_process_external_validity")
  testthat::expect_true(is.finite(a$incremental_r2))
  testthat::expect_equal(nrow(process_criterion_associations(a)), 3)
})

testthat::test_that("item seed model labels predictions non-operational", {
  set.seed(10)
  d <- data.frame(visual_density = runif(30), word_count = runif(30, 20, 100))
  d$irt_difficulty <- .8 * d$visual_density + .01 * d$word_count + rnorm(30, 0, .1)
  d$irt_discrimination <- 1.5 - .4 * d$visual_density + rnorm(30, 0, .1)
  fit <- fit_item_parameter_seed_model(d, predictors = c("visual_density", "word_count"), engine = "lm")
  p <- predict_item_parameter_priors(fit, d[1:3, c("visual_density", "word_count")])
  testthat::expect_s3_class(fit, "eye_item_parameter_seed")
  testthat::expect_true(all(grepl("not_operational", p$operational_status)))
})

testthat::test_that("presentation accessibility audit does not use clinical labels", {
  set.seed(11)
  d <- data.frame(person_id = rep(paste0("P", 1:30), each = 4),
                  rt_ms = rlnorm(120, log(1000), .2), dwell_ms = rlnorm(120, log(700), .2),
                  revisits = rpois(120, 2), aoi_entropy = runif(120),
                  pupil_peak = rnorm(120), valid_gaze_prop = runif(120, .8, 1))
  a <- audit_presentation_accessibility(d)
  testthat::expect_s3_class(a, "eye_presentation_accessibility")
  testthat::expect_false(any(grepl("ADHD|dyslex", a$table$interpretation_label, ignore.case = TRUE)))
})
