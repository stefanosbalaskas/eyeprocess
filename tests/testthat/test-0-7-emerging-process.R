test_that("multiple-response combinations preserve selected options", {
  d <- data.frame(
    participant_id = rep(c("p1", "p2"), each = 4),
    item_id = "i1",
    option_id = rep(c("A", "B", "C", "D"), 2),
    selected = c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  )
  z <- encode_response_combinations(d)
  expect_equal(nrow(z), 2)
  expect_setequal(z$response_combination, c("A|C", "B|D"))
  expect_equal(z$n_selected, c(2L, 2L))
})

test_that("process local-dependence audit returns pair diagnostics", {
  set.seed(7)
  r <- matrix(rnorm(400), ncol = 4)
  colnames(r) <- paste0("i", 1:4)
  p <- r + matrix(rnorm(400, sd = .25), ncol = 4)
  z <- audit_process_local_dependence(r, p, threshold = .20)
  expect_s3_class(z, "eye_process_local_dependence_audit")
  expect_equal(nrow(z$pairs), choose(4, 2))
  expect_true(all(c("response_flag", "process_flag", "concordant_direction") %in% names(z$pairs)))
})

test_that("multiple-response reference model is labelled non-exact", {
  skip_if_not_installed("lme4")
  set.seed(11)
  n_person <- 24
  d <- expand.grid(
    participant_id = paste0("p", seq_len(n_person)),
    item_id = paste0("i", 1:4),
    option_id = c("A", "B"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  theta_map <- setNames(rnorm(n_person), paste0("p", seq_len(n_person)))
  d$theta <- theta_map[d$participant_id]
  eta <- -0.3 + 0.8 * d$theta + ifelse(d$option_id == "A", .25, -.25)
  d$selected <- stats::rbinom(nrow(d), 1, stats::plogis(eta))
  fit <- suppressWarnings(fit_multiple_response_process_irt(d))
  expect_s3_class(fit, "eye_multiple_response_process_irt")
  expect_false(fit$exact_multiple_response)
  expect_identical(fit$status, "experimental-reference")
})
