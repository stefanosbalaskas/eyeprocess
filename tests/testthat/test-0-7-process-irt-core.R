test_that("IRT channels and registry have explicit status", {
  ch <- irt_count_channel("poisson", value = "fixations")
  expect_s3_class(ch, "eye_irt_channel")
  models <- list_irt_models()
  expect_true(all(c("id", "status", "channels") %in% names(models)))
  expect_true("joint_gaze_rt" %in% models$id)
  expect_true("flow_mirt" %in% models$id)
})

test_that("missingness classification preserves process distinctions", {
  d <- data.frame(
    response = c(1, NA, NA, NA, NA),
    reached = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    inspected = c(TRUE, FALSE, FALSE, TRUE, TRUE),
    started = c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )
  z <- as.character(classify_item_missingness(d, inspected = "inspected", started = "started"))
  expect_equal(z[1], "answered")
  expect_equal(z[2], "not_reached")
  expect_equal(z[3], "reached_not_inspected")
  expect_equal(z[4], "inspected_omission")
  expect_equal(z[5], "started_unanswered")
})

test_that("n-gram and sequence embeddings are deterministic", {
  seqs <- list(c("stem", "A", "stem", "B"), c("stem", "B", "B"), c("A", "B", "A"))
  x <- process_ngram_features(seqs, n = c(1, 2))
  y <- process_ngram_features(seqs, n = c(1, 2))
  expect_equal(x, y)
  emb <- process_sequence_embedding(seqs, n = c(1, 2), dimensions = 2)
  expect_equal(nrow(emb), 3)
  expect_lte(ncol(emb), 2)
})

test_that("equating returns finite affine constants", {
  ref <- data.frame(a = c(1, 1.2, .9), b = c(-1, 0, 1))
  new <- data.frame(a = c(.9, 1.1, .8), b = c(-.8, .2, 1.2))
  z <- equate_irt_scales(ref, new, method = "mean-sigma")
  expect_true(is.finite(z$A))
  expect_true(is.finite(z$B))
  expect_equal(nrow(z$transformed), 3)
})

test_that("unimplemented exact experimental engines fail loudly", {
  m <- matrix(c(0,1,1,0,1,1), nrow = 2)
  expect_error(fit_flow_mirt(m), "external_engine|validated", ignore.case = TRUE)
  expect_error(fit_dynamic_gpirt(data.frame(x = 1)), "external_engine|validated", ignore.case = TRUE)
  expect_error(fit_continuous_time_irt(data.frame(x = 1)), "external_engine|validated", ignore.case = TRUE)
})

test_that("custom IRT specifications are evidence-gated by default", {
  ch <- list(response = irt_response_channel("2pl"))

  expect_error(
    irt_model_spec(
      latent = "ability",
      channels = ch
    ),
    "id.*missing|missing.*id",
    ignore.case = TRUE
  )

  spec <- irt_model_spec(
    id = "custom_validation_model",
    latent = "ability",
    channels = ch
  )

  expect_equal(spec$id, "custom_validation_model")
  expect_equal(spec$status, "experimental")
  expect_s3_class(spec, "eye_irt_model_spec")
})
