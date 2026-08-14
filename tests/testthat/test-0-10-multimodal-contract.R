test_that("multimodal measurement contract preserves keys and channels", {
  sim <- simulate_multimodal_irt(n_person=20, n_item=6, seed=1)
  x <- sim$measurement
  expect_s3_class(x, "eye_multimodal_measurement")
  expect_equal(nrow(x$data), 120)
  expect_setequal(names(x$channels), c("response","rt","gaze","pupil"))
  a <- audit_multimodal_measurement(x)
  expect_true(a$valid)
  expect_true(a$key_unique)
})

test_that("duplicated keys are rejected", {
  d <- data.frame(person=c(1,1), item=c(1,1), response=c(1,0))
  expect_error(
    prepare_multimodal_irt_data(d, person="person", item="item", response="response"),
    "not unique"
  )
})

test_that("structural identifiability audit is conservative", {
  sim <- simulate_multimodal_irt(n_person=10, n_item=4, seed=2)
  a <- audit_multimodal_identifiability(sim$measurement)
  expect_false(a$supported)
  expect_true(all(c("few_persons","few_items") %in% a$issues))
})
