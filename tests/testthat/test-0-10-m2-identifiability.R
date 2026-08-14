test_that("M2 identifiability audit supports connected informative simulation", {
  s <- simulate_multimodal_m2(n_person = 40, n_item = 8, seed = 12)
  a <- audit_multimodal_m2_identifiability(s$data)

  expect_s3_class(a, "eye_multimodal_m2_identifiability")
  expect_true(a$supported)
  expect_equal(a$response_design$components, 1)
  expect_equal(a$rt_design$components, 1)
  expect_equal(a$gaze_design$components, 1)
  expect_true(all(a$checks$pass))
})

test_that("M2 data contract rejects duplicate person-item keys", {
  s <- simulate_multimodal_m2(n_person = 30, n_item = 8, seed = 12)
  d <- rbind(s$data, s$data[1, ])

  expect_error(
    audit_multimodal_m2_identifiability(d),
    "at most one row"
  )
})
