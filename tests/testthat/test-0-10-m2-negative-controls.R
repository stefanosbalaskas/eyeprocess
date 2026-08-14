test_that("M2 negative controls preserve within-item marginal values", {
  s <- simulate_multimodal_m2(n_person = 40, n_item = 8, seed = 23)
  nc <- multimodal_m2_negative_controls(s, seed = 99)

  expect_s3_class(nc, "eye_multimodal_m2_negative_controls")
  expect_setequal(
    names(nc$datasets),
    c("observed", "gaze_within_item", "rt_within_item", "response_within_item")
  )

  obs <- nc$datasets$observed
  gz <- nc$datasets$gaze_within_item

  for (it in unique(obs$item_id)) {
    expect_equal(
      sort(obs$gaze[obs$item_id == it]),
      sort(gz$gaze[gz$item_id == it])
    )
  }

  expect_equal(nrow(nc$diagnostics), 12)
})
