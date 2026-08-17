test_that("M3 negative controls are deterministic and preserve dimensions", {
  sim <- simulate_multimodal_m3(n_person=30,n_item=8,seed=6)
  a <- multimodal_m3_negative_controls(sim, seed=99)
  b <- multimodal_m3_negative_controls(sim, seed=99)
  expect_s3_class(a, "eye_multimodal_m3_negative_controls")
  expect_identical(a$datasets, b$datasets)
  expect_true(all(c("pupil_within_item", "pupil_within_person", "pupil_phase_randomized", "luminance_only_pupil", "irrelevant_pupil") %in% names(a$datasets)))
  expect_equal(sort(a$datasets$pupil_within_item$pupil), sort(a$datasets$observed$pupil))
  expect_match(a$interpretation, "Meaningless or misaligned")
})

test_that("M3 phase randomization preserves observed first two moments approximately", {
  sim <- simulate_multimodal_m3(
    n_person=30, n_item=12, pupil_missingness="none",
    dropout=c(response=0,rt=0,gaze=0,pupil=0), seed=61
  )
  neg <- multimodal_m3_negative_controls(sim, seed=62)
  original <- neg$datasets$observed$pupil
  randomized <- neg$datasets$pupil_phase_randomized$pupil
  expect_equal(mean(randomized), mean(original), tolerance = 1e-8)
  expect_equal(stats::sd(randomized), stats::sd(original), tolerance = 1e-6)
  expect_false(identical(randomized, original))
})
