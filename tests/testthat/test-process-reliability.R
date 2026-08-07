test_that("G study and D study produce bounded dependability", {
  data <- mi_trial_data()
  names(data)[names(data) == "person_id"] <- "person"
  names(data)[names(data) == "item_id"] <- "item"
  gs <- fit_process_gstudy(data, "dwell_ms", facets = c("person", "item", "session", "device"))
  expect_s3_class(gs, "eye_process_gstudy")
  ds <- design_process_dstudy(gs, items = c(5, 10), sessions = c(1, 2))
  expect_true(all(ds$design_grid$absolute_dependability >= 0 & ds$design_grid$absolute_dependability <= 1))
  audit <- audit_process_reliability(transform(data, person_id = person, item_id = item), "dwell_ms", person_col = "person_id", item_col = "item_id")
  expect_s3_class(audit, "eye_process_reliability_audit")
  expect_plot_silent(plot_dependability_surface(ds))
})
