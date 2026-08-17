test_that("M3 simulation and audit plots return ggplot objects", {
  skip_if_not_installed("ggplot2")
  sim <- simulate_multimodal_m3(n_person=30,n_item=8,seed=8)
  audit <- audit_multimodal_m3_identifiability(sim)
  for (type in c("channels","pupil_confounds","missingness","person_correlations","item_correlations","device","pupil_truth")) {
    expect_s3_class(plot(sim, type=type), "ggplot")
  }
  for (type in c("checks","missingness","device")) expect_s3_class(plot(audit, type=type), "ggplot")
})
