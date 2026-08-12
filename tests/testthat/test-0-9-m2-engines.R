test_that("external IRT engine registry is explicit and never substitutes engines", {
  reg <- eyeprocess_irt_engine_registry()
  expect_true(all(c("mirt","TAM","GDINA","LNIRT","eRm","equateIRT","catR","mirtCAT") %in% reg$engine))
  expect_error(fit_eyeprocess_mirt(matrix(c(0,1,1,0),2), engine = "TAM"), "only accepts")
  expect_error(run_eyeprocess_equateirt("not_an_export", engine = "wrong"), "only accepts")
  if (!requireNamespace("GDINA", quietly = TRUE)) {
    g <- fit_eyeprocess_gdina(matrix(c(0,1,1,0),2), matrix(1,2,1))
    expect_s3_class(g, "eye_gated_irt_engine")
    expect_null(g$fit)
  }
})
