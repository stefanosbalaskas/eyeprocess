test_that("0.9 governed pipeline records dependencies and outputs", {
  spec <- eye_analysis_spec(method = "declared")
  p <- eye_analysis_pipeline(list(
    eye_pipeline_step("a", function(context, spec) 2),
    eye_pipeline_step("b", function(a, context, spec) a + 3, requires = "a")
  ), spec = spec)
  expect_true(isTRUE(validate_eye_pipeline(p)))
  r <- run_eye_pipeline(p)
  expect_s3_class(r, "eye_pipeline_run")
  expect_equal(pipeline_result(r, "b"), 5)
  expect_true(r$completed)
  expect_true(audit_eye_pipeline(r)$valid)
  expect_match(eye_pipeline_dot(p), "digraph")
  expect_match(eye_pipeline_mermaid(p), "flowchart")
  p2 <- eye_analysis_pipeline(list(
    eye_pipeline_step("dot", function(.context, .spec) length(.context) + length(.spec$decisions))
  ), spec = spec)
  expect_true(is.finite(pipeline_result(run_eye_pipeline(p2, context = list(x = 1)), "dot")))
  expect_error(eye_pipeline_step("not valid", identity), "syntactic")
})

test_that("0.9 strict pipelines reject undeclared step dependencies", {
  a <- eye_pipeline_step("a", function(context, spec) 1)
  b <- eye_pipeline_step("b", function(a, context, spec) a + 1)
  expect_error(
    eye_analysis_pipeline(list(a, b), strict = TRUE),
    "not declared"
  )
})

test_that("0.9 pipeline resumption will not reuse outputs under changed context", {
  p <- eye_analysis_pipeline(list(
    eye_pipeline_step("a", function(context, spec) context$value)
  ))
  r <- run_eye_pipeline(p, context = list(value = 2))
  expect_equal(pipeline_result(resume_eye_pipeline(p, r, context = list(value = 2)), "a"), 2)
  expect_error(
    resume_eye_pipeline(p, r, context = list(value = 3)),
    "different context"
  )
})
