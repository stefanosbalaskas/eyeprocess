make_mediation_data <- function() {
  data.frame(
    participant_id = rep(c("p1", "p2", "p3"), each = 4),
    trial_id = rep(1:4, 3),
    condition = rep(c(0, 1, 0, 1), 3),
    dwell_ms = c(100, 0, 120, NA, 90, 140, 80, 160, 110, 130, 100, 150),
    override = rep(c(0, 1, 0, 1), 3),
    valid_fraction = c(0.95, 0.99, 0.92, 0.96, 0.94, 0.40, 0.97, 0.93, 0.96, 0.95, 0.98, 0.92),
    stringsAsFactors = FALSE
  )
}

test_that("within-between decomposition preserves trial rows", {
  data <- make_mediation_data()
  out <- decompose_within_between(data, c("condition", "dwell_ms"))
  expect_equal(nrow(out), nrow(data))
  expect_equal(tapply(out$condition_within, out$participant_id, mean), c(p1 = 0, p2 = 0, p3 = 0))
  expect_equal(unique(out$condition_between), 0.5)
  expect_true(all(is.na(out$dwell_ms_within[is.na(data$dwell_ms)])))
})

test_that("preparation distinguishes true zero, missing, and poor quality", {
  prepared <- suppressWarnings(prepare_multilevel_mediation_data(
    make_mediation_data(),
    x_col = "condition",
    mediator_col = "dwell_ms",
    outcome_col = "override",
    quality_col = "valid_fraction",
    minimum_quality = 0.80
  ))
  expect_s3_class(prepared, "eye_multilevel_mediation_data")
  expect_equal(nrow(prepared$data), 12L)
  expect_equal(sum(prepared$data$mediation_mediator_true_zero), 1L)
  expect_equal(sum(prepared$data$mediation_mediator_state == "not_observed"), 1L)
  expect_equal(sum(prepared$data$mediation_mediator_state == "poor_quality"), 1L)
  expect_equal(sum(prepared$data$mediation_analysis_eligible), 10L)
  expect_identical(prepared$provenance$row_position_convention$base, 1L)
})

test_that("observation markers cannot use truthy strings", {
  data <- make_mediation_data()
  data$mediator_seen <- rep("False", nrow(data))
  expect_error(
    prepare_multilevel_mediation_data(
      data,
      x_col = "condition",
      mediator_col = "dwell_ms",
      outcome_col = "override",
      mediator_observed_col = "mediator_seen"
    ),
    "TRUE/FALSE or 0/1"
  )
})

test_that("quality values must be numeric", {
  data <- make_mediation_data()
  data$quality <- rep("1", nrow(data))
  data$quality[[1L]] <- "bad"
  expect_error(
    prepare_multilevel_mediation_data(
      data,
      x_col = "condition",
      mediator_col = "dwell_ms",
      outcome_col = "override",
      quality_col = "quality",
      minimum_quality = 0.8
    ),
    "must be numeric when used as a quality variable"
  )
})

test_that("preparation refuses to overwrite derived columns", {
  data <- make_mediation_data()
  data$X_within <- 999
  expect_error(
    prepare_multilevel_mediation_data(
      data,
      x_col = "condition",
      mediator_col = "dwell_ms",
      outcome_col = "override"
    ),
    "Derived mediation columns already exist"
  )
})

test_that("no within-X variation fails unless explicitly allowed", {
  data <- make_mediation_data()
  data$condition <- c(rep(0, 4), rep(1, 4), rep(0, 4))
  expect_error(
    prepare_multilevel_mediation_data(
      data,
      x_col = "condition",
      mediator_col = "dwell_ms",
      outcome_col = "override"
    ),
    "no within-participant variation"
  )
  prepared <- suppressWarnings(prepare_multilevel_mediation_data(
    data,
    x_col = "condition",
    mediator_col = "dwell_ms",
    outcome_col = "override",
    require_within_x = FALSE
  ))
  expect_equal(
    prepared$levels$level[prepared$levels$variable == "condition"],
    "between_only"
  )
})

test_that("cross-language fixture matches canonical semantic fields", {
  fixture <- utils::read.csv(
    testthat::test_path("fixtures", "multilevel_mediation_expected.csv"),
    stringsAsFactors = FALSE
  )
  prepared <- suppressWarnings(prepare_multilevel_mediation_data(
    make_mediation_data(),
    x_col = "condition",
    mediator_col = "dwell_ms",
    outcome_col = "override",
    quality_col = "valid_fraction",
    minimum_quality = 0.80
  ))
  actual <- prepared$data[names(fixture)]
  for (column in c("X_within", "X_between", "M_within", "M_between")) {
    expect_equal(actual[[column]], fixture[[column]], tolerance = 1e-12)
  }
  for (column in setdiff(names(fixture), c("X_within", "X_between", "M_within", "M_between"))) {
    expect_equal(as.character(actual[[column]]), as.character(fixture[[column]]))
  }
})

test_that("additional components stay in the preparation layer", {
  data <- make_mediation_data()
  data$trust <- c(3, 4, 3.2, 4.4, 2.8, 4.1, 3.1, 4.5, 3.3, 4.2, 3, 4.6)
  prepared <- suppressWarnings(prepare_multilevel_mediation_data(
    data, x_col = "condition", mediator_col = "dwell_ms", outcome_col = "override"
  ))
  augmented <- add_multilevel_mediation_component(
    prepared,
    value_col = "trust",
    semantic = "mediator2",
    within_col = "M2_within",
    between_col = "M2_between"
  )
  expect_true(all(c("M2_within", "M2_between") %in% names(augmented$data)))
  expect_identical(augmented$columns$mediator2, "trust")
})
