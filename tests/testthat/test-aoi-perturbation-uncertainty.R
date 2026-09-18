test_that("rectangle and convex polygon perturbations are explicit", {
  aois <- data.frame(
    aoi_id = c("headline", "cta"),
    xmin = c(100, 600), xmax = c(400, 900),
    ymin = c(80, 500), ymax = c(180, 620)
  )
  d <- dilate_aoi(aois, 10)
  expect_equal(d$xmin[1], 90)
  expect_equal(d$xmax[1], 410)
  e <- erode_aoi(aois, 10)
  expect_equal(e$xmin[1], 110)
  expect_equal(e$xmax[1], 390)

  poly <- data.frame(aoi_id = "diamond", shape_type = "polygon")
  poly$polygon <- I(list(matrix(c(500,200, 560,260, 500,320, 440,260), ncol = 2, byrow = TRUE)))
  pd <- dilate_aoi(poly, 10)
  expect_true(diff(range(pd$polygon[[1]][,1])) > diff(range(poly$polygon[[1]][,1])))
  expect_true(diff(range(pd$polygon[[1]][,2])) > diff(range(poly$polygon[[1]][,2])))
})

test_that("polygon degree dilation respects both axis scales", {
  square <- data.frame(aoi_id = "square", shape_type = "polygon")
  square$polygon <- I(list(matrix(
    c(100,100, 200,100, 200,200, 100,200),
    ncol = 2, byrow = TRUE
  )))
  out <- dilate_aoi(
    square, .5,
    unit = "deg",
    degrees_per_pixel = c(.05, .10)
  )
  p <- out$polygon[[1]]
  expect_equal(range(p[,1]), c(90,210), tolerance = 1e-8)
  expect_equal(range(p[,2]), c(95,205), tolerance = 1e-8)
})

test_that("polygon overlap and boundary membership are deterministic", {
  polygons <- data.frame(
    aoi_id = c("left", "right"),
    shape_type = c("polygon", "polygon")
  )
  polygons$polygon <- I(list(
    matrix(c(0,0, 10,0, 10,10, 0,10), ncol = 2, byrow = TRUE),
    matrix(c(9.5,-2, 12,5, 9.5,12), ncol = 2, byrow = TRUE)
  ))
  validation <- validate_aoi_geometry(polygons)
  expect_true(validation$overlap_present)
  expect_error(validate_aoi_geometry(polygons, allow_overlap = FALSE), "overlap")

  single <- polygons[1, , drop = FALSE]
  data <- data.frame(
    x = c(0, 10, 5),
    y = c(5, 5, 0),
    duration = c(1, 1, 1)
  )
  result <- run_aoi_sensitivity_analysis(
    data,
    single,
    create_aoi_perturbation_grid(include_baseline = TRUE),
    x_col = "x",
    y_col = "y",
    duration_col = "duration"
  )
  expect_equal(result$assignments$baseline, c("left", "left", "left"))
})

test_that("pathological geometry is never silently repaired", {
  aois <- data.frame(aoi_id = "a", xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  expect_error(erode_aoi(aois, 20), "collapsed")

  zero <- data.frame(aoi_id = "a", xmin = 0, xmax = 0, ymin = 0, ymax = 1)
  expect_error(validate_aoi_geometry(zero), "zero or negative")

  bow <- data.frame(aoi_id = "bow", shape_type = "polygon")
  bow$polygon <- I(list(matrix(c(0,0, 2,2, 0,2, 2,0), ncol = 2, byrow = TRUE)))
  expect_error(validate_aoi_geometry(bow), "self-intersects")

  concave <- data.frame(aoi_id = "concave", shape_type = "polygon")
  concave$polygon <- I(list(matrix(c(0,0, 4,0, 2,1, 4,4, 0,4), ncol = 2, byrow = TRUE)))
  expect_error(dilate_aoi(concave, .2), "convex polygons only")
})

test_that("screen edge policy and overlap ambiguity are explicit", {
  edge <- data.frame(aoi_id = "a", xmin = 5, xmax = 35, ymin = 10, ymax = 20)
  expect_warning(
    translate_aoi(edge, x = -10, screen_width_px = 100, screen_height_px = 100, boundary_policy = "warn"),
    "extend beyond"
  )
  expect_error(
    translate_aoi(edge, x = -10, screen_width_px = 100, screen_height_px = 100, boundary_policy = "error"),
    "extend beyond"
  )
  clipped <- expect_warning(
    translate_aoi(edge, x = -10, screen_width_px = 100, screen_height_px = 100, boundary_policy = "clip"),
    "clipping"
  )
  expect_equal(clipped$xmin, 0)

  overlap <- data.frame(
    aoi_id = c("a", "b"), xmin = c(0, 5), xmax = c(10, 15),
    ymin = c(0, 0), ymax = c(10, 10)
  )
  audit <- validate_aoi_geometry(overlap)
  expect_true(audit$overlap_present)
  expect_error(validate_aoi_geometry(overlap, allow_overlap = FALSE), "overlap")
})

test_that("pixel degree conversion round trips and never guesses missing geometry", {
  args <- list(
    screen_width_px = 1920, screen_height_px = 1080,
    viewing_distance = 60, physical_screen_size = c(53.1, 29.9)
  )
  deg <- do.call(convert_aoi_margin_to_degrees, c(list(margin_pixels = c(20, 30)), args))
  px <- do.call(convert_aoi_margin_to_pixels, c(list(margin_degrees = deg), args))
  expect_equal(px, c(20, 30), tolerance = 1e-10)
  expect_error(aoi_perturbation_spec("x", "dilation", margin_x = .25, unit = "deg"), "require")
})

test_that("grid accepts pair inputs and combined xy translation", {
  grid <- create_aoi_perturbation_grid(
    dilations = c(1, 2),
    translations_xy = matrix(c(3, -4, -1, 2), ncol = 2, byrow = TRUE),
    anisotropic = c(.5, -.25)
  )
  expect_equal(
    grid$table$perturbation_id,
    c(
      "baseline",
      "dilate_1_px",
      "dilate_2_px",
      "shift_xy_3_-4_px",
      "shift_xy_-1_2_px",
      "anisotropic_0.5_-0.25_px"
    )
  )

  aois <- data.frame(aoi_id = "a", xmin = 100, xmax = 400, ymin = 80, ymax = 180)
  applied <- apply_aoi_perturbation_grid(aois, grid)
  shifted <- applied$geometries[["shift_xy_3_-4_px"]]
  expect_equal(shifted$xmin, 103)
  expect_equal(shifted$ymin, 76)
})

test_that("seeded jitter is reproducible and does not leak RNG state", {
  aois <- data.frame(aoi_id = "a", xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  set.seed(99)
  before <- .Random.seed
  a <- jitter_aoi(aois, 2, seed = 4)
  after <- .Random.seed
  b <- jitter_aoi(aois, 2, seed = 4)
  c <- jitter_aoi(aois, 2, seed = 5)
  expect_equal(a, b)
  expect_false(isTRUE(all.equal(a, c)))
  expect_equal(after, before)
})

test_that("baseline identity, assignment matrices, and missingness are preserved", {
  aois <- data.frame(aoi_id = "a", xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  baseline <- perturb_aoi_geometry(aois, aoi_perturbation_spec("baseline", "baseline"))
  expect_equal(baseline$nominal_geometry, baseline$perturbed_geometry)

  cmp <- compare_aoi_assignments(
    c("a", "__outside__", "b", NA),
    c("a", "a", "__outside__", "b")
  )
  expect_equal(cmp$summary$n_comparable, 3)
  expect_equal(cmp$summary$proportion_unchanged, 1/3)
  expect_equal(cmp$summary$proportion_newly_assigned, 1/3)
  expect_equal(cmp$summary$proportion_lost, 1/3)
  expect_equal(unname(cmp$reassignment_matrix["a", "a"]), 1)
})

test_that("participant trial and AOI stability are available", {
  a <- compare_aoi_assignments(c("a","a","b","b"), c("a","b","b","b"))
  b <- compare_aoi_assignments(c("a","a","b","b"), c("a","a","a","b"))
  metadata <- data.frame(
    observation_id = 1:4,
    participant = c("p1","p1","p2","p2"),
    trial = c(1,2,1,2)
  )
  out <- estimate_aoi_assignment_stability(
    list(x = a, y = b), metadata = metadata,
    group_cols = c("participant", "trial")
  )
  expect_true(all(c("participant", "trial") %in% names(out$group_summaries)))
  expect_gt(nrow(out$aoi_level), 0)

  freq <- estimate_fixation_assignment_probability(
    list(baseline = c("a","b"), p1 = c("a","a"), p2 = c("b","b"))
  )
  hit <- freq[freq$observation_id == 1 & freq$aoi == "a", ]
  expect_equal(hit$assignment_frequency, 2/3)
  expect_match(attr(freq, "caveat"), "not a posterior")
})

test_that("feature recomputation preserves zero cells and missing denominators", {
  d <- data.frame(
    participant = c(1,1,1,2,2),
    trial = c(1,1,1,1,1),
    time = c(.1,.2,.3,NA,NA),
    duration = c(.1,NA,.1,NA,NA)
  )
  assignments <- c("a","a","__outside__",NA,NA)

  complete <- recompute_aoi_features(
    d, assignments,
    participant_col = "participant", trial_col = "trial",
    duration_col = "duration", time_col = "time",
    aoi_levels = c("a", "b")
  )
  p1a <- complete[complete$participant == 1 & complete$aoi == "a", , drop = FALSE]
  p1b <- complete[complete$participant == 1 & complete$aoi == "b", , drop = FALSE]
  p2a <- complete[complete$participant == 2 & complete$aoi == "a", , drop = FALSE]

  expect_equal(p1a$fixation_count, 2)
  expect_true(p1a$inspected)
  expect_true(is.na(p1a$dwell))
  expect_false(p1a$duration_complete)
  expect_equal(p1b$fixation_count, 0)
  expect_false(p1b$inspected)
  expect_equal(p1b$dwell, 0)
  expect_true(p1b$duration_complete)
  expect_true(is.na(p1b$first_fixation))
  expect_true(p1b$time_complete)
  expect_true(is.na(p2a$fixation_count))
  expect_true(is.na(p2a$inspected))
  expect_equal(p2a$n_valid_observations, 0)
  expect_equal(p2a$n_missing_observations, 2)

  expect_warning(
    recompute_aoi_features(
      d, assignments,
      participant_col = "participant", trial_col = "trial",
      time_col = "time", aoi_levels = c("a", "b")
    ),
    "dwell is returned as NA"
  )
  expect_warning(
    recompute_aoi_features(
      d, assignments,
      participant_col = "participant", trial_col = "trial",
      duration_col = "duration", aoi_levels = c("a", "b")
    ),
    "first_fixation is returned as NA"
  )
})

.aoi_test_data <- function() {
  set.seed(12)
  rows <- list()
  centers <- data.frame(
    x = c(250,500,500,500,800), y = c(130,270,430,560,650),
    aoi = c("headline","image","claim","disclosure","cta")
  )
  k <- 0L
  for (participant in 1:8) for (trial in 1:3) for (j in seq_len(nrow(centers))) {
    condition <- participant %% 2
    n <- 5L + if (centers$aoi[j] == "disclosure" && condition == 1) 3L else 0L
    for (fix in seq_len(n)) {
      k <- k + 1L
      rows[[k]] <- data.frame(
        obs = k, participant = paste0("p", participant), trial = trial,
        condition = condition,
        x = rnorm(1, centers$x[j], 35), y = rnorm(1, centers$y[j], 22),
        duration = runif(1, .06, .18), time = j - 1 + (fix - 1) / 20
      )
    }
  }
  do.call(rbind, rows)
}

.aoi_test_aois <- function() {
  data.frame(
    aoi_id = c("headline","image","claim","disclosure","cta"),
    xmin = c(100,300,300,300,680), xmax = c(400,700,700,700,920),
    ymin = c(80,190,360,500,600), ymax = c(180,340,480,590,710)
  )
}

.aoi_test_model <- function(features, assigned, spec) {
  target <- features[features$aoi == "disclosure", , drop = FALSE]
  map <- unique(assigned[, c("participant", "condition")])
  target <- merge(target, map, by = "participant", all.x = TRUE)
  fit <- stats::lm(dwell ~ condition, data = target)
  co <- summary(fit)$coefficients
  if (!"condition" %in% rownames(co)) stop("condition coefficient unavailable")
  est <- unname(co["condition","Estimate"]); se <- unname(co["condition","Std. Error"])
  data.frame(
    term = "condition", estimate = est, SE = se,
    CI_low = est - 1.96 * se, CI_high = est + 1.96 * se,
    p_value = unname(co["condition","Pr(>|t|)"]),
    model_converged = TRUE, N = stats::nobs(fit)
  )
}

test_that("sensitivity features keep all trial by AOI cells", {
  data <- .aoi_test_data()
  aois <- .aoi_test_aois()
  result <- run_aoi_sensitivity_analysis(
    data, aois,
    create_aoi_perturbation_grid(include_baseline = TRUE),
    x_col = "x", y_col = "y", observation_id_col = "obs",
    participant_col = "participant", trial_col = "trial",
    duration_col = "duration", time_col = "time"
  )
  baseline <- result$features$baseline
  expect_equal(nrow(baseline), 8 * 3 * 5)
  expect_setequal(baseline$aoi, aois$aoi_id)
  expect_true(all(baseline$n_valid_observations > 0))
})

test_that("full synthetic sensitivity pipeline propagates inference and provenance", {
  viewing <- list(
    screen_width_px = 1024, screen_height_px = 768,
    viewing_distance = 60, physical_screen_size = c(53.1, 29.9),
    boundary_policy = "allow"
  )
  grid <- do.call(create_aoi_perturbation_grid, c(list(
    dilations = c(.25,.5,1), erosions = .25,
    translations_x = .5, translations_y = .5,
    unit = "deg", include_baseline = TRUE
  ), viewing))
  result <- run_aoi_sensitivity_analysis(
    .aoi_test_data(), .aoi_test_aois(), grid,
    x_col = "x", y_col = "y", observation_id_col = "obs",
    participant_col = "participant", trial_col = "trial",
    duration_col = "duration", time_col = "time",
    model_callback = .aoi_test_model,
    preprocessing_specification = list(duration_unit = "seconds"),
    event_detector = "synthetic_fixations",
    quality_rules = list(missing = "preserve"),
    model_specification = list(family = "lm", outcome = "disclosure_dwell")
  )
  expect_s3_class(result, "eye_aoi_sensitivity")
  expect_equal(nrow(result$grid_result$audit), 7)
  expect_true(all(result$grid_result$audit$status == "completed"))
  expect_true(all(c("term","estimate","SE","CI_low","CI_high","p_value","model_converged","N","direction") %in% names(result$models)))
  expect_equal(result$provenance$event_detector, "synthetic_fixations")
  inf <- assess_aoi_inference_stability(result, "condition")
  expect_equal(inf$n_models, 7)
  expect_match(report_aoi_sensitivity(result), "not probabilities")
})

test_that("model callback failures and nonconvergence remain visible", {
  data <- head(.aoi_test_data(), 40)
  grid <- create_aoi_perturbation_grid(dilations = 5, translations_x = 5)
  callback <- function(features, assigned, spec) {
    if (spec$perturbation_id == "dilate_5_px") stop("planned failure")
    data.frame(
      term = "x", estimate = 1, SE = .2, CI_low = .6, CI_high = 1.4,
      p_value = .03, model_converged = spec$perturbation_id != "shift_x_5_px", N = 10
    )
  }
  result <- run_aoi_sensitivity_analysis(
    data, .aoi_test_aois(), grid, x_col = "x", y_col = "y",
    duration_col = "duration", model_callback = callback
  )
  expect_true(any(result$failures$stage == "model" & grepl("planned failure", result$failures$message)))
  bad <- result$models[result$models$perturbation_id == "shift_x_5_px", ]
  expect_false(bad$model_converged)
  expect_equal(assess_aoi_inference_stability(result, "x")$n_converged, 1)
})

test_that("cross-language parity fixture preserves semantic contract", {
  path <- system.file("extdata", "aoi_perturbation_parity.csv", package = "eyeprocess")
  fixture <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  aois <- data.frame(
    aoi_id = c("a","b"), xmin = c(0,12), xmax = c(10,22),
    ymin = c(0,0), ymax = c(10,10)
  )
  grid <- create_aoi_perturbation_grid(dilations = 1)
  data <- fixture[, c("x","y")]; data$duration <- 1
  result <- run_aoi_sensitivity_analysis(
    data, aois, grid, x_col = "x", y_col = "y", duration_col = "duration"
  )
  expect_equal(result$assignments$baseline, fixture$baseline)
  expect_equal(result$assignments$dilate_1_px, fixture$dilate_1_px)
  cmp <- result$comparisons$dilate_1_px$summary
  expect_equal(cmp$n_comparable, 4)
  expect_equal(cmp$proportion_unchanged, .5)
  expect_equal(cmp$proportion_newly_assigned, .5)
})

test_that("AOI perturbation plots render", {
  data <- .aoi_test_data()
  grid <- create_aoi_perturbation_grid(
    anisotropic = list(c(-.25,-.25), c(0,.25), c(.25,0), c(.5,.5)),
    include_baseline = TRUE
  )
  result <- run_aoi_sensitivity_analysis(
    data, .aoi_test_aois(), grid, x_col = "x", y_col = "y",
    participant_col = "participant", trial_col = "trial",
    duration_col = "duration", time_col = "time",
    model_callback = .aoi_test_model
  )
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  expect_silent(plot_aoi_perturbations(result, perturbation_id = "anisotropic_0.25_0_px", data = head(data, 30), x_col = "x", y_col = "y"))
  expect_silent(plot_aoi_assignment_stability(result))
  expect_silent(plot_aoi_coefficient_stability(result, "condition"))
  expect_silent(plot_aoi_robustness_surface(result))
})
