# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Computational benchmarking and synthetic measurement-stress tests.

#' Define a computational benchmark design
#' @param n_obs Observation counts.
#' @param repetitions Repetitions per size.
#' @param label Benchmark label.
#' @return A tabular R object containing define a computational benchmark design; rows represent analysis units and columns contain the returned quantities.
#' @export
eye_benchmark_design <- function(n_obs = c(1e4, 1e5, 1e6), repetitions = 3L,
                                 label = "eyeprocess_scaling") {
  n_obs <- as.integer(n_obs)
  repetitions <- as.integer(repetitions)[1L]
  if (!length(n_obs) || any(!is.finite(n_obs)) || any(n_obs < 1L)) stop("n_obs must contain positive integers.", call. = FALSE)
  if (!is.finite(repetitions) || repetitions < 1L) stop("repetitions must be a positive integer.", call. = FALSE)
  g <- expand.grid(n_obs = n_obs, repetition = seq_len(repetitions), KEEP.OUT.ATTRS = FALSE)
  g$benchmark_id <- sprintf("B%05d", seq_len(nrow(g)))
  g$label <- as.character(label)[1L]
  class(g) <- c("eye_benchmark_design", "data.frame")
  g[, c("benchmark_id", setdiff(names(g), "benchmark_id")), drop = FALSE]
}

.ep09_default_benchmark_generator <- function(n, row = NULL) {
  data.frame(
    person_id = rep(seq_len(max(1L, ceiling(n / 50))), each = 50, length.out = n),
    timestamp_ms = seq_len(n) * 1000 / 60,
    gaze_x = stats::runif(n), gaze_y = stats::runif(n),
    pupil = stats::rnorm(n, 3.5, .3), valid = stats::runif(n) > .05,
    stringsAsFactors = FALSE
  )
}

.ep09_default_benchmark_operation <- function(data, row = NULL) {
  c(mean_pupil = mean(data$pupil, na.rm = TRUE), valid_fraction = mean(data$valid, na.rm = TRUE))
}

#' Run a computational scaling benchmark
#' @param design Benchmark design.
#' @param generator Function `(n, row)` returning benchmark input.
#' @param operation Function `(data, row)` representing the operation under test.
#' @param gc_before Run garbage collection before timing.
#' @param progress Print progress.
#' @return An object of class "eye_benchmark_result", stored as a named list, with components "design", "results", "created_at", "status", "caveat". It contains a computational scaling benchmark and associated metadata or diagnostics needed to interpret the result.
#' @export
run_eye_benchmark <- function(design = eye_benchmark_design(),
                              generator = .ep09_default_benchmark_generator,
                              operation = .ep09_default_benchmark_operation,
                              gc_before = TRUE, progress = interactive()) {
  design <- .ep09_as_df(design); .ep09_req_cols(design, c("benchmark_id", "n_obs"), "design")
  if (!is.function(generator)) stop("generator must be a function.", call. = FALSE)
  if (!is.function(operation)) stop("operation must be a function.", call. = FALSE)
  rows <- lapply(seq_len(nrow(design)), function(i) {
    row <- design[i, , drop = FALSE]
    if (isTRUE(progress)) message("benchmark ", row$benchmark_id[[1L]], " n=", row$n_obs[[1L]])
    input_cap <- .ep09_capture(generator(as.integer(row$n_obs[[1L]]), row))
    if (!is.na(input_cap$error)) return(data.frame(benchmark_id = row$benchmark_id[[1L]], n_obs = row$n_obs[[1L]],
                                                   elapsed_sec = NA_real_, input_bytes = NA_real_, output_bytes = NA_real_,
                                                   status = "generator_error", error = input_cap$error, stringsAsFactors = FALSE))
    input <- input_cap$value
    if (isTRUE(gc_before)) invisible(gc())
    started <- proc.time()[[3L]]
    cap <- .ep09_capture(operation(input, row))
    elapsed <- proc.time()[[3L]] - started
    data.frame(
      benchmark_id = row$benchmark_id[[1L]], n_obs = row$n_obs[[1L]],
      repetition = if ("repetition" %in% names(row)) row$repetition[[1L]] else NA_integer_,
      elapsed_sec = elapsed,
      input_bytes = as.numeric(utils::object.size(input)),
      output_bytes = if (is.na(cap$error)) as.numeric(utils::object.size(cap$value)) else NA_real_,
      status = if (is.na(cap$error)) "success" else "operation_error",
      error = cap$error,
      stringsAsFactors = FALSE
    )
  })
  structure(list(
    design = design,
    results = .ep09_rbind_fill(rows),
    created_at = as.character(Sys.time()),
    status = "computational_benchmark",
    caveat = "Benchmark timings are hardware-, operating-system-, R-version-, and workload-dependent."
  ), class = "eye_benchmark_result")
}

#' Summarise benchmark timing and memory by problem size
#' @param x Benchmark result.
#' @return A logical value or vector indicating benchmark timing and memory by problem size.
#' @export
summarise_eye_benchmark <- function(x) {
  if (!inherits(x, "eye_benchmark_result")) stop("x must be an eye_benchmark_result.", call. = FALSE)
  d <- x$results; d <- d[d$status == "success", , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  sizes <- sort(unique(d$n_obs))
  .ep09_rbind_fill(lapply(sizes, function(n) {
    z <- d[d$n_obs == n, , drop = FALSE]
    med_elapsed <- stats::median(z$elapsed_sec, na.rm = TRUE)
    data.frame(n_obs = n, runs = nrow(z), median_elapsed_sec = med_elapsed,
               min_elapsed_sec = min(z$elapsed_sec, na.rm = TRUE), max_elapsed_sec = max(z$elapsed_sec, na.rm = TRUE),
               median_input_mb = stats::median(z$input_bytes, na.rm = TRUE) / 1024^2,
               median_output_mb = stats::median(z$output_bytes, na.rm = TRUE) / 1024^2,
               throughput_obs_per_sec = if (is.finite(med_elapsed) && med_elapsed > 0) n / med_elapsed else NA_real_, stringsAsFactors = FALSE)
  }))
}

#' Estimate scaling exponent from benchmark results
#' @param x Benchmark result.
#' @return A data frame containing scaling exponent from benchmark results. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
benchmark_scaling_curve <- function(x) {
  s <- summarise_eye_benchmark(x)
  s <- s[is.finite(s$median_elapsed_sec) & s$median_elapsed_sec > 0 & s$n_obs > 0, , drop = FALSE]
  if (nrow(s) < 2L) return(data.frame(exponent = NA_real_, intercept = NA_real_, n_sizes = nrow(s)))
  fit <- stats::lm(log(median_elapsed_sec) ~ log(n_obs), data = s)
  data.frame(exponent = unname(stats::coef(fit)[[2L]]), intercept = unname(stats::coef(fit)[[1L]]), n_sizes = nrow(s), stringsAsFactors = FALSE)
}

#' Memory estimate for an R object or generated problem size
#' @param x Object, or numeric n when `generator` is supplied.
#' @param generator Optional function taking n.
#' @return A data frame containing memory estimate for an R object or generated problem size. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
benchmark_memory_estimate <- function(x, generator = NULL) {
  obj <- if (is.function(generator)) generator(as.integer(x)[1L]) else x
  bytes <- as.numeric(utils::object.size(obj))
  data.frame(bytes = bytes, kb = bytes / 1024, mb = bytes / 1024^2, gb = bytes / 1024^3, stringsAsFactors = FALSE)
}

#' Define synthetic measurement corruptions for stress testing
#' @param missingness Generic missingness proportion.
#' @param pupil_dropout Pupil dropout proportion.
#' @param gaze_offset_x,gaze_offset_y Additive gaze-coordinate offsets.
#' @param sampling_jitter_sd Timestamp jitter SD in timestamp units.
#' @param aoi_label_noise Proportion of AOI labels randomly reassigned.
#' @param device_shift Additive shift for a declared device-sensitive numeric column.
#' @param trial_drop Proportion of rows/trials removed.
#' @param seed Seed.
#' @return An object of class "eye_synthetic_corruption_plan", stored as a named list, with components "missingness", "pupil_dropout", "gaze_offset_x", "gaze_offset_y", "sampling_jitter_sd", "aoi_label_noise", "device_shift", "trial_drop", "seed", "status". It contains define synthetic measurement corruptions for stress testing and associated metadata or diagnostics needed to interpret the result.
#' @export
synthetic_corruption_plan <- function(missingness = 0, pupil_dropout = 0,
                                      gaze_offset_x = 0, gaze_offset_y = 0,
                                      sampling_jitter_sd = 0, aoi_label_noise = 0,
                                      device_shift = 0, trial_drop = 0, seed = 1L) {
  probs <- c(missingness, pupil_dropout, aoi_label_noise, trial_drop)
  if (any(!is.finite(probs)) || any(probs < 0 | probs >= 1)) stop("Corruption proportions must lie in [0, 1).", call. = FALSE)
  shifts <- c(gaze_offset_x, gaze_offset_y, sampling_jitter_sd, device_shift)
  if (any(!is.finite(shifts))) stop("Corruption offsets, jitter, and device shift must be finite.", call. = FALSE)
  if (sampling_jitter_sd < 0) stop("sampling_jitter_sd must be non-negative.", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  structure(list(
    missingness = missingness, pupil_dropout = pupil_dropout,
    gaze_offset_x = gaze_offset_x, gaze_offset_y = gaze_offset_y,
    sampling_jitter_sd = sampling_jitter_sd, aoi_label_noise = aoi_label_noise,
    device_shift = device_shift, trial_drop = trial_drop,
    seed = as.integer(seed)[1L], status = "synthetic_measurement_corruption"
  ), class = "eye_synthetic_corruption_plan")
}

#' Inject generic missingness into selected columns
#' @param data Data.
#' @param columns Columns.
#' @param proportion Missingness proportion.
#' @param seed Seed.
#' @return An R object containing inject generic missingness into selected columns. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
inject_eye_missingness <- function(data, columns, proportion, seed = 1L) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, columns, "data")
  if (length(proportion) != 1L || !is.finite(proportion) || proportion < 0 || proportion >= 1) stop("proportion must lie in [0, 1).", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  for (nm in columns) {
    idx <- stats::runif(nrow(d)) < proportion
    d[[nm]][idx] <- NA
  }
  d
}

#' Inject pupil dropout
#' @param data Data.
#' @param pupil Pupil column.
#' @param proportion Dropout proportion.
#' @param seed Seed.
#' @return An R object containing inject pupil dropout. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
inject_pupil_dropout <- function(data, pupil = "pupil", proportion, seed = 1L) {
  inject_eye_missingness(data, pupil, proportion, seed)
}

#' Inject additive gaze calibration offset in coordinate units
#' @param data Data.
#' @param x,y Gaze coordinate columns.
#' @param offset_x,offset_y Additive offsets in the same units as x/y.
#' @return An R object containing inject additive gaze calibration offset in coordinate units. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
inject_calibration_offset <- function(data, x = "gaze_x", y = "gaze_y", offset_x = 0, offset_y = 0) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(x, y), "data")
  if (length(offset_x) != 1L || length(offset_y) != 1L || !is.finite(offset_x) || !is.finite(offset_y)) stop("offset_x and offset_y must be finite scalars.", call. = FALSE)
  d[[x]] <- .ep09_num(d[[x]]) + offset_x; d[[y]] <- .ep09_num(d[[y]]) + offset_y
  d
}

#' Inject timestamp jitter
#' @param data Data.
#' @param time Timestamp column.
#' @param sd Jitter standard deviation in timestamp units.
#' @param seed Seed.
#' @return An R object containing inject timestamp jitter. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
inject_sampling_jitter <- function(data, time = "timestamp_ms", sd, seed = 1L) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, time, "data")
  if (length(sd) != 1L || !is.finite(sd) || sd < 0) stop("sd must be a finite non-negative scalar.", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  d[[time]] <- .ep09_num(d[[time]]) + stats::rnorm(nrow(d), sd = sd)
  d
}

#' Inject AOI label noise
#' @param data Data.
#' @param aoi AOI label column.
#' @param proportion Proportion reassigned to another observed label.
#' @param seed Seed.
#' @return An R object containing inject AOI label noise. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
inject_aoi_label_noise <- function(data, aoi = "aoi", proportion, seed = 1L) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, aoi, "data")
  if (length(proportion) != 1L || !is.finite(proportion) || proportion < 0 || proportion >= 1) stop("proportion must lie in [0, 1).", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  lev <- unique(as.character(d[[aoi]][!is.na(d[[aoi]])]))
  if (length(lev) < 2L) return(d)
  idx <- which(!is.na(d[[aoi]]) & stats::runif(nrow(d)) < proportion)
  for (i in idx) {
    cur <- as.character(d[[aoi]][i]); choices <- setdiff(lev, cur)
    d[[aoi]][i] <- sample(choices, 1L)
  }
  d
}

#' Inject an additive device/site shift in a numeric feature
#' @param data Data.
#' @param column Numeric column.
#' @param shift Additive shift.
#' @param rows Optional logical/index rows affected.
#' @return An R object containing inject an additive device/site shift in a numeric feature. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
inject_device_shift <- function(data, column, shift, rows = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, column, "data")
  if (length(shift) != 1L || !is.finite(shift)) stop("shift must be a finite scalar.", call. = FALSE)
  if (is.null(rows)) rows <- seq_len(nrow(d))
  if (is.logical(rows) && length(rows) != nrow(d)) stop("Logical rows must have length nrow(data).", call. = FALSE)
  d[[column]][rows] <- .ep09_num(d[[column]][rows]) + shift
  d
}

#' Inject trial/row imbalance by dropping observations
#' @param data Data.
#' @param proportion Proportion dropped.
#' @param seed Seed.
#' @return A tabular R object containing inject trial/row imbalance by dropping observations; rows represent analysis units and columns contain the returned quantities.
#' @export
inject_trial_imbalance <- function(data, proportion, seed = 1L) {
  d <- .ep09_as_df(data)
  if (length(proportion) != 1L || !is.finite(proportion) || proportion < 0 || proportion >= 1) stop("proportion must lie in [0, 1).", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  keep <- stats::runif(nrow(d)) >= proportion
  d[keep, , drop = FALSE]
}

#' Apply a synthetic corruption plan
#' @param data Data frame.
#' @param plan Corruption plan.
#' @param gaze_columns Gaze columns receiving generic missingness/offset.
#' @param pupil Pupil column.
#' @param time Timestamp column.
#' @param aoi Optional AOI column.
#' @param device_column Optional numeric column receiving device shift.
#' @return An R object containing a synthetic corruption plan. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
apply_synthetic_corruption <- function(data, plan,
                                       gaze_columns = c("gaze_x", "gaze_y"), pupil = "pupil",
                                       time = "timestamp_ms", aoi = NULL, device_column = NULL) {
  if (!inherits(plan, "eye_synthetic_corruption_plan")) stop("plan must be an eye_synthetic_corruption_plan.", call. = FALSE)
  d <- .ep09_as_df(data); seed <- plan$seed
  if (plan$trial_drop > 0) d <- inject_trial_imbalance(d, plan$trial_drop, seed = seed + 1L)
  gc <- intersect(gaze_columns, names(d))
  if (length(gc) && plan$missingness > 0) d <- inject_eye_missingness(d, gc, plan$missingness, seed = seed + 2L)
  if (length(gc) >= 2L && (plan$gaze_offset_x != 0 || plan$gaze_offset_y != 0))
    d <- inject_calibration_offset(d, gc[[1L]], gc[[2L]], plan$gaze_offset_x, plan$gaze_offset_y)
  if (pupil %in% names(d) && plan$pupil_dropout > 0) d <- inject_pupil_dropout(d, pupil, plan$pupil_dropout, seed = seed + 3L)
  if (time %in% names(d) && plan$sampling_jitter_sd > 0) d <- inject_sampling_jitter(d, time, plan$sampling_jitter_sd, seed = seed + 4L)
  if (!is.null(aoi) && aoi %in% names(d) && plan$aoi_label_noise > 0)
    d <- inject_aoi_label_noise(d, aoi, plan$aoi_label_noise, seed = seed + 5L)
  if (!is.null(device_column) && device_column %in% names(d) && plan$device_shift != 0)
    d <- inject_device_shift(d, device_column, plan$device_shift)
  attr(d, "eyeprocess_corruption_plan") <- plan
  d
}

#' Stress-test an analysis under explicit synthetic corruptions
#' @param data Baseline data.
#' @param plans List of corruption plans.
#' @param analysis_fun Function `(data, plan)`.
#' @param metric_fun Function `(analysis_result, plan)` returning scalar/list/data.frame metrics.
#' @param ... Passed to `apply_synthetic_corruption()`.
#' @return An object of class "eye_process_stress_test", stored as a named list, with components "plans", "results", "baseline_hash", "created_at", "caveat". It contains stress-test an analysis under explicit synthetic corruptions and associated metadata or diagnostics needed to interpret the result.
#' @export
stress_test_process_pipeline <- function(data, plans, analysis_fun,
                                         metric_fun = .ep09_default_sensitivity_extract, ...) {
  if (!is.list(plans) || !length(plans)) stop("plans must be a non-empty list of corruption plans.", call. = FALSE)
  if (!is.function(analysis_fun)) stop("analysis_fun must be a function.", call. = FALSE)
  if (!is.function(metric_fun)) stop("metric_fun must be a function.", call. = FALSE)
  rows <- lapply(seq_along(plans), function(i) {
    p <- plans[[i]]
    corrupt <- .ep09_capture(apply_synthetic_corruption(data, p, ...))
    if (!is.na(corrupt$error)) return(data.frame(plan_id = i, status = "corruption_error", error = corrupt$error, stringsAsFactors = FALSE))
    corrupted <- corrupt$value
    cap <- .ep09_capture(analysis_fun(corrupted, p))
    if (!is.na(cap$error)) return(data.frame(plan_id = i, status = "analysis_error", error = cap$error, stringsAsFactors = FALSE))
    met <- .ep09_capture(metric_fun(cap$value, p))
    if (!is.na(met$error)) return(data.frame(plan_id = i, status = "metric_error", error = met$error, stringsAsFactors = FALSE))
    z <- .ep09_as_df(met$value)
    if (!nrow(z)) return(data.frame(plan_id = i, status = "empty_metric", error = NA_character_, stringsAsFactors = FALSE))
    z$plan_id <- i; z$status <- "success"
    z$missingness <- p$missingness; z$pupil_dropout <- p$pupil_dropout
    z$gaze_offset <- sqrt(p$gaze_offset_x^2 + p$gaze_offset_y^2)
    z$sampling_jitter_sd <- p$sampling_jitter_sd; z$aoi_label_noise <- p$aoi_label_noise
    z$device_shift <- p$device_shift; z$trial_drop <- p$trial_drop
    z
  })
  structure(list(
    plans = plans, results = .ep09_rbind_fill(rows),
    baseline_hash = .ep09_hash_object(data), created_at = as.character(Sys.time()),
    caveat = "Synthetic stress tests probe declared perturbations only and do not replace empirical external validation."
  ), class = "eye_process_stress_test")
}

#' Summarise stress-test metrics
#' @param x Stress-test result.
#' @param metric Numeric metric column.
#' @return A data frame containing stress-test metrics. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
stress_test_summary <- function(x, metric = "effect") {
  if (!inherits(x, "eye_process_stress_test")) stop("x must be an eye_process_stress_test.", call. = FALSE)
  d <- x$results; .ep09_req_cols(d, metric, "x$results")
  m <- .ep09_num(d[[metric]]); mf <- m[is.finite(m)]
  data.frame(plans = nrow(d), successful = sum(d$status == "success", na.rm = TRUE),
             median = if (length(mf)) stats::median(mf) else NA_real_,
             min = if (length(mf)) min(mf) else NA_real_,
             max = if (length(mf)) max(mf) else NA_real_,
             stringsAsFactors = FALSE)
}

#' Identify the empirical stress frontier for a metric
#' @param x Stress-test result.
#' @param severity Numeric corruption/severity column.
#' @param metric Metric column.
#' @param acceptable Function returning TRUE/FALSE for metric values.
#' @return A data frame containing the empirical stress frontier for a metric. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
stress_tolerance_frontier <- function(x, severity, metric, acceptable) {
  if (!is.function(acceptable)) stop("acceptable must be a function.", call. = FALSE)
  d <- x$results; .ep09_req_cols(d, c(severity, metric), "x$results")
  ok <- vapply(d[[metric]], acceptable, logical(1))
  sev <- .ep09_num(d[[severity]])
  known <- !is.na(ok) & is.finite(sev)
  data.frame(max_acceptable_severity = if (any(known & ok)) max(sev[known & ok]) else NA_real_,
             first_unacceptable_severity = if (any(known & !ok)) min(sev[known & !ok]) else NA_real_,
             stringsAsFactors = FALSE)
}

#' @export
print.eye_benchmark_result <- function(x, ...) {
  cat("eyeprocess computational benchmark\n")
  cat("  runs   :", nrow(x$results), "\n")
  cat("  success:", sum(x$results$status == "success"), "\n")
  invisible(x)
}

#' @export
print.eye_process_stress_test <- function(x, ...) {
  cat("eyeprocess synthetic measurement stress test\n")
  cat("  plans  :", length(x$plans), "\n")
  cat("  success:", sum(x$results$status == "success"), "\n")
  invisible(x)
}
