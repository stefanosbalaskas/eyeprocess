# eyeprocess 0.9.0.9000 ------------------------------------------------------
# SBC diagnostics, measurement-resolution guards, and pupil preprocessing audits.

#' Compute a simulation-based calibration rank statistic
#'
#' @param truth Scalar simulated truth.
#' @param draws Posterior draws for the same parameter.
#' @param seed Seed used only to randomize ties.
#' @return A numeric value or vector containing a simulation-based calibration rank statistic.
#' @export
simulation_rank_statistic <- function(truth, draws, seed = NULL) {
  truth <- .ep09_num(truth)[1L]; draws <- .ep09_num(draws); draws <- draws[is.finite(draws)]
  if (!is.finite(truth) || !length(draws)) return(NA_integer_)
  if (!is.null(seed)) {
    seed <- .ep09_num(seed)[1L]
    if (!is.finite(seed) || seed < 0) stop("seed must be a non-negative finite scalar.", call. = FALSE)
    seed <- as.integer(seed %% .Machine$integer.max)
  }
  less <- sum(draws < truth); equal <- sum(draws == truth)
  if (equal && !is.null(seed)) set.seed(seed)
  less + if (equal) sample.int(equal + 1L, 1L) - 1L else 0L
}

#' Build SBC rank diagnostics
#'
#' @param ranks Integer rank statistics from 0 through `n_draws`.
#' @param n_draws Number of posterior draws used per rank.
#' @param bins Histogram bins; defaults to a bounded square-root rule.
#' @return `eye_sbc_diagnostics` object.
#' @export
sbc_rank_diagnostics <- function(ranks, n_draws, bins = NULL) {
  ranks0 <- .ep09_num(ranks); n_draws0 <- .ep09_num(n_draws)[1L]
  if (!is.finite(n_draws0) || n_draws0 < 1L || n_draws0 != round(n_draws0)) stop("n_draws must be a positive integer.", call. = FALSE)
  if (any(is.finite(ranks0) & ranks0 != round(ranks0))) stop("ranks must be integer-valued.", call. = FALSE)
  ranks <- as.integer(ranks0); n_draws <- as.integer(n_draws0)
  ranks <- ranks[!is.na(ranks)]
  if (any(ranks < 0L | ranks > n_draws)) stop("ranks must lie between 0 and n_draws inclusive.", call. = FALSE)
  if (is.null(bins)) bins <- max(5L, min(20L, round(sqrt(max(1L, length(ranks))))))
  bins <- as.integer(bins)[1L]
  if (!is.finite(bins) || bins < 2L) stop("bins must be an integer >= 2.", call. = FALSE)
  bins <- min(bins, n_draws + 1L)
  breaks <- seq(-.5, n_draws + .5, length.out = bins + 1L)
  h <- hist(ranks, breaks = breaks, plot = FALSE, include.lowest = TRUE)
  # Expected bin frequencies follow the discrete rank support 0:n_draws.
  # This remains correct when the number of rank categories is not divisible by bins.
  support <- hist(0:n_draws, breaks = breaks, plot = FALSE, include.lowest = TRUE)$counts
  expected <- length(ranks) * support / (n_draws + 1L)
  ok_expected <- expected > 0
  chi <- if (any(ok_expected)) sum((h$counts[ok_expected] - expected[ok_expected])^2 / expected[ok_expected]) else NA_real_
  df <- sum(ok_expected) - 1L
  p <- if (is.finite(chi) && df > 0L) stats::pchisq(chi, df = df, lower.tail = FALSE) else NA_real_
  u <- (ranks + .5) / (n_draws + 1)
  u <- sort(u); emp <- seq_along(u) / length(u)
  ecdf_dev <- if (length(u)) max(abs(emp - u)) else NA_real_
  structure(list(ranks = ranks, n_draws = n_draws, bins = bins, counts = h$counts,
                 breaks = h$breaks, expected_count = expected, chi_square = chi,
                 chi_square_p = p, ecdf_max_deviation = ecdf_dev,
                 caveat = "SBC diagnoses calibration of the supplied simulator-model-inference workflow; it does not establish substantive model adequacy for empirical data."),
            class = "eye_sbc_diagnostics")
}

#' ECDF deviation summary for SBC ranks
#' @param x SBC diagnostics or ranks.
#' @param n_draws Required if x is ranks.
#' @return An R object containing eCDF deviation summary for SBC ranks. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
sbc_ecdf_deviation <- function(x, n_draws = NULL) {
  if (!inherits(x, "eye_sbc_diagnostics")) x <- sbc_rank_diagnostics(x, n_draws)
  x$ecdf_max_deviation
}

#' Interval coverage calibration curve
#' @param truth True values.
#' @param lower Matrix/data.frame of lower limits or numeric vector.
#' @param upper Matrix/data.frame of upper limits or numeric vector.
#' @param nominal Nominal coverage labels, one per interval column.
#' @return A data frame containing interval coverage calibration curve. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
coverage_calibration_curve <- function(truth, lower, upper, nominal = NULL) {
  truth <- .ep09_num(truth); L <- as.matrix(lower); U <- as.matrix(upper)
  storage.mode(L) <- "double"; storage.mode(U) <- "double"
  if (!all(dim(L) == dim(U))) stop("lower and upper must have the same dimensions.", call. = FALSE)
  if (nrow(L) != length(truth)) stop("truth length must match interval rows.", call. = FALSE)
  if (is.null(nominal)) nominal <- if (ncol(L) == 1L) .95 else seq(.5, .95, length.out = ncol(L))
  nominal <- .ep09_num(nominal)
  if (length(nominal) != ncol(L) || any(!is.finite(nominal) | nominal < 0 | nominal > 1)) stop("nominal must contain one finite probability in [0,1] per interval column.", call. = FALSE)
  empirical <- vapply(seq_len(ncol(L)), function(j) {
    z <- L[,j] <= truth & U[,j] >= truth
    if (any(!is.na(z))) mean(z[!is.na(z)]) else NA_real_
  }, numeric(1))
  data.frame(nominal = nominal, empirical = empirical, error = empirical - nominal, stringsAsFactors = FALSE)
}

#' Build a non-collapsed measurement-error budget
#'
#' @param accuracy Measurement inaccuracy/offset metric.
#' @param precision Measurement imprecision metric.
#' @param data_loss Data-loss proportion.
#' @param effective_hz Effective sampling frequency.
#' @param calibration_drift Optional drift metric.
#' @param units Optional named units.
#' @return A data frame containing a non-collapsed measurement-error budget. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
measurement_error_budget <- function(accuracy = NA_real_, precision = NA_real_, data_loss = NA_real_,
                                     effective_hz = NA_real_, calibration_drift = NA_real_, units = NULL) {
  vals <- vapply(list(accuracy, precision, data_loss, effective_hz, calibration_drift), function(z) .ep09_num(z)[1L], numeric(1))
  out <- data.frame(component = c("accuracy_error", "precision_error", "data_loss", "effective_sampling_hz", "calibration_drift"),
                    value = vals,
                    direction = c("lower_better", "lower_better", "lower_better", "context_dependent", "lower_better"),
                    stringsAsFactors = FALSE)
  if (!is.null(units)) out$unit <- rep(units, length.out = nrow(out))
  attr(out, "interpretation") <- "Components remain separate because they describe different measurement limitations and should not be collapsed without a study-specific justification."
  class(out) <- c("eye_measurement_error_budget", "data.frame")
  out
}

#' Audit compatibility between measurement resolution and an analysis target
#'
#' @param event_duration_ms Smallest event duration the analysis intends to resolve.
#' @param effective_hz Empirical sampling frequency.
#' @param spatial_feature_size Optional smallest spatial feature/AOI dimension in coordinate units.
#' @param radial_error Optional empirical radial error in the same spatial units.
#' @param min_samples User-declared minimum samples per temporal feature.
#' @param max_error_fraction User-declared maximum spatial-error / feature-size ratio.
#' @return A named list with components "expected_samples", "temporal_ok", "spatial_error_fraction", "spatial_ok", "min_samples", "max_error_fraction", "overall", "caveat", containing compatibility between measurement resolution and an analysis target and associated metadata or diagnostics.
#' @export
analysis_resolution_guard <- function(event_duration_ms, effective_hz,
                                      spatial_feature_size = NA_real_, radial_error = NA_real_,
                                      min_samples = 3, max_error_fraction = .5) {
  event_duration_ms <- .ep09_num(event_duration_ms)[1L]; effective_hz <- .ep09_num(effective_hz)[1L]
  min_samples <- .ep09_num(min_samples)[1L]; max_error_fraction <- .ep09_num(max_error_fraction)[1L]
  if (!is.finite(event_duration_ms) || event_duration_ms <= 0) stop("event_duration_ms must be positive.", call. = FALSE)
  if (!is.finite(effective_hz) || effective_hz <= 0) stop("effective_hz must be positive.", call. = FALSE)
  if (!is.finite(min_samples) || min_samples <= 0) stop("min_samples must be positive.", call. = FALSE)
  if (!is.finite(max_error_fraction) || max_error_fraction < 0) stop("max_error_fraction must be non-negative.", call. = FALSE)
  sf <- .ep09_num(spatial_feature_size)[1L]; re <- .ep09_num(radial_error)[1L]
  if (is.finite(sf) && sf <= 0) stop("spatial_feature_size must be positive when supplied.", call. = FALSE)
  if (is.finite(re) && re < 0) stop("radial_error must be non-negative when supplied.", call. = FALSE)
  expected_samples <- event_duration_ms / 1000 * effective_hz
  temporal_ok <- expected_samples >= min_samples
  spatial_ratio <- re / sf
  spatial_ok <- if (is.finite(spatial_ratio)) spatial_ratio <= max_error_fraction else NA
  out <- list(expected_samples = expected_samples, temporal_ok = temporal_ok,
              spatial_error_fraction = spatial_ratio, spatial_ok = spatial_ok,
              min_samples = min_samples, max_error_fraction = max_error_fraction,
              overall = isTRUE(temporal_ok) && (is.na(spatial_ok) || isTRUE(spatial_ok)),
              caveat = "Thresholds are researcher-declared compatibility rules. eyeprocess does not impose universal temporal or spatial quality cutoffs.")
  class(out) <- "eye_analysis_resolution_guard"; out
}

#' Audit declared order of pupil preprocessing steps
#'
#' @param steps Character vector in execution order.
#' @param cleaning_patterns Patterns considered cleaning/preprocessing.
#' @param baseline_pattern Pattern identifying baseline correction.
#' @return A named list with components "steps", "baseline_positions", "cleaning_positions", "cleaning_after_baseline", "status", "caveat", containing declared order of pupil preprocessing steps and associated metadata or diagnostics.
#' @export
audit_pupil_preprocessing_order <- function(steps,
                                            cleaning_patterns = c("blink", "missing", "interpol", "artifact", "smooth", "filter"),
                                            baseline_pattern = "baseline") {
  steps <- as.character(steps)
  if (!length(steps) || any(is.na(steps) | !nzchar(steps))) stop("steps must contain non-empty step labels.", call. = FALSE)
  cleaning_patterns <- as.character(cleaning_patterns); baseline_pattern <- as.character(baseline_pattern)[1L]
  if (!length(cleaning_patterns) || any(is.na(cleaning_patterns) | !nzchar(cleaning_patterns))) stop("cleaning_patterns must be non-empty strings.", call. = FALSE)
  if (is.na(baseline_pattern) || !nzchar(baseline_pattern)) stop("baseline_pattern must be a non-empty string.", call. = FALSE)
  low <- tolower(steps)
  base_idx <- grep(baseline_pattern, low)
  clean_idx <- unique(unlist(lapply(cleaning_patterns, function(p) grep(p, low))))
  late_cleaning <- if (length(base_idx) && length(clean_idx)) clean_idx[clean_idx > min(base_idx)] else integer()
  out <- list(steps = steps, baseline_positions = base_idx, cleaning_positions = clean_idx,
              cleaning_after_baseline = steps[late_cleaning],
              status = if (!length(base_idx)) "baseline_not_declared" else if (length(late_cleaning)) "review" else "pass",
              caveat = "This is an order audit. Appropriate preprocessing still depends on the signal, task, device, and analysis plan.")
  class(out) <- "eye_pupil_preprocessing_order_audit"; out
}

#' Evaluate pupil baseline-window sensitivity
#'
#' @param data Pupil data.
#' @param time Time column.
#' @param pupil Pupil column.
#' @param windows Named list of two-element baseline windows.
#' @param by Optional grouping columns.
#' @param correction `subtractive` or `divisive`.
#' @return An R object containing pupil baseline-window sensitivity. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
pupil_baseline_sensitivity <- function(data, time = "time_ms", pupil = "pupil", windows,
                                       by = NULL, correction = c("subtractive", "divisive")) {
  correction <- match.arg(correction); d <- .ep09_as_df(data); .ep09_req_cols(d, c(time, pupil, by))
  if (!is.list(windows) || !length(windows)) stop("windows must be a non-empty named list.", call. = FALSE)
  if (is.null(names(windows)) || any(!nzchar(names(windows)))) names(windows) <- paste0("W", seq_along(windows))
  groups <- .ep09_group_split(d, by); rows <- list(); k <- 0L
  for (wn in names(windows)) {
    w <- .ep09_num(windows[[wn]]); if (length(w) != 2L || any(!is.finite(w))) stop("Each baseline window must have two finite endpoints.", call. = FALSE)
    for (idx in groups) {
      z <- d[idx,,drop=FALSE]; tt <- .ep09_num(z[[time]]); pp <- .ep09_num(z[[pupil]])
      b <- pp[tt >= min(w) & tt <= max(w)]; b <- b[is.finite(b)]
      baseline <- if (length(b)) mean(b) else NA_real_
      post <- pp[tt > max(w)]; post <- post[is.finite(post)]
      corrected <- if (!length(post) || !is.finite(baseline)) NA_real_ else if (correction == "subtractive") mean(post - baseline) else if (baseline != 0) mean(post / baseline) else NA_real_
      k <- k + 1L; rows[[k]] <- cbind(.ep09_group_header(z, by), data.frame(window = wn, start = min(w), end = max(w), baseline = baseline,
                                                                                   corrected_post_mean = corrected, correction = correction, stringsAsFactors = FALSE))
    }
  }
  out <- .ep09_rbind_fill(rows); class(out) <- c("eye_pupil_baseline_sensitivity", "data.frame"); out
}

#' @export
print.eye_sbc_diagnostics <- function(x, ...) {
  cat("<eye_sbc_diagnostics>\n", " ranks: ", length(x$ranks), "\n draws/rank: ", x$n_draws,
      "\n ECDF max deviation: ", format(x$ecdf_max_deviation, digits = 4), "\n", sep = "")
  invisible(x)
}

#' @export
print.eye_analysis_resolution_guard <- function(x, ...) {
  cat("<eye_analysis_resolution_guard>\n", " expected samples: ", format(x$expected_samples, digits=4),
      "\n temporal OK: ", x$temporal_ok, "\n spatial OK: ", x$spatial_ok, "\n overall: ", x$overall, "\n", sep="")
  invisible(x)
}

#' @export
plot.eye_sbc_diagnostics <- function(x, y = NULL, type = c("rank", "ecdf"), ...) {
  type <- match.arg(type)
  if (type == "rank") {
    h <- hist(x$ranks, breaks=x$breaks, plot=FALSE, include.lowest=TRUE)
    graphics::plot(h, main="Simulation-based calibration ranks", xlab="Rank", ...)
    graphics::lines(h$mids, x$expected_count, type = "b", lty = 2)
  } else {
    u <- sort((x$ranks + .5)/(x$n_draws+1)); emp <- seq_along(u)/length(u)
    graphics::plot(u, emp, type="s", xlab="Uniform reference quantile", ylab="Empirical CDF", main="SBC rank ECDF", ...)
    graphics::abline(0,1,lty=2)
  }
  invisible(x)
}

#' @export
plot.eye_measurement_error_budget <- function(x, y = NULL, ...) {
  d <- as.data.frame(x); finite <- is.finite(.ep09_num(d$value))
  if (!any(finite)) return(.ep09_plot_empty())
  graphics::barplot(.ep09_num(d$value[finite]), names.arg=d$component[finite], las=2, main="Measurement error / resolution budget", ...)
  invisible(d)
}

#' @export
plot.eye_pupil_baseline_sensitivity <- function(x, y = NULL, ...) {
  d <- as.data.frame(x)
  ok <- is.finite(.ep09_num(d$corrected_post_mean)) & !is.na(d$window)
  if (!any(ok)) return(.ep09_plot_empty())
  graphics::boxplot(d$corrected_post_mean[ok] ~ d$window[ok], xlab="Baseline window", ylab="Corrected post-window mean",
                    main="Pupil baseline sensitivity", ...); invisible(d)
}
