# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Base-graphics visual diagnostics for 0.9 validation/governance objects.

.ep09_plot_empty <- function(message = "No plottable data", main = NULL) {
  graphics::plot.new(); if (!is.null(main)) graphics::title(main = main)
  graphics::text(.5, .5, message); invisible(NULL)
}

#' @export
plot.eye_process_validation_design <- function(x, y = NULL, ...) {
  g <- expand_process_validation_design(x)
  counts <- c(n_persons = length(unique(g$n_persons)), n_trials = length(unique(g$n_trials)),
              missingness = length(unique(g$missingness)), sampling_rate_hz = length(unique(g$sampling_rate_hz)),
              aoi_error = length(unique(g$aoi_error)), calibration_error = length(unique(g$calibration_error)),
              pupil_dropout = length(unique(g$pupil_dropout)), heterogeneity = length(unique(g$heterogeneity)),
              misspecification = length(unique(g$model_misspecification)))
  graphics::barplot(counts, las = 2, ylab = "Levels", main = paste0("Validation design: ", nrow(g), " conditions"), ...)
  invisible(counts)
}

#' @export
plot.eye_process_validation_result <- function(x, y = NULL,
                                               type = c("recovery", "bias", "coverage", "failure"),
                                               parameter = NULL, ...) {
  type <- match.arg(type)
  if (type == "failure") {
    f <- validation_failure_profile(x)
    if (!nrow(f)) return(.ep09_plot_empty("No recorded failures", "Validation failure profile"))
    graphics::barplot(f$failure_rate, names.arg = f$stage, ylab = "Failure rate", main = "Validation failures", ...)
    return(invisible(f))
  }
  d <- x$estimates
  if (!nrow(d)) return(.ep09_plot_empty())
  if (!is.null(parameter)) d <- d[d$parameter %in% parameter, , drop = FALSE]
  if (!nrow(d)) return(.ep09_plot_empty("Requested parameter not found"))
  if (type == "recovery") {
    graphics::plot(.ep09_num(d$truth), .ep09_num(d$estimate), xlab = "Truth", ylab = "Estimate", main = "Parameter recovery", ...)
    rr <- range(c(.ep09_num(d$truth), .ep09_num(d$estimate)), finite = TRUE); if (all(is.finite(rr))) graphics::abline(0, 1, lty = 2)
  } else if (type == "bias") {
    b <- .ep09_num(d$estimate) - .ep09_num(d$truth)
    graphics::boxplot(b ~ d$parameter, xlab = "Parameter", ylab = "Estimate - truth", main = "Validation bias", ...); graphics::abline(h = 0, lty = 2)
  } else {
    s <- validation_coverage_table(x)
    finite_cov <- is.finite(.ep09_num(s$coverage))
    if (!nrow(s) || !any(finite_cov)) return(.ep09_plot_empty("No finite interval coverage available"))
    graphics::plot(seq_len(nrow(s)), s$coverage, ylim = range(c(s$coverage[finite_cov], s$nominal[finite_cov]), na.rm = TRUE), xlab = "Summary row", ylab = "Coverage", main = "Interval coverage", ...)
    graphics::abline(h = unique(s$nominal)[1L], lty = 2)
  }
  invisible(d)
}

#' @export
plot.eye_validation_reference_comparison <- function(x, y = NULL, metric = "rmse_delta", ...) {
  d <- x$table
  if (!metric %in% names(d)) return(.ep09_plot_empty(paste("Missing", metric)))
  v <- .ep09_num(d[[metric]])
  graphics::plot(seq_along(v), v, type = "h", xlab = "Matched summary row", ylab = metric,
                 main = paste("Frozen-reference delta:", metric), ...); graphics::abline(h = 0, lty = 2)
  invisible(d)
}

#' @export
plot.eye_analysis_pipeline <- function(x, y = NULL, ...) {
  g <- eye_pipeline_graph(x)
  if (!nrow(g$vertices)) return(.ep09_plot_empty())
  v <- g$vertices
  if (!"level" %in% names(v)) v$level <- seq_len(nrow(v))
  levels <- unique(v$level); ypos <- numeric(nrow(v))
  for (lv in levels) { ii <- which(v$level == lv); ypos[ii] <- seq(0, 1, length.out = length(ii) + 2L)[-c(1L, length(ii) + 2L)] }
  xpos <- match(v$level, sort(unique(v$level)))
  graphics::plot(xpos, ypos, type = "n", axes = FALSE, xlab = "Execution level", ylab = "", main = "Governed eyeprocess pipeline", ...)
  if (nrow(g$edges)) for (i in seq_len(nrow(g$edges))) {
    a <- match(g$edges$from[i], v$step); b <- match(g$edges$to[i], v$step)
    graphics::arrows(xpos[a], ypos[a], xpos[b], ypos[b], length = .08)
  }
  graphics::points(xpos, ypos, pch = 21, bg = "white", cex = 2)
  graphics::text(xpos, ypos, labels = v$step, pos = 3, cex = .8)
  graphics::axis(1, at = seq_along(sort(unique(v$level))), labels = sort(unique(v$level)))
  invisible(g)
}

#' @export
plot.eye_pipeline_audit <- function(x, y = NULL, ...) {
  d <- x$table
  val <- if ("status" %in% names(d)) match(d$status, c("not_run", "success", "optional_error", "error")) else as.integer(d$decision_declared)
  val[is.na(val)] <- 0
  graphics::barplot(val, names.arg = d$step, las = 2, ylab = "Audit/status code", main = "Pipeline audit", ...)
  invisible(d)
}

#' @export
plot.eye_api_audit <- function(x, y = NULL, ...) {
  d <- x$table
  if (!is.data.frame(d) || !nrow(d) || !"status" %in% names(d)) return(.ep09_plot_empty())
  s <- sort(table(d$status), decreasing = TRUE)
  graphics::barplot(s, las = 2, ylab = "Exports", main = "API lifecycle audit", ...)
  invisible(s)
}

#' @export
plot.eye_process_sensitivity <- function(x, y = NULL, type = c("specification_curve", "decision_leverage"),
                                         effect = "effect", lower = NULL, upper = NULL, ...) {
  type <- match.arg(type)
  if (type == "decision_leverage") {
    d <- sensitivity_decision_leverage(x, effect = effect)
    if (!nrow(d)) return(.ep09_plot_empty())
    graphics::barplot(d$effect_range, names.arg = d$decision, las = 2, ylab = "Within-decision effect range", main = "Decision leverage", ...)
    return(invisible(d))
  }
  d <- specification_curve_data(x, effect = effect, lower = lower, upper = upper)
  if (!nrow(d)) return(.ep09_plot_empty())
  graphics::plot(d$curve_order, d$.effect, pch = 16, xlab = "Specification (ordered)", ylab = effect, main = "Specification curve", ...)
  graphics::abline(h = 0, lty = 2)
  if (all(c(".lower", ".upper") %in% names(d))) graphics::segments(d$curve_order, d$.lower, d$curve_order, d$.upper)
  invisible(d)
}

#' @export
plot.eye_decision_stability <- function(x, y = NULL, ...) {
  s <- x$summary
  vals <- unlist(s[intersect(c("sign_stability", "threshold_stability", "significance_stability"), names(s))], use.names = TRUE)
  if (!length(vals)) return(.ep09_plot_empty())
  graphics::barplot(vals, ylim = c(0, 1), ylab = "Stability", main = "Decision stability", ...); graphics::abline(h = .9, lty = 2)
  invisible(vals)
}

#' @export
plot.eye_decision_manifest <- function(x, y = NULL, ...) {
  d <- decision_manifest_table(x)
  section <- sub("\\..*$", "", d$path)
  tab <- sort(table(section), decreasing = TRUE)
  if (!length(tab)) return(.ep09_plot_empty("No declared decisions", "Analysis decision manifest"))
  graphics::barplot(tab, las = 2, ylab = "Declared decisions", main = "Analysis decision manifest", ...)
  invisible(tab)
}

#' @export
plot.eye_process_reliability_profile <- function(x, y = NULL, type = c("bland_altman", "summary"), ...) {
  type <- match.arg(type)
  if (type == "bland_altman") {
    if (is.null(x$bland_altman)) return(.ep09_plot_empty("At least two sessions are required", "Bland-Altman process reliability"))
    d <- x$bland_altman$pairs
    sm <- x$bland_altman$summary
    graphics::plot(d$pair_mean, d$difference, xlab = "Pair mean", ylab = "Difference", main = "Bland-Altman process reliability", ...)
    graphics::abline(h = c(sm$bias[[1L]], sm$loa_lower[[1L]], sm$loa_upper[[1L]]), lty = c(1,2,2))
    return(invisible(d))
  }
  vals <- c(ICC_A1 = x$icc$icc_a1[[1L]], BA_bias = if (is.null(x$bland_altman)) NA_real_ else x$bland_altman$summary$bias[[1L]])
  graphics::barplot(vals, main = "Process reliability summary", ...); invisible(vals)
}

#' @export
plot.eye_benchmark_result <- function(x, y = NULL, metric = "elapsed_sec", ...) {
  d <- x$results; if (!nrow(d) || !metric %in% names(d)) return(.ep09_plot_empty())
  size <- if ("n_obs" %in% names(d)) .ep09_num(d$n_obs) else if ("n_rows" %in% names(d)) .ep09_num(d$n_rows) else seq_len(nrow(d))
  graphics::plot(size, .ep09_num(d[[metric]]), log = if (length(size) && all(size > 0, na.rm = TRUE)) "x" else "", xlab = "Observations", ylab = metric, main = "eyeprocess scaling benchmark", ...)
  invisible(d)
}

#' @export
plot.eye_process_stress_test <- function(x, y = NULL, severity = "missingness", metric = "effect", ...) {
  d <- x$results; .ep09_req_cols(d, c(severity, metric), "x$results")
  graphics::plot(.ep09_num(d[[severity]]), .ep09_num(d[[metric]]), xlab = severity, ylab = metric, main = "Synthetic stress response", ...)
  invisible(d)
}

#' @export
plot.eye_calibration_error_model <- function(x, y = NULL, ...) {
  E <- x$errors
  graphics::plot(E[,1L], E[,2L], xlab = "Horizontal error", ylab = "Vertical error", main = "Empirical calibration-error cloud", ...)
  graphics::abline(h = 0, v = 0, lty = 2); graphics::points(x$mean_error[1L], x$mean_error[2L], pch = 4, cex = 1.5)
  invisible(E)
}

#' @export
plot.eye_calibration_drift_profile <- function(x, y = NULL, ...) {
  d <- x$table
  metric <- intersect(c("delta_from_first", "mean_radial_error", "bias_x"), names(d))[1L]
  if (is.na(metric)) return(.ep09_plot_empty())
  graphics::plot(seq_len(nrow(d)), .ep09_num(d[[metric]]), type = "b", xlab = "Session/group order", ylab = metric, main = "Calibration drift profile", ...)
  invisible(d)
}

#' @export
plot.eye_data_quality_profile <- function(x, y = NULL, metric = NULL, ...) {
  d <- x$table
  if (is.null(metric)) metric <- intersect(c("valid_fraction", "effective_hz", "rms_s2s", "missing_fraction"), names(d))[1L]
  if (is.na(metric) || !metric %in% names(d)) return(.ep09_plot_empty("No requested quality metric"))
  graphics::barplot(.ep09_num(d[[metric]]), names.arg = seq_len(nrow(d)), ylab = metric, main = "Eye-tracking data quality", ...)
  invisible(d)
}

#' @export
plot.eye_probabilistic_aoi_assignment <- function(x, y = NULL, ...) {
  d <- x$probabilities
  if (!nrow(d)) return(.ep09_plot_empty())
  aois <- unique(d$aoi); samples <- sort(unique(d$sample_id))
  M <- matrix(0, nrow = length(aois), ncol = length(samples), dimnames = list(aois, samples))
  for (i in seq_len(nrow(d))) M[match(d$aoi[i], aois), match(d$sample_id[i], samples)] <- d$probability[i]
  graphics::image(seq_along(samples), seq_along(aois), t(M), xlab = "Sample", ylab = "AOI", axes = FALSE, main = "Probabilistic AOI membership", ...)
  graphics::axis(1); graphics::axis(2, at = seq_along(aois), labels = aois, las = 2)
  invisible(M)
}

#' @export
plot.eye_sampling_irregularity_audit <- function(x, y = NULL, ...) {
  d <- x$table
  graphics::barplot(d$interval_cv, names.arg = seq_len(nrow(d)), ylab = "Interval CV", main = "Sampling irregularity", ...)
  graphics::abline(h = x$cv_threshold, lty = 2); invisible(d)
}

#' @export
plot.eye_temporal_leakage_audit <- function(x, y = NULL, ...) {
  d <- x$detail
  graphics::barplot(d$temporal_delta, names.arg = d$feature, las = 2, ylab = "available_at - outcome_at", main = "Temporal feature provenance", ...)
  graphics::abline(h = 0, lty = 2); invisible(d)
}

#' @export
plot.eye_process_negative_controls <- function(x, y = NULL, effect = "effect", ...) {
  d <- x$results; .ep09_req_cols(d, c("control", effect))
  graphics::boxplot(.ep09_num(d[[effect]]) ~ d$control, ylab = effect, main = "Process negative controls", ...)
  graphics::abline(h = 0, lty = 2); invisible(d)
}

#' @export
plot.eye_reproducibility_comparison <- function(x, y = NULL, ...) {
  d <- x$detail; vals <- as.integer(d$identical)
  graphics::barplot(vals, names.arg = d$field, las = 2, ylim = c(0,1), ylab = "Identical", main = "Reproducibility fingerprint comparison", ...)
  invisible(d)
}

#' @export
plot.eye_prov_graph <- function(x, y = NULL, ...) {
  validate_eye_prov_graph(x); n <- nrow(x$nodes)
  if (!n) return(.ep09_plot_empty())
  theta <- seq(0, 2*pi, length.out = n + 1L)[-1L]; xx <- cos(theta); yy <- sin(theta)
  graphics::plot(xx, yy, type = "n", axes = FALSE, xlab = "", ylab = "", main = "eyeprocess provenance graph", asp = 1, ...)
  if (nrow(x$edges)) for (i in seq_len(nrow(x$edges))) {
    a <- match(x$edges$from[i], x$nodes$id); b <- match(x$edges$to[i], x$nodes$id)
    graphics::arrows(xx[a], yy[a], xx[b], yy[b], length = .08)
  }
  graphics::points(xx, yy, pch = 21, bg = "white", cex = 2); graphics::text(xx, yy, labels = x$nodes$label, pos = 3, cex = .8)
  invisible(x)
}

#' @export
plot.eye_software_paper_evidence <- function(x, y = NULL, ...) {
  cvr <- software_paper_coverage(x)
  vals <- c(covered = cvr$n_covered, pending = cvr$n_pending, unsupported = cvr$n_unsupported)
  graphics::barplot(vals, ylab = "Claims", main = "Software-paper evidence coverage", ...); invisible(vals)
}
