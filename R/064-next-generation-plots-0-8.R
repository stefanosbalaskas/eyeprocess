# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Plot methods and standalone process/signal visualization helpers.

.ep08_plot_empty <- function(main, text = "No plottable data") {
  graphics::plot.new(); graphics::title(main = main); graphics::text(.5, .5, text)
  invisible(NULL)
}

#' Plot biometric preflight diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot biometric preflight diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_biometric_preflight <- function(x, type = c("heatmap", "decision_counts"), ...) {
  type <- match.arg(type)
  tab <- x$table
  if (!nrow(tab)) return(.ep08_plot_empty("Biometric pre-flight"))
  if (type == "decision_counts") {
    counts <- table(tab$preflight_decision)
    graphics::barplot(counts, las = 2, ylab = "Groups", main = "Biometric pre-flight decisions", ...)
    return(invisible(counts))
  }
  flags <- x$flag_columns
  if (!length(flags)) return(.ep08_plot_empty("Biometric pre-flight", "No flag columns"))
  ord <- order(tab$preflight_flag_count, decreasing = TRUE)
  M <- as.matrix(tab[ord, flags, drop = FALSE]) * 1
  graphics::image(seq_len(ncol(M)), seq_len(nrow(M)), t(M[nrow(M):1, , drop = FALSE]),
                  axes = FALSE, xlab = "Criterion", ylab = "Group ordered by severity",
                  main = "Pre-flight criterion flag map", ...)
  graphics::axis(1, at = seq_len(ncol(M)), labels = sub("_flag$", "", flags), las = 2, cex.axis = .7)
  invisible(M)
}

#' Plot process anomaly audit diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process anomaly audit diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_anomaly_audit <- function(x, ...) {
  tab <- x$table
  if (!nrow(tab)) return(.ep08_plot_empty("Process anomaly audit"))
  y <- tab$mahalanobis_process_distance
  graphics::plot(seq_along(y), y, type = "h", xlab = "Groups ordered by distance",
                 ylab = "Mahalanobis process distance",
                 main = "Multivariate process/data-quality review distance", ...)
  graphics::abline(h = x$threshold, lty = 2)
  invisible(tab)
}

#' Plot presentation accessibility diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot presentation accessibility diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_presentation_accessibility <- function(x, ...) {
  tab <- x$table
  if (!nrow(tab)) return(.ep08_plot_empty("Presentation sensitivity"))
  graphics::hist(tab$presentation_sensitivity_index,
                 xlab = "Presentation sensitivity index",
                 main = "Presentation/accessibility sensitivity audit", ...)
  graphics::abline(v = x$threshold, lty = 2)
  invisible(tab)
}

#' Plot process drift audit diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process drift audit diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param metric Metric to evaluate or display.
#' @param item Optional item identifier used to restrict or highlight results.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_drift_audit <- function(x,
                                         type = c("trajectory", "delta", "heatmap", "control"),
                                         metric = NULL, item = NULL, ...) {
  type <- match.arg(type)
  tr <- x$trajectories; tab <- x$table
  if (is.null(metric)) metric <- x$metrics[1L]
  if (!metric %in% x$metrics) stop("Unknown metric: ", metric, call. = FALSE)
  dc <- paste0(metric, "_delta")
  fc <- paste0(metric, "_drift_flag")
  if (metric == "irt_difficulty") fc <- "difficulty_drift_flag"
  if (metric == "irt_discrimination") fc <- "discrimination_drift_flag"
  if (metric == "valid_gaze_prop") fc <- "gaze_quality_drift_flag"
  if (metric == "screen_luminance") fc <- "screen_luminance_drift_flag"

  if (type == "trajectory") {
    ids <- unique(tr[[x$item]])
    if (is.null(item)) item <- ids[1L]
    z <- tr[tr[[x$item]] %in% item, , drop = FALSE]
    z <- z[order(z$batch_order), , drop = FALSE]
    if (!nrow(z)) return(.ep08_plot_empty("Process drift", "No trajectory for requested item"))
    graphics::plot(z$batch_order, z[[metric]], type = "b", xlab = "Deployment batch order",
                   ylab = metric, main = paste("Deployment trajectory:", item), ...)
    return(invisible(z))
  }
  if (type == "delta") {
    if (!dc %in% names(tab)) return(.ep08_plot_empty("Drift delta", "Delta unavailable"))
    ord <- order(tab[[dc]])
    graphics::dotchart(tab[[dc]][ord], labels = tab[[x$item]][ord],
                       xlab = paste(metric, "delta"), main = "Latest minus baseline drift", ...)
    graphics::abline(v = 0, lty = 3)
    return(invisible(tab))
  }
  if (type == "control") {
    z <- tr
    agg <- stats::aggregate(z[[metric]], by = list(batch_order = z$batch_order), FUN = .ep08_mean)
    names(agg)[2L] <- "mean_metric"
    mu <- mean(agg$mean_metric, na.rm = TRUE); s <- stats::sd(agg$mean_metric, na.rm = TRUE)
    graphics::plot(agg$batch_order, agg$mean_metric, type = "b", xlab = "Deployment batch order",
                   ylab = paste("Mean", metric), main = "Process-drift control view", ...)
    graphics::abline(h = mu, lty = 2)
    if (is.finite(s)) graphics::abline(h = c(mu - 2 * s, mu + 2 * s), lty = 3)
    return(invisible(agg))
  }
  delta_cols <- grep("_delta$", names(tab), value = TRUE)
  if (!length(delta_cols)) return(.ep08_plot_empty("Drift heatmap", "No deltas"))
  M <- as.matrix(tab[delta_cols]); rownames(M) <- tab[[x$item]]
  # scale each metric to comparable standardized delta for visualization only
  Z <- apply(M, 2L, function(v) {
    s <- stats::sd(v, na.rm = TRUE); if (!is.finite(s) || s == 0) rep(0, length(v)) else v / s
  })
  if (is.null(dim(Z))) Z <- matrix(Z, ncol = 1L)
  graphics::image(seq_len(ncol(Z)), seq_len(nrow(Z)), t(Z[nrow(Z):1, , drop = FALSE]),
                  axes = FALSE, xlab = "Metric delta", ylab = "Item", main = "Standardized drift delta map", ...)
  graphics::axis(1, at = seq_len(ncol(Z)), labels = sub("_delta$", "", delta_cols), las = 2, cex.axis = .7)
  invisible(Z)
}

#' Plot process window sensitivity diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process window sensitivity diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_window_sensitivity <- function(x, ...) {
  tab <- x$table
  if (!nrow(tab)) return(.ep08_plot_empty("Process-window sensitivity"))
  widths <- sort(unique(tab$width_ms)); steps <- sort(unique(tab$step_ms))
  if (length(steps) == 1L) {
    graphics::plot(tab$width_ms, tab$mean_value, type = "b", xlab = "Window width (ms)",
                   ylab = paste("Mean", x$metric), main = "Process-window sensitivity", ...)
  } else {
    mat <- matrix(NA_real_, nrow = length(widths), ncol = length(steps),
                  dimnames = list(widths, steps))
    for (i in seq_len(nrow(tab))) mat[as.character(tab$width_ms[i]), as.character(tab$step_ms[i])] <- tab$mean_value[i]
    graphics::matplot(widths, mat, type = "b", lty = seq_len(ncol(mat)), pch = seq_len(ncol(mat)),
                      xlab = "Window width (ms)", ylab = paste("Mean", x$metric),
                      main = "Process-window sensitivity", ...)
    graphics::legend("topright", legend = paste("step", steps, "ms"), lty = seq_along(steps), pch = seq_along(steps), bty = "n")
  }
  invisible(tab)
}

#' Plot pupil frequency features diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot pupil frequency features diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_pupil_frequency_features <- function(x,
                                              type = c("features", "power_relationship"), ...) {
  type <- match.arg(type); tab <- x$features
  if (!nrow(tab)) return(.ep08_plot_empty("Pupil frequency features"))
  if (type == "power_relationship") {
    graphics::plot(tab$pupil_low_frequency_power, tab$pupil_high_frequency_power,
                   xlab = "Low-band power", ylab = "High-band power",
                   main = "Pupil frequency-band relationship", ...)
  } else {
    vars <- c("pupil_frequency_contrast", "pupil_velocity_activity", "pupil_ripa_proxy")
    vars <- vars[vars %in% names(tab)]
    vals <- vapply(vars, function(v) .ep08_mean(tab[[v]]), numeric(1))
    graphics::barplot(vals, las = 2, ylab = "Mean feature value",
                      main = "Pupil activity/frequency feature summary", ...)
  }
  invisible(tab)
}

#' Plot pupil frequency stability diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot pupil frequency stability diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param feature Process feature to evaluate or display.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_pupil_frequency_stability <- function(x, feature = "pupil_frequency_contrast", ...) {
  tab <- x$table
  if (!nrow(tab) || !feature %in% names(tab)) return(.ep08_plot_empty("Pupil frequency stability"))
  agg <- stats::aggregate(tab[[feature]], by = list(window_ms = tab$window_ms), FUN = .ep08_mean)
  names(agg)[2L] <- "value"
  graphics::plot(agg$window_ms, agg$value, type = "b", xlab = "Window length (ms)",
                 ylab = feature, main = "Pupil frequency-feature stability", ...)
  invisible(agg)
}

#' Plot pupil deconvolution diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot pupil deconvolution diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_pupil_deconvolution <- function(x,
                                         type = c("observed_fitted", "effects", "residuals", "kernels"), ...) {
  type <- match.arg(type)
  if (type == "kernels") {
    tt <- seq(-500, 3000, length.out = 300)
    yy <- pupil_response_kernel(tt, tmax_ms = x$tmax_ms, shape = x$shape)
    graphics::plot(tt, yy, type = "l", xlab = "Time since event (ms)", ylab = "Kernel",
                   main = "Pupil response kernel", ...)
    return(invisible(data.frame(time = tt, kernel = yy)))
  }
  if (type == "effects") {
    e <- x$effects
    beta_cols <- grep("^beta__", names(e), value = TRUE)
    if (!nrow(e) || !length(beta_cols)) return(.ep08_plot_empty("Pupil event effects"))
    vals <- vapply(beta_cols, function(v) .ep08_mean(e[[v]]), numeric(1))
    graphics::barplot(vals, names.arg = sub("^beta__", "", beta_cols), las = 2,
                      ylab = "Mean event coefficient", main = "Pupil deconvolution event effects", ...)
    return(invisible(e))
  }
  d <- x$fitted
  if (!nrow(d)) return(.ep08_plot_empty("Pupil deconvolution"))
  if (type == "observed_fitted" && length(x$by) && all(x$by %in% names(d))) {
    first <- d[1L, x$by, drop = FALSE]
    keep <- rep(TRUE, nrow(d))
    for (nm in x$by) keep <- keep & as.character(d[[nm]]) == as.character(first[[nm]][1L])
    d <- d[keep, , drop = FALSE]
  }
  if (type == "residuals") {
    graphics::plot(d$time, d$residual, pch = 16, cex = .5, xlab = "Time (ms)", ylab = "Residual",
                   main = "Pupil deconvolution residuals", ...)
    graphics::abline(h = 0, lty = 2)
  } else {
    ord <- order(d$time)
    graphics::plot(d$time[ord], d$observed[ord], type = "l", xlab = "Time (ms)", ylab = "Pupil",
                   main = "Observed and fitted pupil response", ...)
    graphics::lines(d$time[ord], d$fitted[ord], lty = 2, lwd = 2)
    graphics::legend("topright", c("observed", "fitted"), lty = c(1, 2), bty = "n")
  }
  invisible(d)
}

#' Plot pupil confound model diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot pupil confound model diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_pupil_confound_model <- function(x,
                                          type = c("luminance", "trial_order", "raw_adjusted", "theta_luminance_surface"), ...) {
  type <- match.arg(type); d <- x$data
  if (!nrow(d)) return(.ep08_plot_empty("Pupil confound model"))
  if (type == "luminance") {
    graphics::plot(d$.luminance, d$.pupil, xlab = "Luminance", ylab = "Raw pupil",
                   main = "Raw pupil response by luminance", ...)
  } else if (type == "trial_order") {
    graphics::plot(d$.trial, d$pupil_confound_adjusted, xlab = "Trial order", ylab = "Adjusted pupil",
                   main = "Adjusted pupil by trial order", ...)
  } else if (type == "raw_adjusted") {
    graphics::plot(d$.pupil, d$pupil_confound_adjusted, xlab = "Raw pupil", ylab = "Adjusted pupil",
                   main = "Raw vs confound-adjusted pupil", ...)
    graphics::abline(0, 1, lty = 2)
  } else {
    if (.ep08_sd(d$.theta) == 0) return(.ep08_plot_empty("Theta x luminance surface", "Theta was not supplied or had no variation"))
    lum <- seq(min(d$.luminance), max(d$.luminance), length.out = 30)
    th <- seq(min(d$.theta), max(d$.theta), length.out = 30)
    grid <- expand.grid(.luminance = lum, .theta = th)
    grid$.trial <- stats::median(d$.trial)
    grid$.person <- d$.person[1L]; grid$.item <- d$.item[1L]
    pred <- tryCatch(stats::predict(x$model, newdata = grid), error = function(e) rep(NA_real_, nrow(grid)))
    Z <- matrix(pred, nrow = length(lum), ncol = length(th))
    graphics::contour(lum, th, Z, xlab = "Luminance", ylab = "Theta",
                      main = "Modelled pupil theta-luminance surface", ...)
  }
  invisible(d)
}

#' Plot aoi trajectory diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot aoi trajectory diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_aoi_trajectory <- function(x, type = c("coefficients", "profiles"), ...) {
  type <- match.arg(type); tab <- x$features
  if (!nrow(tab)) return(.ep08_plot_empty("AOI trajectory"))
  coef_cols <- grep("_gca_degree", names(tab), value = TRUE)
  if (!length(coef_cols)) return(.ep08_plot_empty("AOI trajectory", "No trajectory coefficients"))
  vals <- vapply(coef_cols, function(v) .ep08_mean(tab[[v]]), numeric(1))
  if (type == "coefficients") {
    graphics::dotchart(vals, labels = coef_cols, xlab = "Mean orthogonal-polynomial coefficient",
                       main = "AOI trajectory coefficient map", ...)
    graphics::abline(v = 0, lty = 3)
  } else {
    mat <- as.matrix(tab[coef_cols]);
    graphics::matplot(t(mat), type = "l", lty = 1, axes = FALSE,
                      xlab = "Trajectory feature", ylab = "Coefficient",
                      main = "Participant/trial AOI trajectory profiles", ...)
    graphics::axis(1, at = seq_along(coef_cols), labels = coef_cols, las = 2, cex.axis = .6)
  }
  invisible(tab)
}

#' Plot aoi growth curve diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot aoi growth curve diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_aoi_growth_curve <- function(x, ...) {
  pred <- predict_aoi_trajectory(x)
  graphics::plot(x$time, x$outcome, pch = 16, cex = .6, xlab = "Time", ylab = "AOI response/proportion",
                 main = "AOI growth curve", ...)
  graphics::lines(pred$time, pred$predicted, lwd = 2)
  invisible(pred)
}

#' Plot multiblock process map diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot multiblock process map diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_multiblock_process_map <- function(x,
                                            type = c("individuals", "variables", "blocks", "contributions"), ...) {
  type <- match.arg(type)
  if (type == "individuals") {
    d <- x$person_coordinates
    dims <- names(d)[vapply(d, is.numeric, logical(1))]
    if (length(dims) < 2L) return(.ep08_plot_empty("Multiblock map", "Fewer than two dimensions"))
    graphics::plot(d[[dims[1L]]], d[[dims[2L]]], xlab = dims[1L], ylab = dims[2L],
                   main = paste("Multiblock individuals --", x$engine), ...)
    return(invisible(d))
  }
  if (type == "variables") {
    d <- x$variable_coordinates
    dims <- names(d)[vapply(d, is.numeric, logical(1))]
    if (length(dims) < 2L) return(.ep08_plot_empty("Multiblock variables", "Fewer than two dimensions"))
    graphics::plot(d[[dims[1L]]], d[[dims[2L]]], type = "n", xlab = dims[1L], ylab = dims[2L],
                   main = "Multiblock variable coordinates", ...)
    graphics::text(d[[dims[1L]]], d[[dims[2L]]], labels = d$variable, cex = .7)
    return(invisible(d))
  }
  d <- x$block_coordinates
  if (is.null(d) || !nrow(d)) return(.ep08_plot_empty("Multiblock blocks", "No block coordinates"))
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  if (!length(num)) return(.ep08_plot_empty("Multiblock blocks", "No numeric block coordinates"))
  vals <- rowMeans(abs(as.matrix(d[num])), na.rm = TRUE)
  graphics::barplot(vals, names.arg = d$block, las = 2, ylab = "Mean absolute component coordinate",
                    main = "Multiblock contribution summary", ...)
  invisible(d)
}

#' Plot process profile mixture diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process profile mixture diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_profile_mixture <- function(x,
                                             type = c("profiles", "posterior", "scatter", "parallel"), ...) {
  type <- match.arg(type)
  if (type == "posterior") {
    pcols <- grep("^profile_probability_", names(x$assignment), value = TRUE)
    if (!length(pcols)) return(.ep08_plot_empty("Process profiles", "No posterior/proximity probabilities"))
    M <- as.matrix(x$assignment[pcols])
    graphics::matplot(t(M), type = "l", lty = 1, axes = FALSE,
                      xlab = "Profile", ylab = "Probability/proximity",
                      main = "Process-profile membership uncertainty", ...)
    graphics::axis(1, at = seq_len(ncol(M)), labels = pcols, las = 2)
    return(invisible(M))
  }
  s <- x$summary
  vars <- intersect(x$variables, names(s))
  if (type == "profiles" || type == "parallel") {
    M <- as.matrix(s[vars]); rownames(M) <- s$profile
    graphics::matplot(seq_along(vars), t(M), type = "l", lty = seq_len(nrow(M)), pch = seq_len(nrow(M)),
                      axes = FALSE, xlab = "Process feature", ylab = "Profile mean",
                      main = paste("Process profiles --", x$status), ...)
    graphics::axis(1, at = seq_along(vars), labels = vars, las = 2, cex.axis = .7)
    graphics::legend("topright", rownames(M), lty = seq_len(nrow(M)), pch = seq_len(nrow(M)), bty = "n")
    return(invisible(s))
  }
  Z <- x$scaled_data
  if (ncol(Z) < 2L) return(.ep08_plot_empty("Process-profile scatter", "Fewer than two features"))
  cls <- factor(x$assignment$profile)
  graphics::plot(Z[, 1L], Z[, 2L], pch = as.integer(cls), xlab = colnames(Z)[1L], ylab = colnames(Z)[2L],
                 main = "Process-profile scatter", ...)
  graphics::legend("topright", levels(cls), pch = seq_along(levels(cls)), bty = "n")
  invisible(x$assignment)
}

#' Plot streaming score diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot streaming score diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_streaming_score <- function(x, ...) {
  d <- x$history
  if (!nrow(d) || !any(is.finite(d$theta))) return(.ep08_plot_empty("Streaming score", "No finite score estimates"))
  ylim <- range(c(d$theta - d$theta_se, d$theta + d$theta_se), na.rm = TRUE)
  if (!all(is.finite(ylim))) ylim <- range(d$theta, na.rm = TRUE)
  graphics::plot(d$step, d$theta, type = "b", ylim = ylim,
                 xlab = "Observed-response step", ylab = "Theta estimate",
                 main = paste("Streaming person score --", x$method), ...)
  good <- is.finite(d$theta_se)
  if (any(good)) graphics::arrows(d$step[good], d$theta[good] - d$theta_se[good],
                                  d$step[good], d$theta[good] + d$theta_se[good],
                                  angle = 90, code = 3, length = .04)
  invisible(d)
}

#' Plot process external validity diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process external validity diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_external_validity <- function(x,
                                               type = c("associations", "incremental", "observed_fitted", "residuals"), ...) {
  type <- match.arg(type)
  if (type == "associations") {
    d <- x$associations
    graphics::dotchart(d$correlation, labels = d$predictor, xlim = c(-1, 1),
                       xlab = "Correlation with external criterion",
                       main = "Process external-validity associations", ...)
    graphics::abline(v = 0, lty = 3)
    return(invisible(d))
  }
  if (type == "incremental") {
    d <- incremental_process_validity(x)
    vals <- c(baseline = d$baseline_r2, full = d$full_r2)
    ymax <- max(vals, na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) ymax <- 1
    graphics::barplot(vals, ylim = c(0, ymax * 1.1), ylab = expression(R^2),
                      main = paste("Incremental process validity; delta R2 =", round(d$incremental_r2, 3)), ...)
    return(invisible(d))
  }
  f <- stats::fitted(x$full_model); r <- stats::residuals(x$full_model); y <- x$data[[x$criterion]]
  if (type == "observed_fitted") {
    graphics::plot(f, y, xlab = "Fitted external criterion", ylab = "Observed external criterion",
                   main = "Observed vs fitted external criterion", ...)
    graphics::abline(0, 1, lty = 2)
  } else {
    graphics::plot(f, r, xlab = "Fitted external criterion", ylab = "Residual",
                   main = "External-validity residual check", ...)
    graphics::abline(h = 0, lty = 2)
  }
  invisible(x)
}

#' Plot signal filter audit diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot signal filter audit diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_signal_filter_audit <- function(x, ...) {
  d <- x$data
  graphics::plot(d$sample_index, d$raw, type = "l", xlab = "Sample index", ylab = "Signal",
                 main = paste("Raw and filtered eye signal --", x$method), ...)
  graphics::lines(d$sample_index, d$filtered, lwd = 2, lty = 2)
  graphics::legend("topright", c("raw", "filtered"), lty = c(1, 2), lwd = c(1, 2), bty = "n")
  invisible(d)
}

#' Plot item parameter seed diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot item parameter seed diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param candidate_data Optional candidate-item data used for the requested display.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_item_parameter_seed <- function(x, candidate_data = NULL, ...) {
  if (!inherits(x, "eye_item_parameter_seed")) stop("x must be eye_item_parameter_seed.", call. = FALSE)
  if (is.null(candidate_data)) {
    d <- x$training_data
    graphics::plot(d[[x$difficulty]], d[[x$discrimination]],
                   xlab = "Calibrated difficulty", ylab = "Calibrated discrimination",
                   main = "Item-parameter seed training space", ...)
    return(invisible(d))
  }
  p <- predict_item_parameter_priors(x, candidate_data)
  graphics::plot(p$predicted_pre_pilot_difficulty, p$predicted_pre_pilot_discrimination,
                 xlab = "Predicted pre-pilot difficulty", ylab = "Predicted pre-pilot discrimination",
                 main = "Candidate item screening map", ...)
  invisible(p)
}

#' Plot candidate item bank audit diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot candidate item bank audit diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_candidate_item_bank_audit <- function(x, ...) {
  p <- x$table
  graphics::plot(p$predicted_pre_pilot_difficulty, p$predicted_pre_pilot_discrimination,
                 pch = ifelse(p$review_required, 4, 1),
                 xlab = "Predicted pre-pilot difficulty", ylab = "Predicted pre-pilot discrimination",
                 main = "Candidate item-bank screening audit", ...)
  invisible(p)
}

#' Plot visual context irt diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot visual context irt diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_visual_context_irt <- function(x,
                                        type = c("loadings", "difficulty_change", "context_registry"), ...) {
  type <- match.arg(type)
  if (type == "context_registry") {
    m <- x$registry$mapping
    counts <- sort(table(m$visual_context_id), decreasing = TRUE)
    graphics::barplot(counts, las = 2, ylab = "Items", main = "Visual-context item registry", ...)
    return(invisible(m))
  }
  if (!requireNamespace("mirt", quietly = TRUE)) stop("Package `mirt` is required to plot this fitted object.", call. = FALSE)
  base <- tryCatch(mirt::coef(x$base_model, simplify = TRUE, IRTpars = TRUE)$items, error = function(e) NULL)
  ctx_raw <- tryCatch(mirt::coef(x$context_model, simplify = TRUE, IRTpars = FALSE)$items, error = function(e) NULL)
  if (is.null(ctx_raw)) return(.ep08_plot_empty("Visual-context IRT", "Could not extract item parameters"))
  if (type == "loadings") {
    acols <- grep("^a", colnames(ctx_raw), value = TRUE)
    if (length(acols) < 2L) return(.ep08_plot_empty("Visual-context loadings", "Second loading unavailable"))
    graphics::plot(ctx_raw[, acols[1L]], ctx_raw[, acols[2L]],
                   xlab = acols[1L], ylab = acols[2L],
                   main = paste("Ability vs visual-context loading:", x$context), ...)
    graphics::text(ctx_raw[, acols[1L]], ctx_raw[, acols[2L]], labels = rownames(ctx_raw), pos = 3, cex = .7)
    return(invisible(ctx_raw))
  }
  if (is.null(base) || !"b" %in% colnames(base) || !all(c("a1", "d") %in% colnames(ctx_raw)))
    return(.ep08_plot_empty("Difficulty change", "IRT difficulty parameters unavailable"))
  ctx_b <- -ctx_raw[, "d"] / pmax(abs(ctx_raw[, "a1"]), 1e-8)
  delta <- ctx_b - base[, "b"]
  graphics::dotchart(delta, labels = rownames(ctx_raw), xlab = "Context-adjusted minus base difficulty",
                     main = "Difficulty change after visual-context adjustment", ...)
  graphics::abline(v = 0, lty = 3)
  invisible(delta)
}

#' Plot validation bundle diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot validation bundle diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_validation_bundle <- function(x, ...) {
  m <- validation_bundle_manifest(x)
  score <- c(missing = 0, empty = 0.25, error = 0, available = 1)[m$status]
  graphics::barplot(score, names.arg = m$slot, las = 2, ylim = c(0, 1),
                    ylab = "Evidence availability", main = paste("Validation bundle:", x$model_name), ...)
  invisible(m)
}

# Standalone plot utilities ---------------------------------------------------

#' Plot raw-to-processed pupil preprocessing stages
#'
#' @param data Sample-level data.
#' @param time Time column.
#' @param signals Signal columns to overlay.
#' @return A tabular R object containing plot raw-to-processed pupil preprocessing stages; rows represent analysis units and columns contain the returned quantities.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_pupil_preprocessing_audit <- function(
    data, time = "time_ms",
    signals = c("pupil_raw", "pupil_interpolated", "pupil_smoothed", "pupil_bc"), ...) {
  data <- .ep08_as_df(data); .ep08_req_cols(data, time)
  signals <- intersect(signals, names(data))
  if (!length(signals)) stop("No requested pupil preprocessing signals were found.", call. = FALSE)
  tt <- .ep08_num(data[[time]])
  Y <- as.matrix(data[signals]); storage.mode(Y) <- "double"
  graphics::matplot(tt, Y, type = "l", lty = seq_len(ncol(Y)),
                    xlab = "Time", ylab = "Pupil signal",
                    main = "Pupil preprocessing audit", ...)
  graphics::legend("topright", signals, lty = seq_len(ncol(Y)), bty = "n")
  invisible(data[, c(time, signals), drop = FALSE])
}

#' Plot tonic/phasic pupil components
#' @return A tabular R object containing plot tonic/phasic pupil components; rows represent analysis units and columns contain the returned quantities.
#' @export
#' @param data Data frame containing the required process variables.
#' @param time Time values or name of the time variable.
#' @param smoothed Name of the smoothed pupil-signal column.
#' @param tonic Name of the tonic pupil-component column.
#' @param phasic Name of the phasic pupil-component column.
#' @param ... Additional arguments passed to the underlying method or helper.
plot_pupil_components <- function(data, time = "time_ms", smoothed = "pupil_smoothed",
                                  tonic = "pupil_tonic", phasic = "pupil_phasic", ...) {
  plot_pupil_preprocessing_audit(data, time = time, signals = c(smoothed, tonic, phasic), ...)
}

.ep08_transition_table <- function(data, from = "from", to = "to", normalize = c("from", "all", "none")) {
  normalize <- match.arg(normalize)
  data <- .ep08_as_df(data); .ep08_req_cols(data, c(from, to))
  fromv <- as.character(data[[from]]); tov <- as.character(data[[to]])
  ok <- !is.na(fromv) & nzchar(fromv) & !is.na(tov) & nzchar(tov)
  tab <- table(fromv[ok], tov[ok])
  if (!length(tab) || nrow(tab) == 0L || ncol(tab) == 0L) stop("No complete AOI transitions are available.", call. = FALSE)
  if (normalize == "from") {
    den <- rowSums(tab); den[den == 0] <- 1; tab <- sweep(tab, 1L, den, "/")
  } else if (normalize == "all") {
    total <- sum(tab)
    if (total > 0) tab <- tab / total
  }
  tab
}

#' Plot an AOI transition matrix
#' @param data Transition-pair data.
#' @param from,to Column names.
#' @param normalize Normalize within from-AOI, globally, or not at all.
#' @return An R object containing plot an AOI transition matrix. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_aoi_transition_matrix <- function(data, from = "from", to = "to",
                                       normalize = c("from", "all", "none"), ...) {
  normalize <- match.arg(normalize)
  M <- .ep08_transition_table(data, from, to, normalize)
  graphics::image(seq_len(ncol(M)), seq_len(nrow(M)), t(M[nrow(M):1, , drop = FALSE]),
                  axes = FALSE, xlab = "To AOI", ylab = "From AOI",
                  main = paste("AOI transition matrix -- normalize:", normalize), ...)
  graphics::axis(1, at = seq_len(ncol(M)), labels = colnames(M), las = 2)
  graphics::axis(2, at = seq_len(nrow(M)), labels = rev(rownames(M)), las = 2)
  invisible(M)
}

#' Plot top AOI transitions by probability/count
#' @return An R object containing plot top AOI transitions by probability/count. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param data Data frame containing the required process variables.
#' @param from Name of the column identifying the transition origin.
#' @param to Name of the column identifying the transition destination.
#' @param normalize Normalization rule applied to transition counts or weights.
#' @param top_n Maximum number of highest-ranked entries to display.
#' @param ... Additional arguments passed to the underlying method or helper.
plot_aoi_transition_rank <- function(data, from = "from", to = "to",
                                     normalize = c("from", "all", "none"), top_n = 20L, ...) {
  normalize <- match.arg(normalize)
  M <- .ep08_transition_table(data, from, to, normalize)
  df <- as.data.frame(as.table(M), stringsAsFactors = FALSE)
  names(df) <- c("from", "to", "value")
  df$transition <- paste(df$from, "->", df$to)
  df <- df[order(df$value, decreasing = TRUE), , drop = FALSE]
  df <- head(df, as.integer(top_n))
  graphics::barplot(rev(df$value), names.arg = rev(df$transition), horiz = TRUE, las = 1,
                    xlab = if (normalize == "none") "Count" else "Transition probability",
                    main = "Top AOI transitions", ...)
  invisible(df)
}

#' Plot process-feature stability across resamples/splits
#'
#' @param data Table with feature and stability/rank information.
#' @param feature Feature column.
#' @param stability Stability/selection-rate column.
#' @param top_n Maximum features shown.
#' @return An R object containing plot process-feature stability across resamples/splits. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_process_feature_stability <- function(data, feature = "feature",
                                           stability = "selection_rate", top_n = 20L, ...) {
  data <- .ep08_as_df(data); .ep08_req_cols(data, c(feature, stability))
  d <- data[order(.ep08_num(data[[stability]]), decreasing = TRUE), , drop = FALSE]
  d <- head(d, as.integer(top_n))
  graphics::barplot(rev(.ep08_num(d[[stability]])), names.arg = rev(as.character(d[[feature]])),
                    horiz = TRUE, las = 1, xlab = "Stability / selection rate",
                    main = "Process-feature stability", ...)
  invisible(d)
}

#' Plot channel-ablation delta from a full/reference model
#'
#' @param x `eye_process_channel_ablation` object or compatible table.
#' @param table Optional explicit table.
#' @param channel_col,metric_col,value_col Column names.
#' @param full_label Full/reference channel label.
#' @return A data frame containing plot channel-ablation delta from a full/reference model. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param metric Metric to evaluate or display.
#' @param ... Additional arguments passed to the underlying method or helper.
plot_process_channel_ablation_delta <- function(
    x = NULL, table = NULL, channel_col = "channel", metric_col = "metric",
    value_col = "value", full_label = "full", metric = NULL, ...) {
  if (is.null(table)) {
    if (is.data.frame(x)) table <- x else if (is.list(x)) {
      candidates <- c("results", "table", "summary", "metrics")
      hit <- candidates[candidates %in% names(x)][1L]
      if (!is.na(hit)) table <- x[[hit]]
    }
  }
  table <- .ep08_as_df(table, "ablation table")
  .ep08_req_cols(table, c(channel_col, value_col))
  if (!is.null(metric) && metric_col %in% names(table)) table <- table[table[[metric_col]] %in% metric, , drop = FALSE]
  if (!nrow(table)) stop("No ablation rows to plot.", call. = FALSE)
  ch <- as.character(table[[channel_col]]); val <- .ep08_num(table[[value_col]])
  ref <- val[ch == full_label & is.finite(val)]
  if (!length(ref)) ref <- val[is.finite(val)]
  if (!length(ref)) stop("No finite ablation metric values are available.", call. = FALSE)
  ref <- max(ref)
  delta <- val - ref
  ord <- order(delta)
  graphics::dotchart(delta[ord], labels = ch[ord], xlab = "Metric difference from full/reference",
                     main = "Process-channel ablation delta", ...)
  graphics::abline(v = 0, lty = 2)
  invisible(data.frame(channel = ch, value = val, delta_from_reference = delta))
}

# Additional representation plot methods ------------------------------------

#' Plot process windows diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process windows diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param feature Process feature to evaluate or display.
#' @param group Optional grouping variable.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_windows <- function(x, feature = "pupil_mean", group = NULL, ...) {
  d <- x$data
  if (!nrow(d) || !feature %in% names(d)) return(.ep08_plot_empty("Process windows"))
  if (is.null(group) || !group %in% names(d)) {
    agg <- stats::aggregate(d[[feature]], by = list(window_mid = d$window_mid), FUN = .ep08_mean)
    names(agg)[2L] <- "value"
    graphics::plot(agg$window_mid, agg$value, type = "b", xlab = "Window midpoint (ms)",
                   ylab = feature, main = "Windowed process trajectory", ...)
    return(invisible(agg))
  }
  groups <- unique(as.character(d[[group]]))
  series <- lapply(groups, function(g) {
    z <- d[as.character(d[[group]]) == g, , drop = FALSE]
    a <- stats::aggregate(z[[feature]], by = list(window_mid = z$window_mid), FUN = .ep08_mean)
    names(a)[2L] <- "value"; a$group <- g; a
  })
  all <- do.call(rbind, series)
  xs <- sort(unique(all$window_mid))
  M <- sapply(groups, function(g) {
    z <- all[all$group == g, ]; z$value[match(xs, z$window_mid)]
  })
  graphics::matplot(xs, M, type = "l", lty = seq_along(groups),
                    xlab = "Window midpoint (ms)", ylab = feature,
                    main = "Windowed process trajectories by group", ...)
  graphics::legend("topright", groups, lty = seq_along(groups), bty = "n")
  invisible(all)
}

#' Plot pupil fatigue drift diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot pupil fatigue drift diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_pupil_fatigue_drift <- function(x, ...) {
  d <- x$data
  if (!nrow(d)) return(.ep08_plot_empty("Pupil fatigue/drift"))
  graphics::plot(d$trial_order, d$pupil, pch = 16, cex = .5,
                 xlab = "Trial order", ylab = "Pupil response",
                 main = paste("Within-person trial-order sensitivity --", x$engine), ...)
  fit <- tryCatch(stats::lm(pupil ~ trial_order, data = d), error = function(e) NULL)
  if (!is.null(fit)) graphics::abline(fit, lty = 2, lwd = 2)
  invisible(d)
}

#' Plot process feature blocks diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process feature blocks diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_feature_blocks <- function(x, ...) {
  graphics::barplot(x$block_sizes, names.arg = names(x$block_sizes), las = 2,
                    ylab = "Variables", main = "Process-feature block sizes", ...)
  invisible(x$block_sizes)
}

#' Plot presentation fairness comparison diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot presentation fairness comparison diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_presentation_fairness_comparison <- function(x, ...) {
  d <- x$summary
  if (!nrow(d)) return(.ep08_plot_empty("Presentation fairness comparison"))
  # aggregate() with vector-valued FUN creates a matrix/list-like second column.
  y <- d$outcome
  if (is.matrix(y)) means <- y[, "mean"] else {
    means <- vapply(y, function(z) if (length(z) >= 2L) z[2L] else NA_real_, numeric(1))
  }
  graphics::barplot(means, names.arg = as.character(d$variant), las = 2,
                    ylab = "Mean outcome", main = "Presentation-variant outcome comparison", ...)
  invisible(d)
}

#' Plot a pupil-signal power spectrum
#' @param signal Numeric pupil signal.
#' @param sampling_rate_hz Sampling rate.
#' @param max_hz Maximum frequency shown; defaults to Nyquist.
#' @return A data frame containing plot a pupil-signal power spectrum. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_pupil_spectrum <- function(signal, sampling_rate_hz, max_hz = sampling_rate_hz / 2, ...) {
  if (!is.finite(sampling_rate_hz) || sampling_rate_hz <= 0) stop("sampling_rate_hz must be positive.", call. = FALSE)
  if (!is.finite(max_hz) || max_hz <= 0) stop("max_hz must be positive.", call. = FALSE)
  y <- .ep08_interp_signal(signal)
  if (length(y) < 8L || !all(is.finite(y))) stop("At least eight samples with sufficient finite signal values are required.", call. = FALSE)
  y <- y - mean(y, na.rm = TRUE)
  n <- length(y); f <- stats::fft(y)
  power <- Mod(f)^2 / n
  freq <- (0:(n - 1L)) * sampling_rate_hz / n
  keep <- freq <= max_hz & freq <= sampling_rate_hz / 2
  graphics::plot(freq[keep], power[keep], type = "l", xlab = "Frequency (Hz)",
                 ylab = "FFT power", main = "Pupil signal spectrum", ...)
  invisible(data.frame(frequency_hz = freq[keep], power = power[keep]))
}

#' Plot pupil low/high-band power summaries
#' @param x `eye_pupil_frequency_features` object.
#' @return An R object containing plot pupil low/high-band power summaries. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_pupil_band_power <- function(x, ...) {
  if (!inherits(x, "eye_pupil_frequency_features")) stop("x must be eye_pupil_frequency_features.", call. = FALSE)
  d <- x$features
  vals <- c(low_band = .ep08_mean(d$pupil_low_frequency_power),
            high_band = .ep08_mean(d$pupil_high_frequency_power))
  graphics::barplot(vals, ylab = "Mean band power", main = "Pupil frequency-band power", ...)
  invisible(vals)
}

#' Plot pupil activity features across windows/groups
#' @param x `eye_pupil_frequency_features` or `eye_pupil_frequency_stability` object.
#' @param feature Activity feature.
#' @return An R object containing plot pupil activity features across windows/groups. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_pupil_activity_windows <- function(x, feature = "pupil_frequency_contrast", ...) {
  d <- if (inherits(x, "eye_pupil_frequency_features")) x$features else if (inherits(x, "eye_pupil_frequency_stability")) x$table else
    stop("x must be a pupil frequency feature/stability object.", call. = FALSE)
  if (!feature %in% names(d)) stop("feature not found in object.", call. = FALSE)
  if (!nrow(d)) return(.ep08_plot_empty("Pupil activity", "No feature rows"))
  graphics::plot(seq_len(nrow(d)), .ep08_num(d[[feature]]), type = "b",
                 xlab = "Window/group index", ylab = feature,
                 main = "Pupil activity across windows/groups", ...)
  invisible(d)
}

#' Plot pupil activity sensitivity to window length
#' @param x `eye_pupil_frequency_stability` object.
#' @param feature Feature name.
#' @return An R object containing plot pupil activity sensitivity to window length. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
plot_pupil_activity_sensitivity <- function(x, feature = "pupil_frequency_contrast", ...) {
  plot.eye_pupil_frequency_stability(x, feature = feature, ...)
}

#' Explicit wrapper for process-window sensitivity plotting
#' @return An R object containing explicit wrapper for process-window sensitivity plotting. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot_process_window_sensitivity <- function(x, ...) plot.eye_process_window_sensitivity(x, ...)
