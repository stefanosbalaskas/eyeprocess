.plot_empty <- function(message = "No data available", main = NULL) {
  graphics::plot.new()
  if (!is.null(main)) graphics::title(main = main)
  graphics::text(0.5, 0.5, message)
  invisible(NULL)
}

plot.eye_dataset <- function(x, y = NULL, ...) plot_eye_overview(x, ...)

plot_eye_overview <- function(x, ...) {
  .assert_eye_dataset(x)
  counts <- c(
    recordings = nrow(x$recordings), gaze_samples = nrow(x$gaze_samples),
    eye_samples = nrow(x$eye_samples), episodes = nrow(x$episodes),
    events = nrow(x$events), trials = sum(x$intervals$interval_type == "trial"),
    responses = nrow(x$responses), biometrics = nrow(x$biometrics),
    features = nrow(x$features)
  )
  graphics::barplot(counts, las = 2, main = "eyeprocess dataset overview", ylab = "Rows / counts", ...)
  invisible(counts)
}

.select_trial_data <- function(d, trial_id = NULL, recording_id = NULL) {
  if (!is.null(recording_id)) d <- d[d$recording_id %in% recording_id, , drop = FALSE]
  if (!is.null(trial_id) && "trial_id" %in% names(d)) d <- d[d$trial_id %in% trial_id, , drop = FALSE]
  d
}

plot_eye_trace <- function(x, trial_id = NULL, recording_id = NULL, valid_only = TRUE, reverse_y = TRUE, main = "Gaze trace", ...) {
  .assert_eye_dataset(x)
  d <- .select_trial_data(x$gaze_samples, trial_id, recording_id)
  if (valid_only) d <- d[d$valid %in% TRUE & is.finite(d$gaze_x) & is.finite(d$gaze_y), ]
  d <- d[order(d$timestamp_seconds), ]
  if (!nrow(d)) return(.plot_empty("No gaze samples match the selection.", main))
  ylim <- range(d$gaze_y, na.rm = TRUE)
  if (reverse_y) ylim <- rev(ylim)
  graphics::plot(d$gaze_x, d$gaze_y, type = "l", xlab = "Gaze x", ylab = "Gaze y", ylim = ylim, main = main, ...)
  graphics::points(d$gaze_x, d$gaze_y, pch = 16, cex = 0.25)
  invisible(d)
}

plot_fixations <- function(x, trial_id = NULL, recording_id = NULL, source = c("all", "vendor", "eyeprocess"), scale = 0.03, reverse_y = TRUE, main = "Fixations", ...) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  d <- x$episodes[x$episodes$episode_type == "fixation", ]
  d <- .select_trial_data(d, trial_id, recording_id)
  if (source != "all") d <- d[d$derived_by == source, ]
  d <- d[is.finite(d$centroid_x) & is.finite(d$centroid_y), ]
  if (!nrow(d)) return(.plot_empty("No fixations match the selection.", main))
  ylim <- range(d$centroid_y, na.rm = TRUE); if (reverse_y) ylim <- rev(ylim)
  cex <- pmax(0.5, sqrt(pmax(d$duration_ms, 1)) * scale)
  graphics::plot(d$centroid_x, d$centroid_y, xlab = "Fixation x", ylab = "Fixation y", ylim = ylim,
    pch = 21, cex = cex, main = main, ...)
  invisible(d)
}

plot_scanpath <- function(x, trial_id = NULL, recording_id = NULL, reverse_y = TRUE, label = TRUE, main = "Scanpath", ...) {
  .assert_eye_dataset(x)
  d <- x$episodes[x$episodes$episode_type %in% c("fixation", "aoi_visit"), ]
  if (any(d$episode_type == "aoi_visit")) d <- d[d$episode_type == "aoi_visit", ] else d <- d[d$episode_type == "fixation", ]
  d <- .select_trial_data(d, trial_id, recording_id)
  d <- d[order(d$start_time), ]
  d <- d[is.finite(d$centroid_x) & is.finite(d$centroid_y), ]
  if (!nrow(d)) return(.plot_empty("No scanpath episodes match the selection.", main))
  ylim <- range(d$centroid_y, na.rm = TRUE); if (reverse_y) ylim <- rev(ylim)
  graphics::plot(d$centroid_x, d$centroid_y, type = "o", pch = 21,
    cex = pmax(0.7, sqrt(pmax(d$duration_ms, 1)) / 15),
    xlab = "X", ylab = "Y", ylim = ylim, main = main, ...)
  if (label) graphics::text(d$centroid_x, d$centroid_y, labels = seq_len(nrow(d)), pos = 3, cex = 0.7)
  invisible(d)
}

plot_gaze_heatmap <- function(x, trial_id = NULL, recording_id = NULL, bins = c(50L, 50L), valid_only = TRUE, main = "Gaze density", ...) {
  .assert_eye_dataset(x)
  d <- .select_trial_data(x$gaze_samples, trial_id, recording_id)
  if (valid_only) d <- d[d$valid %in% TRUE, ]
  d <- d[is.finite(d$gaze_x) & is.finite(d$gaze_y), ]
  if (!nrow(d)) return(.plot_empty("No gaze samples match the selection.", main))
  xr <- range(d$gaze_x); yr <- range(d$gaze_y)
  if (diff(xr) == 0) xr <- xr + c(-0.5, 0.5)
  if (diff(yr) == 0) yr <- yr + c(-0.5, 0.5)
  xb <- seq(xr[1L], xr[2L], length.out = as.integer(bins[1L]) + 1L)
  yb <- seq(yr[1L], yr[2L], length.out = as.integer(bins[2L]) + 1L)
  xi <- cut(d$gaze_x, xb, include.lowest = TRUE, labels = FALSE)
  yi <- cut(d$gaze_y, yb, include.lowest = TRUE, labels = FALSE)
  z <- matrix(0, nrow = length(xb) - 1L, ncol = length(yb) - 1L)
  tab <- table(xi, yi)
  z[as.integer(rownames(tab)), as.integer(colnames(tab))] <- tab
  graphics::image(xb[-length(xb)], yb[-length(yb)], z, xlab = "Gaze x", ylab = "Gaze y", main = main, useRaster = TRUE, ...)
  invisible(list(x_breaks = xb, y_breaks = yb, density = z))
}

plot_aoi_dwell <- function(x, feature = c("dwell_time_ms", "dwell_proportion"), aggregate = mean, main = NULL, ...) {
  .assert_eye_dataset(x)
  feature <- match.arg(feature)
  f <- x$features[x$features$feature_name == feature & !is.na(x$features$aoi_id), ]
  if (!nrow(f)) return(.plot_empty("No AOI dwell features available.", main %||% "AOI dwell"))
  vals <- tapply(f$value, f$aoi_id, aggregate, na.rm = TRUE)
  graphics::barplot(vals, las = 2, ylab = feature, main = main %||% paste("AOI", feature), ...)
  invisible(vals)
}

plot_transition_matrix <- function(x, normalize = c("row", "none", "all"), source = c("visits", "fixations", "samples"), main = "AOI transition matrix", ...) {
  normalize <- match.arg(normalize); source <- match.arg(source)
  mat <- transition_matrix(x, normalize = normalize, source = source)
  if (!length(mat)) return(.plot_empty("No transitions available.", main))
  graphics::image(seq_len(nrow(mat)), seq_len(ncol(mat)), t(mat[nrow(mat):1L, , drop = FALSE]), axes = FALSE,
    xlab = "From AOI", ylab = "To AOI", main = main, ...)
  graphics::axis(1, at = seq_len(nrow(mat)), labels = rownames(mat), las = 2)
  graphics::axis(2, at = seq_len(ncol(mat)), labels = rev(colnames(mat)), las = 2)
  invisible(mat)
}

plot_pupil_timeseries <- function(x, trial_id = NULL, recording_id = NULL, eye = NULL, column = "pupil_diameter", main = "Pupil time series", ...) {
  .assert_eye_dataset(x)
  d <- .select_trial_data(x$eye_samples, trial_id, recording_id)
  if (!is.null(eye)) d <- d[d$eye %in% eye, ]
  if (!column %in% names(d)) .eye_stop("Pupil column `", column, "` not found.")
  d <- d[order(d$recording_id, d$eye, d$timestamp_seconds), ]
  if (!nrow(d)) return(.plot_empty("No pupil observations match the selection.", main))
  groups <- interaction(d$recording_id, d$eye, drop = TRUE)
  split_data <- split(d, groups)
  yr <- range(d[[column]], na.rm = TRUE)
  xr <- range(d$timestamp_seconds, na.rm = TRUE)
  if (!all(is.finite(c(xr, yr)))) return(.plot_empty("Pupil values are unavailable.", main))
  first <- split_data[[1L]]
  graphics::plot(first$timestamp_seconds, first[[column]], type = "l", xlim = xr, ylim = yr,
    xlab = "Time (seconds)", ylab = column, main = main, ...)
  if (length(split_data) > 1L) {
    for (i in 2:length(split_data)) {
      z <- split_data[[i]]
      graphics::lines(z$timestamp_seconds, z[[column]], lty = i)
    }
  }
  graphics::legend("topright", legend = names(split_data), lty = seq_along(split_data), cex = 0.7)
  invisible(d)
}

plot_biometrics <- function(x, channels = NULL, trial_id = NULL, recording_id = NULL, main = "Biometric streams", ...) {
  .assert_eye_dataset(x)
  d <- .select_trial_data(x$biometrics, trial_id, recording_id)
  if (!is.null(channels)) d <- d[d$channel %in% channels, ]
  if (!nrow(d)) return(.plot_empty("No biometric observations match the selection.", main))
  channels <- unique(d$channel)
  old <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old), add = TRUE)
  graphics::par(mfrow = c(length(channels), 1L), mar = c(3, 4, 2, 1))
  for (ch in channels) {
    z <- d[d$channel == ch, ]; z <- z[order(z$timestamp_seconds), ]
    graphics::plot(z$timestamp_seconds, z$value, type = "l", xlab = "Time (seconds)", ylab = .first_nonmissing(z$unit, "value"), main = ch, ...)
  }
  invisible(d)
}

plot_signal_quality <- function(x, by_trial = FALSE, main = "Signal quality", ...) {
  q <- audit_signal_quality(x, by_trial = by_trial)
  if (!nrow(q)) return(.plot_empty("No signal-quality metrics available.", main))
  labels <- if (by_trial) paste(q$recording_id, q$trial_id, q$metric, sep = ":") else paste(q$recording_id, q$metric, sep = ":")
  graphics::barplot(q$value, names.arg = labels, las = 2, ylim = c(0, max(1, q$value, na.rm = TRUE)), ylab = "Fraction", main = main, ...)
  invisible(q)
}

plot_sampling_rate <- function(x, expected_hz = NULL, main = "Estimated sampling rate", ...) {
  q <- audit_sampling_rate(x, expected_hz = expected_hz)
  if (!nrow(q)) return(.plot_empty("No gaze sampling rates available.", main))
  graphics::barplot(q$value, names.arg = q$recording_id, las = 2, ylab = "Hz", main = main, ...)
  if (!is.null(expected_hz)) graphics::abline(h = expected_hz, lty = 2)
  invisible(q)
}

plot_missingness <- function(x, component = c("gaze_samples", "eye_samples", "biometrics"), top = 20L, main = NULL, ...) {
  component <- match.arg(component)
  d <- audit_missingness(x, component = component)
  if (!nrow(d)) return(.plot_empty("No component data available.", main %||% "Missingness"))
  agg <- stats::aggregate(missing_fraction ~ field, d, mean)
  agg <- agg[order(agg$missing_fraction, decreasing = TRUE), ]
  agg <- head(agg, top)
  graphics::barplot(agg$missing_fraction, names.arg = agg$field, las = 2, ylab = "Missing fraction", main = main %||% paste(component, "missingness"), ...)
  invisible(agg)
}

plot_trial_timeline <- function(x, recording_id = NULL, main = "Trial timeline", ...) {
  .assert_eye_dataset(x)
  d <- trial_table(x)
  if (!is.null(recording_id)) d <- d[d$recording_id %in% recording_id, ]
  d <- d[order(d$recording_id, d$start_time), ]
  if (!nrow(d)) return(.plot_empty("No trials available.", main))
  y <- seq_len(nrow(d))
  graphics::plot(range(c(d$start_time, d$end_time), na.rm = TRUE), range(y), type = "n", xlab = "Time (seconds)", ylab = "Trial", yaxt = "n", main = main, ...)
  graphics::segments(d$start_time, y, d$end_time, y, lwd = 3)
  graphics::axis(2, at = y, labels = d$trial_id, las = 2, cex.axis = 0.7)
  invisible(d)
}

plot_feature_distribution <- function(x, feature_name, group = NULL, main = NULL, ...) {
  .assert_eye_dataset(x)
  f <- x$features[x$features$feature_name %in% feature_name, ]
  if (!nrow(f)) return(.plot_empty("Feature not found.", main %||% feature_name))
  if (is.null(group) || !group %in% names(f)) {
    graphics::hist(f$value, xlab = feature_name, main = main %||% paste("Distribution of", feature_name), ...)
  } else {
    graphics::boxplot(f$value ~ f[[group]], xlab = group, ylab = feature_name, main = main %||% feature_name, ...)
  }
  invisible(f)
}

plot_feature_correlation <- function(x, features = NULL, main = "Feature correlations", ...) {
  .assert_eye_dataset(x)
  wide <- features_wide(x)
  if (!is.null(features)) wide <- wide[c(intersect(names(wide), c("recording_id", "participant_id", "trial_id", "item_id")), intersect(features, names(wide)))]
  num <- wide[vapply(wide, is.numeric, logical(1))]
  if (ncol(num) < 2L) return(.plot_empty("At least two numeric features are required.", main))
  mat <- stats::cor(num, use = "pairwise.complete.obs")
  graphics::image(seq_len(nrow(mat)), seq_len(ncol(mat)), t(mat[nrow(mat):1L, ]), zlim = c(-1, 1), axes = FALSE, main = main, ...)
  graphics::axis(1, at = seq_len(nrow(mat)), labels = rownames(mat), las = 2)
  graphics::axis(2, at = seq_len(ncol(mat)), labels = rev(colnames(mat)), las = 2)
  invisible(mat)
}

plot_coordinate_spaces <- function(x, main = "Coordinate-space usage", ...) {
  d <- audit_coordinate_spaces(x)
  if (!nrow(d)) return(.plot_empty("No coordinate spaces are in use.", main))
  mat <- t(as.matrix(d[c("n_gaze", "n_episodes", "n_aoi_geometry")]))
  colnames(mat) <- d$coordinate_space_id
  graphics::barplot(mat, beside = FALSE, las = 2, legend.text = rownames(mat), main = main, ylab = "Rows", ...)
  invisible(d)
}

plot_clock_alignment <- function(x, channel = NULL, main = "Gaze and biometric clocks", ...) {
  d <- audit_clock_sync(x, channel = channel)
  if (!nrow(d) || !"gaze_start" %in% names(d)) return(.plot_empty("Clock overlap cannot be evaluated.", main))
  y <- seq_len(nrow(d))
  graphics::plot(range(c(d$gaze_start, d$gaze_end, d$biometric_start, d$biometric_end), na.rm = TRUE), range(y) + c(-0.5, 0.5), type = "n", xlab = "Time (seconds)", ylab = "Recording", yaxt = "n", main = main, ...)
  graphics::segments(d$gaze_start, y + 0.12, d$gaze_end, y + 0.12, lwd = 4)
  graphics::segments(d$biometric_start, y - 0.12, d$biometric_end, y - 0.12, lwd = 4, lty = 2)
  graphics::axis(2, at = y, labels = d$recording_id, las = 2)
  graphics::legend("topright", legend = c("Gaze", "Biometrics"), lty = c(1, 2), lwd = 3)
  invisible(d)
}

plot_item_difficulty <- function(model, ...) {
  pars <- item_parameters(model)
  if (!nrow(pars)) return(.plot_empty("No item parameters available.", "Item difficulty"))
  difficulty_col <- .first_existing(names(pars), c("difficulty", "b", "d"))
  if (is.null(difficulty_col)) return(.plot_empty("Difficulty parameter not identified.", "Item difficulty"))
  graphics::dotchart(pars[[difficulty_col]], labels = pars$item_id, xlab = "Difficulty", main = "Item difficulty", ...)
  invisible(pars)
}

plot_model_diagnostics <- function(model, ...) {
  if (inherits(model, "eyeprocess_model")) {
    if (!is.null(model$fit) && inherits(model$fit, "glm")) {
      graphics::plot(model$fit, ...)
      return(invisible(model))
    }
    if (!is.null(model$fit) && inherits(model$fit, "lm")) {
      graphics::plot(model$fit, ...)
      return(invisible(model))
    }
  }
  .plot_empty("No generic diagnostic plot is available for this model class.", "Model diagnostics")
}
