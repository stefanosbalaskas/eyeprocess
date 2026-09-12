# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Windowed response-process representations and AOI trajectory features.

#' Specify temporal process windows
#'
#' @param width_ms Window width in milliseconds.
#' @param step_ms Step between successive windows.
#' @param start_ms,end_ms Analysis range relative to the chosen alignment origin.
#' @param align Alignment label, e.g. stimulus, response, or custom.
#' @param min_samples Minimum samples required within a window.
#' @return An `eye_process_window_spec` object.
#' @export
process_window_spec <- function(width_ms = 1000, step_ms = 500,
                                start_ms = 0, end_ms = 3000,
                                align = c("stimulus", "response", "custom"),
                                min_samples = 5L) {
  align <- match.arg(align)
  vals <- c(width_ms, step_ms, start_ms, end_ms)
  if (any(!is.finite(vals))) stop("Window timing values must be finite.", call. = FALSE)
  if (width_ms <= 0 || step_ms <= 0 || end_ms <= start_ms)
    stop("Require width_ms > 0, step_ms > 0, and end_ms > start_ms.", call. = FALSE)
  if (width_ms > end_ms - start_ms)
    stop("width_ms cannot exceed the analysis range.", call. = FALSE)
  min_samples <- as.integer(min_samples)
  if (min_samples < 2L) stop("min_samples must be at least 2.", call. = FALSE)
  structure(list(width_ms = width_ms, step_ms = step_ms,
                 start_ms = start_ms, end_ms = end_ms,
                 align = align, min_samples = min_samples),
            class = "eye_process_window_spec")
}

#' Print a process window spec object
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
print.eye_process_window_spec <- function(x, ...) {
  cat("<eye_process_window_spec>\n")
  cat(" align:", x$align, "\n")
  cat(" range:", x$start_ms, "to", x$end_ms, "ms\n")
  cat(" width:", x$width_ms, "ms; step:", x$step_ms, "ms\n")
  invisible(x)
}

.ep08_auc <- function(y, time) {
  y <- .ep08_num(y); time <- .ep08_num(time)
  ok <- is.finite(y) & is.finite(time)
  y <- y[ok]; time <- time[ok]
  if (length(y) < 2L) return(NA_real_)
  o <- order(time); y <- y[o]; time <- time[o]
  sum(diff(time) * (head(y, -1L) + tail(y, -1L)) / 2, na.rm = TRUE)
}

.ep08_slope <- function(y, time) {
  y <- .ep08_num(y); time <- .ep08_num(time)
  ok <- is.finite(y) & is.finite(time)
  if (sum(ok) < 3L || .ep08_sd(time[ok]) == 0) return(NA_real_)
  unname(stats::coef(stats::lm(y[ok] ~ time[ok]))[2L])
}

.ep08_rmssd <- function(x) {
  x <- .ep08_num(x); x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  sqrt(mean(diff(x)^2, na.rm = TRUE))
}

.ep08_entropy_numeric <- function(x, bins = 8L) {
  x <- .ep08_num(x); x <- x[is.finite(x)]
  if (length(x) < 2L || length(unique(x)) < 2L) return(0)
  br <- unique(stats::quantile(x, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE))
  if (length(br) < 3L) return(0)
  h <- table(cut(x, breaks = br, include.lowest = TRUE))
  p <- as.numeric(h) / sum(h)
  p <- p[p > 0]
  -sum(p * log(p))
}

.ep08_entropy_factor <- function(x) {
  x <- as.character(x); x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(NA_real_)
  p <- as.numeric(table(x)); p <- p / sum(p)
  -sum(p * log(p))
}

.ep08_switch_count <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) < 2L) return(0L)
  sum(x[-1L] != x[-length(x)])
}

.ep08_path_length <- function(x, y) {
  x <- .ep08_num(x); y <- .ep08_num(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2L) return(NA_real_)
  sum(sqrt(diff(x)^2 + diff(y)^2), na.rm = TRUE)
}

.ep08_velocity_mean <- function(x, y, time) {
  x <- .ep08_num(x); y <- .ep08_num(y); time <- .ep08_num(time)
  ok <- is.finite(x) & is.finite(y) & is.finite(time)
  x <- x[ok]; y <- y[ok]; time <- time[ok]
  if (length(x) < 2L) return(NA_real_)
  dt <- diff(time) / 1000
  good <- is.finite(dt) & dt > 0
  if (!any(good)) return(NA_real_)
  v <- sqrt(diff(x)^2 + diff(y)^2)[good] / dt[good]
  .ep08_mean(v)
}

.ep08_window_starts <- function(spec) {
  seq(spec$start_ms, spec$end_ms - spec$width_ms, by = spec$step_ms)
}

#' Extract standardized sliding-window process features
#'
#' @param data Sample-level eye-tracking/pupil data.
#' @param person,trial,time Identifier/time columns.
#' @param spec Window specification.
#' @param align_time Optional column containing the alignment event time in the
#'   same units as `time`. Required for response/custom alignment unless `time`
#'   is already relative to the desired origin.
#' @param pupil Optional pupil signal column.
#' @param pupil_tonic,pupil_phasic Optional decomposed pupil columns.
#' @param gaze_x,gaze_y Optional gaze coordinates.
#' @param aoi Optional AOI-state column.
#' @param valid_gaze,valid_pupil Optional validity columns/indicators.
#' @param blink,trackloss Optional blink/trackloss indicators.
#' @return An `eye_process_windows` object.
#' @export
extract_process_windows <- function(
    data, person = "person_id", trial = "trial_id", time = "time_ms",
    spec = process_window_spec(), align_time = NULL,
    pupil = "pupil_bc", pupil_tonic = "pupil_tonic", pupil_phasic = "pupil_phasic",
    gaze_x = "x", gaze_y = "y", aoi = "aoi",
    valid_gaze = "valid_gaze_prop", valid_pupil = "valid_pupil_prop",
    blink = "blink", trackloss = "trackloss") {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(person, trial, time))
  if (!inherits(spec, "eye_process_window_spec")) stop("spec must be process_window_spec().", call. = FALSE)
  d <- data
  t <- .ep08_num(d[[time]])
  if (!is.null(align_time)) {
    .ep08_req_cols(d, align_time)
    t <- t - .ep08_num(d[[align_time]])
  }
  d$.ep08_relative_time <- t
  group_cols <- c(person, trial)
  global_aois <- if (aoi %in% names(d)) {
    sort(unique(as.character(d[[aoi]])[!is.na(d[[aoi]]) & nzchar(as.character(d[[aoi]]))]))
  } else character()
  groups <- .ep08_split_rows(d, group_cols)
  starts <- .ep08_window_starts(spec)
  out <- list(); k <- 0L

  for (g in groups) {
    gd <- d[g, , drop = FALSE]
    for (s in starts) {
      e <- s + spec$width_ms
      idx <- which(is.finite(gd$.ep08_relative_time) & gd$.ep08_relative_time >= s & gd$.ep08_relative_time < e)
      if (length(idx) < spec$min_samples) next
      z <- gd[idx, , drop = FALSE]
      zt <- z$.ep08_relative_time
      pv <- if (pupil %in% names(z)) .ep08_num(z[[pupil]]) else rep(NA_real_, nrow(z))
      pt <- if (pupil_tonic %in% names(z)) .ep08_num(z[[pupil_tonic]]) else rep(NA_real_, nrow(z))
      pp <- if (pupil_phasic %in% names(z)) .ep08_num(z[[pupil_phasic]]) else rep(NA_real_, nrow(z))
      gx <- if (gaze_x %in% names(z)) .ep08_num(z[[gaze_x]]) else rep(NA_real_, nrow(z))
      gy <- if (gaze_y %in% names(z)) .ep08_num(z[[gaze_y]]) else rep(NA_real_, nrow(z))
      av <- if (aoi %in% names(z)) as.character(z[[aoi]]) else rep(NA_character_, nrow(z))
      gv <- if (valid_gaze %in% names(z)) .ep08_num(z[[valid_gaze]]) else rep(NA_real_, nrow(z))
      qv <- if (valid_pupil %in% names(z)) .ep08_num(z[[valid_pupil]]) else rep(NA_real_, nrow(z))
      bl <- if (blink %in% names(z)) as.numeric(.ep08_bool(z[[blink]])) else rep(NA_real_, nrow(z))
      tl <- if (trackloss %in% names(z)) as.numeric(.ep08_bool(z[[trackloss]])) else rep(NA_real_, nrow(z))
      ac <- if (sum(is.finite(pv)) >= 3L) {
        tryCatch(suppressWarnings(stats::cor(head(pv, -1L), tail(pv, -1L), use = "complete.obs")),
                 error = function(e) NA_real_)
      } else NA_real_

      base <- .ep08_group_values(gd, seq_len(nrow(gd)), group_cols)
      row <- cbind(base, data.frame(
        window_start = s,
        window_end = e,
        window_mid = s + spec$width_ms / 2,
        n_samples_window = nrow(z),
        valid_gaze_prop = if (any(is.finite(gv))) .ep08_mean(gv) else NA_real_,
        valid_pupil_prop = if (any(is.finite(qv))) .ep08_mean(qv) else mean(is.finite(pv)),
        blink_prop = .ep08_mean(bl),
        trackloss_prop = .ep08_mean(tl),
        pupil_mean = .ep08_mean(pv),
        pupil_sd = .ep08_sd(pv),
        pupil_slope = .ep08_slope(pv, zt),
        pupil_auc = .ep08_auc(pv, zt),
        pupil_rmssd = .ep08_rmssd(pv),
        pupil_entropy = .ep08_entropy_numeric(pv),
        pupil_peak = if (any(is.finite(pv))) max(pv, na.rm = TRUE) else NA_real_,
        pupil_autocorr_lag1 = ac,
        tonic_mean = .ep08_mean(pt),
        phasic_mean = .ep08_mean(pp),
        phasic_sd = .ep08_sd(pp),
        phasic_auc = .ep08_auc(pp, zt),
        gaze_velocity_mean = .ep08_velocity_mean(gx, gy, zt),
        gaze_x_sd = .ep08_sd(gx),
        gaze_y_sd = .ep08_sd(gy),
        gaze_path_length = .ep08_path_length(gx, gy),
        aoi_entropy = .ep08_entropy_factor(av),
        aoi_switch_count = .ep08_switch_count(av),
        stringsAsFactors = FALSE
      ))
      if (length(global_aois)) {
        for (lv in global_aois) {
          row[[paste0("aoi_prop__", make.names(lv))]] <- if (any(!is.na(av))) mean(av == lv, na.rm = TRUE) else NA_real_
        }
      }
      k <- k + 1L; out[[k]] <- row
    }
  }
  tab <- if (length(out)) do.call(rbind, out) else data.frame()
  rownames(tab) <- NULL
  structure(list(data = tab, spec = spec, person = person, trial = trial,
                 time = time, align_time = align_time,
                 source_n = nrow(data), status = "windowed_process_representation"),
            class = "eye_process_windows")
}

#' Summarize extracted process windows
#' @param x `eye_process_windows` object.
#' @param by Optional grouping columns present in the extracted table.
#' @export
summarize_process_windows <- function(x, by = NULL) {
  if (!inherits(x, "eye_process_windows")) stop("x must be eye_process_windows.", call. = FALSE)
  d <- x$data
  if (!nrow(d)) return(d)
  numeric_cols <- names(d)[vapply(d, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("window_start", "window_end", "window_mid"))
  if (is.null(by) || !length(by)) {
    return(data.frame(metric = numeric_cols,
                      mean = vapply(numeric_cols, function(m) .ep08_mean(d[[m]]), numeric(1)),
                      sd = vapply(numeric_cols, function(m) .ep08_sd(d[[m]]), numeric(1)),
                      stringsAsFactors = FALSE))
  }
  .ep08_req_cols(d, by)
  stats::aggregate(d[numeric_cols], by = d[by], FUN = .ep08_mean)
}

#' Bind compatible process-window objects
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
bind_process_windows <- function(...) {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !inherits(xs[[1L]], "eye_process_windows")) xs <- xs[[1L]]
  if (!length(xs) || !all(vapply(xs, inherits, logical(1), "eye_process_windows")))
    stop("All inputs must be eye_process_windows objects.", call. = FALSE)
  tab <- .ep08_rbind_fill(lapply(xs, function(z) z$data))
  structure(list(data = tab, spec = lapply(xs, function(z) z$spec), source_n = sum(vapply(xs, function(z) z$source_n, numeric(1))),
                 status = "bound_process_windows"), class = "eye_process_windows")
}

#' Validate a process-window representation
#' @export
#' @param x Object to process, inspect, compare, or plot.
validate_process_windows <- function(x) {
  if (!inherits(x, "eye_process_windows")) stop("x must be eye_process_windows.", call. = FALSE)
  d <- x$data
  required <- c("window_start", "window_end", "window_mid", "n_samples_window")
  missing <- setdiff(required, names(d))
  issues <- character()
  if (length(missing)) issues <- c(issues, paste("missing columns:", paste(missing, collapse = ", ")))
  if (nrow(d) && any(d$window_end <= d$window_start, na.rm = TRUE)) issues <- c(issues, "non-positive window width")
  if (nrow(d) && any(d$n_samples_window < 1, na.rm = TRUE)) issues <- c(issues, "empty windows")
  data.frame(valid = length(issues) == 0L,
             issue = if (length(issues)) paste(issues, collapse = "; ") else "none",
             n_windows = nrow(d), stringsAsFactors = FALSE)
}

#' Audit sensitivity of process summaries to temporal window choices
#'
#' @param data Sample-level data.
#' @param widths_ms Window widths.
#' @param steps_ms Step widths; recycled or crossed depending on `grid`.
#' @param metric Extracted process metric to compare.
#' @param grid If TRUE, evaluate all width-step combinations.
#' @param ... Passed to `extract_process_windows()`.
#' @export
audit_process_window_sensitivity <- function(
    data, widths_ms = c(250, 500, 1000, 1500), steps_ms = c(100, 250, 500),
    metric = "pupil_mean", grid = TRUE, ...) {
  settings <- if (isTRUE(grid)) {
    expand.grid(width_ms = widths_ms, step_ms = steps_ms, KEEP.OUT.ATTRS = FALSE)
  } else {
    n <- max(length(widths_ms), length(steps_ms))
    data.frame(width_ms = rep(widths_ms, length.out = n), step_ms = rep(steps_ms, length.out = n))
  }
  rows <- lapply(seq_len(nrow(settings)), function(i) {
    s <- settings[i, ]
    args <- list(...)
    start_ms <- if (!is.null(args$spec)) args$spec$start_ms else 0
    end_ms <- if (!is.null(args$spec)) args$spec$end_ms else 3000
    args$spec <- process_window_spec(s$width_ms, s$step_ms, start_ms = start_ms, end_ms = end_ms)
    x <- do.call(extract_process_windows, c(list(data = data), args))
    d <- x$data
    val <- if (metric %in% names(d)) .ep08_mean(d[[metric]]) else NA_real_
    sdv <- if (metric %in% names(d)) .ep08_sd(d[[metric]]) else NA_real_
    data.frame(width_ms = s$width_ms, step_ms = s$step_ms,
               metric = metric, mean_value = val, sd_value = sdv,
               n_windows = nrow(d), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  structure(list(table = tab, metric = metric, settings = settings,
                 caveat = "Window sensitivity assesses representation robustness; it does not select a causal or psychologically privileged window."),
            class = "eye_process_window_sensitivity")
}

.ep08_poly_features <- function(time, y, degree = 3L) {
  time <- .ep08_num(time); y <- .ep08_num(y)
  ok <- is.finite(time) & is.finite(y)
  time <- time[ok]; y <- y[ok]
  if (length(y) < degree + 2L || length(unique(time)) < degree + 1L)
    return(rep(NA_real_, degree))
  tt <- as.numeric(scale(time))
  if (all(!is.finite(tt)) || .ep08_sd(tt) == 0) return(rep(NA_real_, degree))
  P <- stats::poly(tt, degree = degree, raw = FALSE)
  dat <- data.frame(y = y, P)
  names(dat)[-1L] <- paste0("p", seq_len(degree))
  fit <- stats::lm(stats::reformulate(names(dat)[-1L], response = "y"), data = dat)
  unname(stats::coef(fit)[-1L])
}

#' Extract AOI growth-curve/trajectory features
#'
#' Converts AOI occupancy over binned time into orthogonal-polynomial trajectory
#' coefficients. Coefficients summarize temporal shape and are not latent
#' psychological traits by themselves.
#'
#' @param data Sample-level data.
#' @param person,trial,time,aoi Column names.
#' @param bin_ms Temporal bin width.
#' @param degree Polynomial degree.
#' @param aois Optional AOIs to encode; defaults to observed AOIs.
#' @export
aoi_trajectory_features <- function(data, person = "person_id", trial = "trial_id",
                                    time = "time_ms", aoi = "aoi", bin_ms = 100,
                                    degree = 3L, aois = NULL) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(person, trial, time, aoi))
  if (!nrow(data)) stop("data must contain at least one row.", call. = FALSE)
  degree <- as.integer(degree)
  if (degree < 1L || degree > 6L) stop("degree must be between 1 and 6.", call. = FALSE)
  if (!is.finite(bin_ms) || bin_ms <= 0) stop("bin_ms must be positive.", call. = FALSE)
  d <- data[, c(person, trial, time, aoi), drop = FALSE]
  d$.time <- .ep08_num(d[[time]])
  d$.aoi <- as.character(d[[aoi]])
  d$.bin <- floor(d$.time / bin_ms) * bin_ms
  if (is.null(aois)) aois <- sort(unique(d$.aoi[!is.na(d$.aoi) & nzchar(d$.aoi)]))
  if (!length(aois)) stop("No AOI levels were available.", call. = FALSE)
  groups <- .ep08_split_rows(d, c(person, trial))
  rows <- lapply(groups, function(idx) {
    z <- d[idx, , drop = FALSE]
    bins <- sort(unique(z$.bin[is.finite(z$.bin)]))
    base <- .ep08_group_values(d, idx, c(person, trial))
    out <- base
    for (lv in aois) {
      prop <- vapply(bins, function(b) {
        q <- z$.aoi[z$.bin == b]
        if (!length(q)) NA_real_ else mean(q == lv, na.rm = TRUE)
      }, numeric(1))
      cf <- .ep08_poly_features(bins, prop, degree = degree)
      for (j in seq_len(degree)) out[[paste0(make.names(lv), "_gca_degree", j)]] <- cf[j]
    }
    out
  })
  tab <- do.call(rbind, rows); rownames(tab) <- NULL
  structure(list(features = tab, aois = aois, degree = degree, bin_ms = bin_ms,
                 person = person, trial = trial,
                 caveat = "AOI trajectory coefficients summarize time-course shape and should not be interpreted causally without a design supporting that claim."),
            class = "eye_aoi_trajectory")
}

#' Fit a single AOI growth curve
#' @param data Data frame.
#' @param time Time column.
#' @param outcome Numeric AOI proportion/indicator column.
#' @param degree Polynomial degree.
#' @export
fit_aoi_growth_curve <- function(data, time, outcome, degree = 3L) {
  data <- .ep08_as_df(data)
  degree <- as.integer(degree)
  if (!is.finite(degree) || degree < 1L || degree > 6L) stop("degree must be between 1 and 6.", call. = FALSE)
  .ep08_req_cols(data, c(time, outcome))
  tt <- .ep08_num(data[[time]]); yy <- .ep08_num(data[[outcome]])
  ok <- is.finite(tt) & is.finite(yy)
  tt <- tt[ok]; yy <- yy[ok]
  if (length(yy) < degree + 2L) stop("Insufficient observations for requested degree.", call. = FALSE)
  P <- stats::poly(tt, degree = degree, raw = FALSE)
  df <- data.frame(y = yy, time = tt, P)
  names(df)[-(1:2)] <- paste0("p", seq_len(degree))
  fit <- stats::lm(stats::reformulate(names(df)[-(1:2)], response = "y"), data = df)
  structure(list(model = fit, time = tt, outcome = yy, degree = degree,
                 poly = P, range = range(tt), status = "trajectory_shape_model"),
            class = "eye_aoi_growth_curve")
}

#' Predict from an AOI growth curve
#' @export
#' @param object Object supplied to the S3 method.
#' @param time Time values or name of the time variable.
predict_aoi_trajectory <- function(object, time = NULL) {
  if (!inherits(object, "eye_aoi_growth_curve")) stop("object must be eye_aoi_growth_curve.", call. = FALSE)
  if (is.null(time)) time <- seq(object$range[1L], object$range[2L], length.out = 101L)
  time <- .ep08_num(time)
  P <- stats::predict(object$poly, newdata = time)
  df <- as.data.frame(P); names(df) <- paste0("p", seq_len(object$degree))
  data.frame(time = time, predicted = as.numeric(stats::predict(object$model, newdata = df)))
}

#' Compare AOI trajectory feature objects
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
compare_aoi_trajectories <- function(...) {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !inherits(xs[[1L]], "eye_aoi_trajectory")) xs <- xs[[1L]]
  if (!length(xs)) stop("Supply at least one eye_aoi_trajectory object.", call. = FALSE)
  if (!all(vapply(xs, inherits, logical(1), "eye_aoi_trajectory")))
    stop("All inputs must be eye_aoi_trajectory objects.", call. = FALSE)
  data.frame(
    model = seq_along(xs),
    n_rows = vapply(xs, function(x) nrow(x$features), integer(1)),
    n_aois = vapply(xs, function(x) length(x$aois), integer(1)),
    degree = vapply(xs, function(z) z$degree, integer(1)),
    bin_ms = vapply(xs, function(z) z$bin_ms, numeric(1))
  )
}
