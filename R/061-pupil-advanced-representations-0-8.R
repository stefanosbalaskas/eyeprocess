# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Advanced pupillometry representations: frequency/activity features, event
# deconvolution, luminance/fatigue confound adjustment, and robust filtering.

.ep08_interp_signal <- function(y) {
  y <- .ep08_num(y)
  idx <- seq_along(y)
  ok <- is.finite(y)
  if (sum(ok) < 2L) return(rep(NA_real_, length(y)))
  stats::approx(idx[ok], y[ok], xout = idx, rule = 2)$y
}

#' Compute pupil signal power in a frequency band
#'
#' @param y Pupil signal.
#' @param sampling_rate_hz Sampling rate in Hz.
#' @param lower_hz,upper_hz Frequency-band limits.
#' @param detrend Remove the mean before FFT.
#' @export
pupil_band_power <- function(y, sampling_rate_hz, lower_hz, upper_hz, detrend = TRUE) {
  y <- .ep08_interp_signal(y)
  if (all(!is.finite(y)) || length(y) < 8L) return(NA_real_)
  if (!is.finite(sampling_rate_hz) || sampling_rate_hz <= 0) stop("sampling_rate_hz must be positive.", call. = FALSE)
  if (!is.finite(lower_hz) || !is.finite(upper_hz) || lower_hz < 0 || lower_hz >= upper_hz)
    stop("Require 0 <= lower_hz < upper_hz.", call. = FALSE)
  if (isTRUE(detrend)) y <- y - mean(y, na.rm = TRUE)
  if (.ep08_sd(y) == 0) return(0)
  n <- length(y)
  f <- stats::fft(y)
  power <- Mod(f)^2 / n
  freq <- (0:(n - 1L)) * sampling_rate_hz / n
  keep <- freq >= lower_hz & freq <= upper_hz & freq <= sampling_rate_hz / 2
  if (!any(keep)) return(NA_real_)
  sum(power[keep], na.rm = TRUE)
}

#' Derivative-based pupil activity magnitude
#' @param y Pupil signal.
#' @param time_ms Time in milliseconds.
#' @export
pupil_velocity_activity <- function(y, time_ms) {
  y <- .ep08_interp_signal(y); t <- .ep08_num(time_ms) / 1000
  ok <- is.finite(y) & is.finite(t)
  y <- y[ok]; t <- t[ok]
  if (length(y) < 4L) return(NA_real_)
  dt <- diff(t); dy <- diff(y)
  good <- is.finite(dt) & dt > 0 & is.finite(dy)
  if (!any(good)) return(NA_real_)
  sqrt(mean((dy[good] / dt[good])^2, na.rm = TRUE))
}

.ep08_roll_mean <- function(x, width) {
  x <- .ep08_interp_signal(x)
  width <- as.integer(width)
  width <- max(3L, width)
  if (width %% 2L == 0L) width <- width + 1L
  stats::filter(x, rep(1 / width, width), sides = 2)
}

.ep08_width_from_ms <- function(window_ms, time_ms) {
  t <- sort(unique(.ep08_num(time_ms)))
  step <- stats::median(diff(t), na.rm = TRUE)
  if (!is.finite(step) || step <= 0) return(5L)
  w <- max(3L, as.integer(round(window_ms / step)))
  if (w %% 2L == 0L) w <- w + 1L
  w
}

.ep08_derivative_power <- function(y, time_ms, smooth_window_ms) {
  y <- .ep08_interp_signal(y); t <- .ep08_num(time_ms) / 1000
  if (length(y) < 6L) return(NA_real_)
  w <- .ep08_width_from_ms(smooth_window_ms, time_ms)
  sm <- as.numeric(.ep08_roll_mean(y, w))
  ok <- is.finite(sm) & is.finite(t)
  sm <- sm[ok]; t <- t[ok]
  if (length(sm) < 6L) return(NA_real_)
  dt <- diff(t); dy <- diff(sm)
  good <- is.finite(dt) & dt > 0 & is.finite(dy)
  if (!any(good)) return(NA_real_)
  mean((dy[good] / dt[good])^2, na.rm = TRUE)
}

#' Compute a transparent pupil activity index
#'
#' @param y Pupil signal.
#' @param time_ms Time vector.
#' @param sampling_rate_hz Sampling rate for frequency methods.
#' @param method `velocity`, `frequency_contrast`, or `ripa_proxy`.
#' @param low_band,high_band Frequency bands for frequency contrast.
#' @param fast_window_ms,slow_window_ms Smoothing windows for the RIPA-style proxy.
#' @export
pupil_activity_index <- function(
    y, time_ms = seq_along(y), sampling_rate_hz = NULL,
    method = c("velocity", "frequency_contrast", "ripa_proxy"),
    low_band = c(0.05, 0.50), high_band = c(0.50, 4.00),
    fast_window_ms = 250, slow_window_ms = 750) {
  method <- match.arg(method)
  if (method == "velocity") return(pupil_velocity_activity(y, time_ms))
  if (method == "frequency_contrast") {
    if (is.null(sampling_rate_hz)) stop("sampling_rate_hz is required for frequency_contrast.", call. = FALSE)
    lo <- pupil_band_power(y, sampling_rate_hz, low_band[1L], low_band[2L])
    hi <- pupil_band_power(y, sampling_rate_hz, high_band[1L], high_band[2L])
    if (!is.finite(lo) || !is.finite(hi)) return(NA_real_)
    return(log1p(hi) - log1p(lo))
  }
  fast <- .ep08_derivative_power(y, time_ms, fast_window_ms)
  slow <- .ep08_derivative_power(y, time_ms, slow_window_ms)
  if (!is.finite(fast) || !is.finite(slow)) return(NA_real_)
  log1p(fast) - log1p(slow)
}

#' Extract pupil frequency-domain and activity features by group
#'
#' @param data Sample-level data.
#' @param by Grouping columns, e.g. person and trial/window.
#' @param time,pupil Column names.
#' @param sampling_rate_hz Either a scalar or a column name.
#' @param low_band,high_band Frequency bands.
#' @export
pupil_frequency_features <- function(
    data, by = c("person_id", "trial_id"), time = "time_ms", pupil = "pupil_bc",
    sampling_rate_hz = 60, low_band = c(0.05, 0.50), high_band = c(0.50, 4.00)) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(by, time, pupil))
  groups <- .ep08_split_rows(data, by)
  rows <- lapply(groups, function(idx) {
    z <- data[idx, , drop = FALSE]
    sr <- if (is.character(sampling_rate_hz) && length(sampling_rate_hz) == 1L) {
      .ep08_req_cols(z, sampling_rate_hz); .ep08_mean(z[[sampling_rate_hz]])
    } else as.numeric(sampling_rate_hz)[1L]
    y <- z[[pupil]]; tt <- z[[time]]
    lo <- pupil_band_power(y, sr, low_band[1L], low_band[2L])
    hi <- pupil_band_power(y, sr, high_band[1L], high_band[2L])
    cbind(.ep08_group_values(data, idx, by), data.frame(
      sampling_rate_hz = sr,
      pupil_low_frequency_power = lo,
      pupil_high_frequency_power = hi,
      pupil_frequency_contrast = if (is.finite(lo) && is.finite(hi)) log1p(hi) - log1p(lo) else NA_real_,
      pupil_velocity_activity = pupil_velocity_activity(y, tt),
      pupil_ripa_proxy = pupil_activity_index(y, tt, sampling_rate_hz = sr, method = "ripa_proxy"),
      stringsAsFactors = FALSE
    ))
  })
  tab <- do.call(rbind, rows); rownames(tab) <- NULL
  structure(list(
    features = tab, low_band = low_band, high_band = high_band,
    by = by, pupil = pupil, time = time,
    caveat = paste(
      "Frequency/activity features are signal representations, not pure cognitive-load measures.",
      "Short windows, luminance, gaze position, blink handling, and filtering can materially affect them."
    )
  ), class = "eye_pupil_frequency_features")
}

#' Audit stability of pupil frequency features across window lengths
#' @param data Sample-level data.
#' @param windows_ms Window lengths to evaluate.
#' @param by,time,pupil,sampling_rate_hz Passed through to feature construction.
#' @export
audit_pupil_frequency_stability <- function(
    data, windows_ms = c(500, 1000, 2000), by = c("person_id", "trial_id"),
    time = "time_ms", pupil = "pupil_bc", sampling_rate_hz = 60) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(by, time, pupil))
  groups <- .ep08_split_rows(data, by)
  rows <- list(); k <- 0L
  for (idx in groups) {
    z <- data[idx, , drop = FALSE]
    tz <- .ep08_num(z[[time]])
    if (!any(is.finite(tz))) next
    t0 <- min(tz[is.finite(tz)])
    for (w in windows_ms) {
      zz <- z[.ep08_num(z[[time]]) >= t0 & .ep08_num(z[[time]]) < t0 + w, , drop = FALSE]
      if (nrow(zz) < 8L) next
      f <- pupil_frequency_features(zz, by = by, time = time, pupil = pupil, sampling_rate_hz = sampling_rate_hz)$features
      if (!nrow(f)) next
      f$window_ms <- w
      k <- k + 1L; rows[[k]] <- f
    }
  }
  tab <- if (length(rows)) do.call(rbind, rows) else data.frame()
  structure(list(table = tab, windows_ms = windows_ms,
                 caveat = "Large feature changes across window choices indicate representation instability."),
            class = "eye_pupil_frequency_stability")
}

#' Canonical gamma-shaped pupil response kernel
#'
#' @param time_since_event_ms Time relative to event onset.
#' @param tmax_ms Approximate response peak time.
#' @param shape Shape parameter.
#' @param normalize Normalize peak to one.
#' @export
pupil_response_kernel <- function(time_since_event_ms, tmax_ms = 930, shape = 10.1, normalize = TRUE) {
  t <- pmax(.ep08_num(time_since_event_ms), 0) / 1000
  tmax <- as.numeric(tmax_ms) / 1000
  if (!is.finite(tmax) || tmax <= 0 || !is.finite(shape) || shape <= 0)
    stop("tmax_ms and shape must be positive.", call. = FALSE)
  out <- (t / tmax)^shape * exp(-shape * ((t / tmax) - 1))
  out[.ep08_num(time_since_event_ms) < 0] <- 0
  out[!is.finite(out)] <- 0
  if (isTRUE(normalize) && max(out, na.rm = TRUE) > 0) out <- out / max(out, na.rm = TRUE)
  out
}

#' Build an event-locked pupil regressor
#' @param time_ms Sample times.
#' @param event_time_ms Event onset.
#' @param tmax_ms,shape Kernel parameters.
#' @export
pupil_event_regressor <- function(time_ms, event_time_ms, tmax_ms = 930, shape = 10.1) {
  pupil_response_kernel(.ep08_num(time_ms) - as.numeric(event_time_ms), tmax_ms = tmax_ms, shape = shape)
}

.ep08_resolve_event_time <- function(z, spec) {
  if (is.character(spec) && length(spec) == 1L) {
    .ep08_req_cols(z, spec)
    v <- .ep08_num(z[[spec]])
    return(.ep08_first_finite(v))
  }
  if (is.numeric(spec) && length(spec) == 1L) return(as.numeric(spec))
  stop("Each event specification must be a scalar numeric time or a column name.", call. = FALSE)
}

#' Fit transparent event-related pupil deconvolution models
#'
#' Fits a linear superposition model of overlapping event kernels. This is a
#' transparent reference deconvolution layer, not a universal physiological model.
#'
#' @param data Sample-level pupil data.
#' @param by Grouping columns, typically person and trial.
#' @param time,pupil Column names.
#' @param events Named list mapping event labels to scalar event times or columns.
#' @param tmax_ms,shape Kernel parameters.
#' @param min_samples Minimum usable samples per group.
#' @export
fit_pupil_event_deconvolution <- function(
    data, by = c("person_id", "trial_id"), time = "time_ms", pupil = "pupil_bc",
    events = list(stimulus = 0), tmax_ms = 930, shape = 10.1, min_samples = 20L) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(by, time, pupil))
  if (!length(events) || is.null(names(events)) || any(!nzchar(names(events))))
    stop("events must be a named list.", call. = FALSE)
  groups <- .ep08_split_rows(data, by)
  fits <- list(); effects <- list(); fitted_rows <- list(); k <- 0L
  for (idx in groups) {
    z <- data[idx, , drop = FALSE]
    tt <- .ep08_num(z[[time]]); yy <- .ep08_num(z[[pupil]])
    X <- list()
    event_times <- numeric(length(events)); names(event_times) <- names(events)
    valid_events <- TRUE
    for (nm in names(events)) {
      et <- .ep08_resolve_event_time(z, events[[nm]])
      event_times[nm] <- et
      if (!is.finite(et)) { valid_events <- FALSE; break }
      X[[paste0("event__", make.names(nm))]] <- pupil_event_regressor(tt, et, tmax_ms, shape)
    }
    if (!valid_events) next
    df <- data.frame(pupil = yy, time = tt, X, check.names = FALSE)
    ok <- stats::complete.cases(df)
    df <- df[ok, , drop = FALSE]
    if (nrow(df) < min_samples || .ep08_sd(df$pupil) == 0) next
    predictors <- names(df)[grepl("^event__", names(df))]
    fit <- stats::lm(stats::reformulate(predictors, response = "pupil"), data = df)
    base <- .ep08_group_values(data, idx, by)
    co <- stats::coef(fit)
    er <- base
    for (nm in names(events)) {
      cn <- paste0("event__", make.names(nm))
      er[[paste0("beta__", make.names(nm))]] <- unname(co[cn])
      er[[paste0("event_time__", make.names(nm))]] <- event_times[nm]
    }
    er$residual_sd <- stats::sd(stats::residuals(fit), na.rm = TRUE)
    er$r_squared <- summary(fit)$r.squared
    k <- k + 1L
    fits[[k]] <- fit; effects[[k]] <- er
    fr <- cbind(base[rep(1L, nrow(df)), , drop = FALSE],
                data.frame(time = df$time, observed = df$pupil,
                           fitted = stats::fitted(fit), residual = stats::residuals(fit)))
    fitted_rows[[k]] <- fr
  }
  effect_tab <- if (length(effects)) do.call(rbind, effects) else data.frame()
  fitted_tab <- if (length(fitted_rows)) do.call(rbind, fitted_rows) else data.frame()
  structure(list(
    fits = fits, effects = effect_tab, fitted = fitted_tab, events = events,
    tmax_ms = tmax_ms, shape = shape, by = by,
    status = "transparent_linear_kernel_deconvolution",
    caveat = paste(
      "Event coefficients depend on the selected response kernel and event timing.",
      "Use kernel/timing sensitivity analyses before substantive physiological interpretation."
    )
  ), class = "eye_pupil_deconvolution")
}

#' Extract event effects from pupil deconvolution
#' @export
#' @param x Object to process, inspect, compare, or plot.
pupil_event_effects <- function(x) {
  if (!inherits(x, "eye_pupil_deconvolution")) stop("x must be eye_pupil_deconvolution.", call. = FALSE)
  x$effects
}

#' Compare pupil deconvolution kernels
#' @param data Same input used for fitting.
#' @param tmax_values Candidate peak times.
#' @param ... Passed to `fit_pupil_event_deconvolution()`.
#' @export
compare_pupil_kernels <- function(data, tmax_values = c(512, 930), ...) {
  rows <- lapply(tmax_values, function(tm) {
    fit <- fit_pupil_event_deconvolution(data, tmax_ms = tm, ...)
    e <- fit$effects
    data.frame(tmax_ms = tm,
               mean_r_squared = if (nrow(e)) .ep08_mean(e$r_squared) else NA_real_,
               mean_residual_sd = if (nrow(e)) .ep08_mean(e$residual_sd) else NA_real_,
               n_groups = nrow(e), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Fit a luminance/fatigue/process confound model for pupil response
#'
#' @param data Trial-level data.
#' @param pupil,luminance,trial_order Column names.
#' @param theta Optional latent-score column.
#' @param person,item Optional identifiers.
#' @param engine `auto`, `mgcv`, or `lm`.
#' @return An `eye_pupil_confound_model` object containing raw and adjusted values.
#' @export
fit_pupil_confound_model <- function(
    data, pupil = "pupil_peak", luminance = "screen_luminance",
    trial_order = "trial_sequence", theta = NULL,
    person = "person_id", item = "item_id",
    engine = c("auto", "mgcv", "lm")) {
  engine <- match.arg(engine)
  data <- .ep08_as_df(data)
  req <- c(pupil, luminance, trial_order)
  if (!is.null(theta)) req <- c(req, theta)
  .ep08_req_cols(data, req)
  d <- data
  d$.pupil <- .ep08_num(d[[pupil]])
  d$.luminance <- .ep08_num(d[[luminance]])
  d$.trial <- .ep08_num(d[[trial_order]])
  d$.theta <- if (!is.null(theta)) .ep08_num(d[[theta]]) else 0
  d$.person <- if (person %in% names(d)) factor(d[[person]]) else factor(rep("all", nrow(d)))
  d$.item <- if (item %in% names(d)) factor(d[[item]]) else factor(rep("all", nrow(d)))
  ok <- stats::complete.cases(d[, c(".pupil", ".luminance", ".trial", ".theta")])
  fitd <- d[ok, , drop = FALSE]
  if (nrow(fitd) < 30L) stop("At least 30 complete observations are required.", call. = FALSE)
  n_lum <- length(unique(fitd$.luminance))
  n_trial <- length(unique(fitd$.trial))
  if (n_lum < 2L || n_trial < 2L) {
    stop("Luminance and trial-order predictors must each contain at least two unique values.", call. = FALSE)
  }

  chosen <- engine
  mgcv_eligible <- requireNamespace("mgcv", quietly = TRUE) && nrow(fitd) >= 50L &&
    length(unique(fitd$.luminance)) >= 4L && length(unique(fitd$.trial)) >= 4L
  if (engine == "auto") chosen <- if (mgcv_eligible) "mgcv" else "lm"
  fit <- NULL
  if (chosen == "mgcv") {
    if (!requireNamespace("mgcv", quietly = TRUE)) stop("Package `mgcv` is required for engine='mgcv'.", call. = FALSE)
    if (length(unique(fitd$.luminance)) < 4L || length(unique(fitd$.trial)) < 4L)
      stop("engine='mgcv' requires at least four unique luminance and trial-order values.", call. = FALSE)
    k_lum <- max(3L, min(6L, length(unique(fitd$.luminance)) - 1L))
    k_trial <- max(3L, min(6L, length(unique(fitd$.trial)) - 1L))
    k_theta <- max(3L, min(6L, length(unique(fitd$.theta)) - 1L))
    rhs <- c(paste0("s(.luminance,k=", k_lum, ")"), paste0("s(.trial,k=", k_trial, ")"))
    if (.ep08_sd(fitd$.theta) > 0) rhs <- c(rhs, paste0("s(.theta,k=", k_theta, ")"),
                                           paste0("ti(.luminance,.theta,k=c(", k_lum, ",", k_theta, "))"))
    if (nlevels(fitd$.person) > 1L) rhs <- c(rhs, "s(.person,bs='re')")
    if (nlevels(fitd$.item) > 1L) rhs <- c(rhs, "s(.item,bs='re')")
    form <- stats::as.formula(paste(".pupil ~", paste(rhs, collapse = " + ")))
    fit <- mgcv::gam(form, data = fitd, method = "REML")
  } else {
    lum_degree <- min(2L, n_lum - 1L)
    trial_degree <- min(2L, n_trial - 1L)
    rhs <- c(
      paste0("poly(.luminance,", lum_degree, ",raw=TRUE)"),
      paste0("poly(.trial,", trial_degree, ",raw=TRUE)")
    )
    if (.ep08_sd(fitd$.theta) > 0) rhs <- c(rhs, ".theta", ".luminance:.theta")
    if (nlevels(fitd$.person) > 1L && nlevels(fitd$.person) <= 100L) rhs <- c(rhs, ".person")
    if (nlevels(fitd$.item) > 1L && nlevels(fitd$.item) <= 100L) rhs <- c(rhs, ".item")
    fit <- stats::lm(stats::as.formula(paste(".pupil ~", paste(rhs, collapse = " + "))), data = fitd)
  }
  fitd$pupil_confound_residual <- as.numeric(stats::residuals(fit))
  fitd$pupil_confound_adjusted <- fitd$pupil_confound_residual + mean(fitd$.pupil, na.rm = TRUE)
  structure(list(
    model = fit, data = fitd, engine = chosen,
    original_columns = list(pupil = pupil, luminance = luminance, trial_order = trial_order,
                            theta = theta, person = person, item = item),
    status = "confound_adjustment_sensitivity",
    caveat = paste(
      "Adjusted pupil values remain model-dependent and should be described as luminance/fatigue-adjusted,",
      "not as pure cognition or effort isolated from all confounding."
    )
  ), class = "eye_pupil_confound_model")
}

#' Extract confound-adjusted pupil values
#' @export
#' @param x Object to process, inspect, compare, or plot.
adjust_pupil_confounds <- function(x) {
  if (!inherits(x, "eye_pupil_confound_model")) stop("x must be eye_pupil_confound_model.", call. = FALSE)
  x$data
}

#' Extract pupil confound-model effects
#' @export
#' @param x Object to process, inspect, compare, or plot.
pupil_confound_effects <- function(x) {
  if (!inherits(x, "eye_pupil_confound_model")) stop("x must be eye_pupil_confound_model.", call. = FALSE)
  if (inherits(x$model, "gam")) return(summary(x$model)$s.table)
  summary(x$model)$coefficients
}

#' Audit within-person pupil fatigue/trial-order drift
#'
#' @param data Trial-level data.
#' @param pupil,trial_order,person Required columns.
#' @param luminance,difficulty Optional covariates.
#' @param engine `auto`, `plm`, or `lm_fixed_effects`.
#' @export
audit_pupil_fatigue_drift <- function(
    data, pupil = "pupil_peak", trial_order = "trial_sequence", person = "person_id",
    luminance = NULL, difficulty = NULL,
    engine = c("auto", "plm", "lm_fixed_effects")) {
  engine <- match.arg(engine)
  data <- .ep08_as_df(data)
  req <- c(pupil, trial_order, person, luminance, difficulty)
  req <- req[!is.null(req) & nzchar(req)]
  .ep08_req_cols(data, req)
  d <- data.frame(
    pupil = .ep08_num(data[[pupil]]), trial_order = .ep08_num(data[[trial_order]]),
    person = factor(data[[person]]), stringsAsFactors = FALSE
  )
  if (!is.null(luminance)) d$luminance <- .ep08_num(data[[luminance]])
  if (!is.null(difficulty)) d$difficulty <- .ep08_num(data[[difficulty]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 20L || nlevels(droplevels(d$person)) < 2L)
    stop("At least 20 complete rows from two or more persons are required.", call. = FALSE)
  d$person <- droplevels(d$person)
  rhs <- c("trial_order")
  if ("luminance" %in% names(d)) rhs <- c(rhs, "luminance")
  if ("difficulty" %in% names(d)) rhs <- c(rhs, "difficulty")
  chosen <- engine
  if (engine == "auto") chosen <- if (requireNamespace("plm", quietly = TRUE)) "plm" else "lm_fixed_effects"
  if (chosen == "plm") {
    if (!requireNamespace("plm", quietly = TRUE)) stop("Package `plm` is required.", call. = FALSE)
    d$.index <- ave(seq_len(nrow(d)), d$person, FUN = seq_along)
    pd <- plm::pdata.frame(d, index = c("person", ".index"), row.names = FALSE)
    fit <- plm::plm(stats::reformulate(rhs, response = "pupil"), data = pd, model = "within")
  } else {
    fit <- stats::lm(stats::reformulate(c(rhs, "person"), response = "pupil"), data = d)
  }
  co <- summary(fit)$coefficients
  structure(list(model = fit, coefficients = co, data = d, engine = chosen,
                 status = "within_person_fatigue_drift_sensitivity",
                 caveat = "Trial-order association is a fatigue/drift sensitivity signal, not proof of a fatigue mechanism."),
            class = "eye_pupil_fatigue_drift")
}

#' Compare raw and confound-adjusted pupil values
#' @export
#' @param x Object to process, inspect, compare, or plot.
compare_raw_adjusted_pupil <- function(x) {
  if (!inherits(x, "eye_pupil_confound_model")) stop("x must be eye_pupil_confound_model.", call. = FALSE)
  d <- x$data
  data.frame(
    metric = c("mean", "sd", "correlation_with_luminance", "correlation_with_trial_order"),
    raw = c(.ep08_mean(d$.pupil), .ep08_sd(d$.pupil),
            suppressWarnings(stats::cor(d$.pupil, d$.luminance, use = "complete.obs")),
            suppressWarnings(stats::cor(d$.pupil, d$.trial, use = "complete.obs"))),
    adjusted = c(.ep08_mean(d$pupil_confound_adjusted), .ep08_sd(d$pupil_confound_adjusted),
                 suppressWarnings(stats::cor(d$pupil_confound_adjusted, d$.luminance, use = "complete.obs")),
                 suppressWarnings(stats::cor(d$pupil_confound_adjusted, d$.trial, use = "complete.obs")))
  )
}

#' Filter a one-dimensional eye signal robustly
#'
#' @param signal Numeric signal.
#' @param width Median-filter width.
#' @param method `auto`, `robfilter`, or `runmed`.
#' @param online Passed to `robfilter::med.filter()` when used.
#' @return An `eye_signal_filter_audit` object.
#' @export
filter_eye_signal <- function(signal, width = 9L,
                              method = c("auto", "robfilter", "runmed"), online = TRUE) {
  method <- match.arg(method)
  y <- .ep08_num(signal)
  if (sum(is.finite(y)) < 5L) stop("Too few finite signal values.", call. = FALSE)
  width <- as.integer(width); width <- max(3L, width)
  if (width %% 2L == 0L) width <- width + 1L
  max_odd <- if (length(y) %% 2L == 1L) length(y) else length(y) - 1L
  width <- min(width, max_odd)
  if (width < 3L) stop("Signal is too short for median filtering.", call. = FALSE)
  chosen <- method
  if (method == "auto") chosen <- if (requireNamespace("robfilter", quietly = TRUE)) "robfilter" else "runmed"
  if (chosen == "robfilter") {
    if (!requireNamespace("robfilter", quietly = TRUE)) stop("Package `robfilter` is required.", call. = FALSE)
    fit <- robfilter::med.filter(y = y, width = width, minNonNAs = max(2L, floor(width / 2)),
                                online = online, extrapolate = TRUE)
    level <- fit$level
    if (is.data.frame(level) || is.matrix(level)) level <- level[, ncol(level)]
    filtered <- as.numeric(level)
  } else {
    yi <- .ep08_interp_signal(y)
    filtered <- stats::runmed(yi, k = width, endrule = "median")
    fit <- NULL
  }
  tab <- data.frame(sample_index = seq_along(y), raw = y, filtered = filtered,
                    residual = y - filtered)
  structure(list(data = tab, fit = fit, method = chosen, width = width,
                 status = if (chosen == "robfilter") "robust_online_median_filter" else "base_running_median_reference",
                 caveat = "Filtering choices can change downstream pupil/process features; preserve raw data and audit sensitivity."),
            class = "eye_signal_filter_audit")
}

#' Filter pupil signal robustly
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
filter_pupil_signal <- function(...) filter_eye_signal(...)

#' Summarize a signal-filter audit
#' @export
#' @param x Object to process, inspect, compare, or plot.
audit_signal_filter <- function(x) {
  if (!inherits(x, "eye_signal_filter_audit")) stop("x must be eye_signal_filter_audit.", call. = FALSE)
  d <- x$data
  data.frame(method = x$method, width = x$width,
             raw_sd = .ep08_sd(d$raw), filtered_sd = .ep08_sd(d$filtered),
             residual_sd = .ep08_sd(d$residual),
             raw_filtered_cor = suppressWarnings(stats::cor(d$raw, d$filtered, use = "complete.obs")))
}

#' Compare multiple signal filters
#' @param signal Numeric signal.
#' @param widths Widths to compare.
#' @param methods Methods to compare.
#' @export
compare_signal_filters <- function(signal, widths = c(5L, 9L, 15L), methods = c("runmed", "robfilter")) {
  rows <- list(); k <- 0L
  for (m in methods) {
    if (m == "robfilter" && !requireNamespace("robfilter", quietly = TRUE)) next
    for (w in widths) {
      f <- filter_eye_signal(signal, width = w, method = m)
      a <- audit_signal_filter(f)
      k <- k + 1L; rows[[k]] <- a
    }
  }
  if (!length(rows)) stop("No requested filter method was available.", call. = FALSE)
  do.call(rbind, rows)
}
