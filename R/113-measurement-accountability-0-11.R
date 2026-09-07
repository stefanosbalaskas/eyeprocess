# Measurement-accountability diagnostics -------------------------------------

.ep_finite <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}

.ep_mad_sigma <- function(x) {
  x <- .ep_finite(x)
  if (!length(x)) return(NA_real_)
  1.4826 * stats::median(abs(x - stats::median(x)))
}

#' Pupil latency estimator sensitivity and resolvability audit
#'
#' Estimates pupil-response latency using a sustained threshold, maximum-slope
#' tangent intersection, and piecewise breakpoint. The result reports estimator
#' spread and a sampling/noise-aware resolvability label rather than treating a
#' single latency estimate as algorithm- or hardware-independent.
#'
#' @param time Numeric sample times in seconds.
#' @param pupil Numeric pupil values.
#' @param event_time Nominal event time in seconds.
#' @param baseline_window Two-element window relative to `event_time`.
#' @param search_window Two-element search window relative to `event_time`.
#' @param direction `"constriction"` or `"dilation"`.
#' @param threshold_sigma Robust-noise multiples used by the sustained threshold.
#' @param sustain_ms Required duration above threshold in milliseconds.
#' @return A list of estimator-specific latencies, spread, signal diagnostics,
#'   resolvability, and provenance.
#' @export
pupil_latency_sensitivity <- function(
    time, pupil, event_time = 0,
    baseline_window = c(-0.5, 0), search_window = c(0, 2),
    direction = c("constriction", "dilation"), threshold_sigma = 3,
    sustain_ms = 40) {
  direction <- match.arg(direction)
  if (length(time) != length(pupil) || length(time) < 8L) {
    stop("`time` and `pupil` must have equal length and at least 8 samples.", call. = FALSE)
  }
  keep <- is.finite(time) & is.finite(pupil)
  time <- as.numeric(time[keep]); pupil <- as.numeric(pupil[keep])
  ord <- order(time); time <- time[ord]; pupil <- pupil[ord]
  dt <- stats::median(diff(unique(time)))
  if (!is.finite(dt) || dt <= 0) stop("`time` must contain increasing samples.", call. = FALSE)
  b <- time >= event_time + baseline_window[1] & time < event_time + baseline_window[2]
  s <- time >= event_time + search_window[1] & time <= event_time + search_window[2]
  if (sum(b) < 3L || sum(s) < 4L) stop("Baseline/search windows contain too few samples.", call. = FALSE)
  baseline <- stats::median(pupil[b])
  noise <- .ep_mad_sigma(pupil[b])
  if (!is.finite(noise) || noise == 0) noise <- stats::sd(pupil[b])
  if (!is.finite(noise) || noise == 0) noise <- .Machine$double.eps
  sign <- if (direction == "constriction") -1 else 1
  st <- time[s]
  response <- sign * (pupil[s] - baseline)
  run_n <- max(1L, round((sustain_ms / 1000) / dt))
  hit <- response >= threshold_sigma * noise
  run <- stats::filter(as.integer(hit), rep(1, run_n), sides = 1)
  hit_i <- which(run >= run_n)[1]
  threshold_latency <- if (length(hit_i)) st[max(1L, hit_i - run_n + 1L)] - event_time else NA_real_

  slopes <- diff(response) / diff(st)
  peak <- if (length(slopes)) which.max(slopes) + 1L else NA_integer_
  tangent_latency <- NA_real_
  if (is.finite(peak) && slopes[peak - 1L] > 0) {
    tangent_latency <- st[peak] - response[peak] / slopes[peak - 1L] - event_time
  }

  candidates <- 3:(length(st) - 2L)
  sse <- vapply(candidates, function(k) {
    x <- st[k:length(st)] - st[k]
    den <- sum(x^2)
    beta <- if (den > 0) sum(x * response[k:length(response)]) / den else 0
    pred <- c(rep(0, k - 1L), beta * x)
    sum((response - pred)^2)
  }, numeric(1))
  k <- candidates[which.min(sse)]
  breakpoint_latency <- st[k] - event_time

  estimates <- c(
    sustained_threshold = threshold_latency,
    max_slope_tangent = tangent_latency,
    piecewise_breakpoint = breakpoint_latency
  )
  good <- estimates[is.finite(estimates)]
  spread <- if (length(good) >= 2L) diff(range(good)) * 1000 else NA_real_
  amplitude <- max(response, na.rm = TRUE)
  snr <- amplitude / noise
  resolvability <- if (length(good) >= 2L && spread <= 50 && snr >= 3) {
    "high"
  } else if (length(good) && snr >= 1.5) {
    "moderate"
  } else {
    "low"
  }
  structure(list(
    estimates_s = estimates,
    estimator_spread_ms = spread,
    sampling_hz = 1 / dt,
    baseline = baseline,
    noise_mad_sigma = noise,
    response_amplitude = amplitude,
    signal_to_noise = snr,
    latency_resolvability = resolvability,
    provenance = list(direction = direction, threshold_sigma = threshold_sigma,
                      sustain_ms = sustain_ms, baseline_window = baseline_window,
                      search_window = search_window)
  ), class = "eyeprocess_pupil_latency_sensitivity")
}

#' Event-marker plausibility audit
#'
#' Evaluates whether independent channel offsets corroborate a nominal event.
#' This is annotation/event plausibility QC, not clock synchronization, and it
#' never modifies timestamps.
#'
#' @param offsets Numeric corroborating-channel offsets in seconds.
#' @param tolerance Allowed absolute offset in seconds.
#' @param min_corroborating Minimum corroborating channels for `confirmed`.
#' @return A list containing status, consensus offset, uncertainty, and counts.
#' @export
event_marker_qc <- function(offsets, tolerance, min_corroborating = 2L) {
  x <- .ep_finite(offsets)
  if (!is.numeric(tolerance) || length(tolerance) != 1L || tolerance <= 0) {
    stop("`tolerance` must be one positive number.", call. = FALSE)
  }
  if (!length(x)) return(structure(list(status = "implausible", n = 0L,
    estimated_offset_s = NA_real_, uncertainty_s = NA_real_,
    note = "Event plausibility only; no clock-drift correction was applied."),
    class = "eyeprocess_event_marker_qc"))
  center <- stats::median(x)
  uncertainty <- .ep_mad_sigma(x)
  consensus <- sum(abs(x - center) <= tolerance)
  within <- sum(abs(x) <= tolerance)
  status <- if (length(x) >= min_corroborating && consensus >= min_corroborating && abs(center) <= tolerance) {
    "confirmed"
  } else if (consensus >= 1L && abs(center) <= 2 * tolerance) {
    "plausible"
  } else if (consensus >= 1L) {
    "ambiguous"
  } else {
    "implausible"
  }
  structure(list(status = status, n = length(x), within_tolerance = within,
    estimated_offset_s = center, uncertainty_s = uncertainty,
    tolerance_s = tolerance,
    note = "Event plausibility only; no clock-drift correction was applied."),
    class = "eyeprocess_event_marker_qc")
}

#' Build a measurement-to-generalization validation ladder
#'
#' @param acquisition_qc,analytical_qc,construct_check,within_person,held_out_person
#'   Stage statuses: `pass`, `warning`, `fail`, or `not_assessed`.
#' @param claim Claim type; use `generalizable` for out-of-person claims.
#' @return A structured validation-ladder result.
#' @export
validation_ladder <- function(acquisition_qc = "not_assessed", analytical_qc = "not_assessed",
                              construct_check = "not_assessed", within_person = "not_assessed",
                              held_out_person = "not_assessed", claim = "descriptive") {
  stages <- c(acquisition_qc = acquisition_qc, analytical_qc = analytical_qc,
              construct_check = construct_check, within_person = within_person,
              held_out_person = held_out_person)
  stages <- tolower(gsub("-", "_", as.character(stages)))
  valid <- c("pass", "warning", "fail", "not_assessed")
  if (any(!stages %in% valid)) stop("Stage status must be pass, warning, fail, or not_assessed.", call. = FALSE)
  general <- tolower(gsub("-", "_", claim)) %in% c("generalizable", "generalization", "out_of_person", "population")
  held <- unname(stages["held_out_person"]) == "pass"
  status <- if (any(stages == "fail") || (general && !held)) {
    "not_supported"
  } else if (any(stages %in% c("warning", "not_assessed"))) {
    "qualified"
  } else {
    "supported"
  }
  structure(list(stages = stages, claim = claim, claim_status = status,
    held_out_person_generalization = held,
    interpretation = "Within-person evidence is calibration/personalization evidence unless held-out-person validation passes."),
    class = "eyeprocess_validation_ladder")
}
