utils::globalVariables(c("delta_ms"))

# Explicit acquisition-integrity evidence. No timestamp resampling or
# attempted hardware/SDK synchronization is performed.

#' Audit acquisition timestamp integrity
#'
#' Separates a user-declared device-native and SDK rate, the observed
#' timestamp distribution, and a declared analysis grid. A 500-Hz analysis
#' grid never changes the observed information rate of a 4-Hz native stream.
#'
#' @param time_s Numeric timestamps in seconds, in original observation order.
#' @param valid Optional logical per-sample validity vector.
#' @param device_native_rate_hz Optional user-declared hardware rate.
#' @param sdk_delivered_rate_hz Optional user-declared SDK output rate.
#' @param analysis_grid_rate_hz Optional user-declared analysis grid rate.
#' @param catchup_long_ratio Long-interval threshold divided by median positive
#'   interval.
#' @param catchup_short_ratio Next-interval threshold divided by median positive
#'   interval.
#' @return A list of `summary`, `intervals`, and `claim_boundary`.
#' @export
audit_eye_acquisition_integrity <- function(
    time_s, valid = NULL, device_native_rate_hz = NULL,
    sdk_delivered_rate_hz = NULL, analysis_grid_rate_hz = NULL,
    catchup_long_ratio = 2.5, catchup_short_ratio = 0.5) {
  if (!is.numeric(time_s) || length(time_s) < 3L ||
      anyNA(time_s) || any(!is.finite(time_s))) {
    stop("time_s must be >=3 finite numeric timestamps in seconds.", call. = FALSE)
  }
  if (is.null(valid)) valid <- rep(TRUE, length(time_s))
  if (!is.logical(valid) || length(valid) != length(time_s) || anyNA(valid)) {
    stop("valid must be a complete logical vector matching time_s.", call. = FALSE)
  }
  declared <- list(
    device_native_rate_hz = device_native_rate_hz,
    sdk_delivered_rate_hz = sdk_delivered_rate_hz,
    analysis_grid_rate_hz = analysis_grid_rate_hz
  )
  for (name in names(declared)) {
    value <- declared[[name]]
    if (!is.null(value) &&
        (!is.numeric(value) || length(value) != 1L ||
         !is.finite(value) || value <= 0)) {
      stop(name, " must be a finite positive scalar when supplied.", call. = FALSE)
    }
  }
  if (!is.numeric(catchup_long_ratio) || length(catchup_long_ratio) != 1L ||
      !is.finite(catchup_long_ratio) || catchup_long_ratio <= 1 ||
      !is.numeric(catchup_short_ratio) || length(catchup_short_ratio) != 1L ||
      !is.finite(catchup_short_ratio) || catchup_short_ratio <= 0 ||
      catchup_short_ratio >= 1) {
    stop("catchup ratios require long >1 and short between 0 and 1.", call. = FALSE)
  }
  dt <- diff(time_s)
  if (any(dt < 0)) stop("timestamp reversal detected; do not silently sort.", call. = FALSE)
  positive <- dt[dt > 0]
  if (!length(positive)) {
    stop("at least one positive interval is needed to estimate rate.", call. = FALSE)
  }
  median_dt <- stats::median(positive)
  bursts <- c(
    dt[-length(dt)] > catchup_long_ratio * median_dt &
      dt[-1L] < catchup_short_ratio * median_dt,
    FALSE
  )
  p <- stats::quantile(dt * 1000, c(.005, .05, .5, .95, .995),
                       names = FALSE, type = 7)
  summary <- data.frame(
    n_samples = length(time_s),
    duration_s = tail(time_s, 1L) - time_s[[1L]],
    observed_span_rate_hz = if ((tail(time_s, 1L) - time_s[[1L]]) > 0) {
      (length(time_s) - 1) / (tail(time_s, 1L) - time_s[[1L]])
    } else NA_real_,
    observed_positive_median_rate_hz = 1 / median_dt,
    p005_ms = p[[1L]], p05_ms = p[[2L]], median_ms = p[[3L]],
    p95_ms = p[[4L]], p995_ms = p[[5L]],
    max_gap_ms = max(dt) * 1000,
    quantized_or_duplicate_timestamps = sum(dt == 0),
    catchup_bursts = sum(bursts),
    invalid_state_fraction = mean(!valid),
    device_native_rate_hz = if (is.null(device_native_rate_hz)) NA_real_ else device_native_rate_hz,
    sdk_delivered_rate_hz = if (is.null(sdk_delivered_rate_hz)) NA_real_ else sdk_delivered_rate_hz,
    analysis_grid_rate_hz = if (is.null(analysis_grid_rate_hz)) NA_real_ else analysis_grid_rate_hz,
    upsampling_does_not_add_information =
      !is.null(analysis_grid_rate_hz) &&
      (( !is.null(device_native_rate_hz) && analysis_grid_rate_hz > device_native_rate_hz) ||
       analysis_grid_rate_hz > 1 / median_dt)
  )
  intervals <- data.frame(
    sample_index = seq_along(dt) + 1L,
    delta_ms = dt * 1000,
    quantized_or_duplicate = dt == 0,
    catchup_burst_start = bursts
  )
  structure(list(
    summary = summary,
    intervals = intervals,
    claim_boundary = paste(
      "Timestamp process only; quantization and scheduling can produce",
      "zeros or catch-up bursts. Declared rates are not measured hardware",
      "rates; analysis-grid upsampling cannot restore temporal information."
    )
  ), class = "eye_acquisition_integrity")
}

#' Plot observed acquisition intervals
#'
#' @param x An audit from `audit_eye_acquisition_integrity()`.
#' @return A ggplot histogram with the observed positive median interval.
#' @export
plot_eye_acquisition_integrity <- function(x) {
  if (!inherits(x, "eye_acquisition_integrity")) {
    stop("x must be an eye_acquisition_integrity audit.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install optional ggplot2 for acquisition plotting.", call. = FALSE)
  }
  data <- x$intervals
  reference <- x$summary$median_ms
  ggplot2::ggplot(data, ggplot2::aes(x = delta_ms)) +
    ggplot2::geom_histogram(bins = 35L, fill = "#387C88",
                            colour = "white") +
    ggplot2::geom_vline(xintercept = reference, linetype = "dashed") +
    ggplot2::labs(
      title = "Observed acquisition intervals",
      subtitle = "Recorded gaps and timestamp quantization are preserved",
      x = "Inter-sample interval (milliseconds)",
      y = "Recorded intervals"
    ) +
    ggplot2::theme_minimal()
}
