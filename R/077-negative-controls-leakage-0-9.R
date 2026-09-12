# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Temporal provenance, leakage sentinels, placebo windows, and negative controls.

#' Declare temporal provenance for process features
#'
#' @param feature Feature names.
#' @param available_at Earliest time at which each feature is available.
#' @param outcome_at Time at which the modeled outcome becomes available.
#' @param source Optional source labels.
#' @param transformation Optional transformation descriptions.
#' @param unit Time unit label.
#' @return A provenance table.
#' @export
process_feature_time_provenance <- function(feature, available_at, outcome_at,
                                            source = NA_character_, transformation = NA_character_,
                                            unit = "ms") {
  lens <- c(length(feature), length(available_at), length(outcome_at), length(source), length(transformation))
  n <- max(lens)
  if (!is.finite(n) || n < 1L) stop("At least one feature must be supplied.", call. = FALSE)
  recycle <- function(x) {
    if (!length(x)) rep(NA, n) else rep(x, length.out = n)
  }
  out <- data.frame(
    feature = as.character(recycle(feature)),
    available_at = .ep09_num(recycle(available_at)),
    outcome_at = .ep09_num(recycle(outcome_at)),
    source = as.character(recycle(source)),
    transformation = as.character(recycle(transformation)),
    unit = as.character(unit)[1L],
    stringsAsFactors = FALSE
  )
  if (any(is.na(out$feature) | !nzchar(out$feature))) stop("feature names cannot be missing or empty.", call. = FALSE)
  if (any(!is.finite(out$available_at)) || any(!is.finite(out$outcome_at))) stop("available_at and outcome_at must be finite.", call. = FALSE)
  out$lead <- out$outcome_at - out$available_at
  out$available_before_outcome <- is.finite(out$lead) & out$lead >= 0
  class(out) <- c("eye_feature_time_provenance", class(out))
  out
}

#' Audit temporal leakage in a feature provenance table
#'
#' Leakage here means information becoming available after the declared outcome
#' boundary. It is a data/analysis property and is not an allegation of misconduct.
#'
#' @param provenance Output of [process_feature_time_provenance()] or compatible table.
#' @param allow_equal Whether features available exactly at outcome time are allowed.
#' @param tolerance Numeric tolerance in the provenance time unit.
#' @export
audit_temporal_leakage <- function(provenance, allow_equal = TRUE, tolerance = 0) {
  d <- .ep09_as_df(provenance)
  .ep09_req_cols(d, c("feature", "available_at", "outcome_at"), "provenance")
  tolerance <- .ep09_num(tolerance)[1L]
  if (!is.finite(tolerance) || tolerance < 0) stop("tolerance must be a non-negative finite scalar.", call. = FALSE)
  delta <- .ep09_num(d$available_at) - .ep09_num(d$outcome_at)
  leak <- if (isTRUE(allow_equal)) delta > tolerance else delta >= -tolerance
  detail <- data.frame(
    feature = as.character(d$feature),
    available_at = .ep09_num(d$available_at),
    outcome_at = .ep09_num(d$outcome_at),
    temporal_delta = delta,
    leakage_flag = leak,
    stringsAsFactors = FALSE
  )
  out <- list(
    status = if (any(leak, na.rm = TRUE)) "flagged" else "pass",
    n_features = nrow(detail),
    n_flagged = sum(leak, na.rm = TRUE),
    flagged_fraction = if (nrow(detail)) mean(leak, na.rm = TRUE) else NA_real_,
    detail = detail,
    interpretation = "A leakage flag denotes temporal/information contamination relative to the declared outcome boundary; it is not a misconduct label."
  )
  class(out) <- "eye_temporal_leakage_audit"
  out
}

#' Validate feature availability against an analysis cutoff
#' @param provenance Feature provenance table.
#' @param cutoff Scalar cutoff or named vector by feature.
#' @export
validate_feature_availability <- function(provenance, cutoff) {
  d <- .ep09_as_df(provenance)
  .ep09_req_cols(d, c("feature", "available_at"), "provenance")
  if (!length(cutoff)) stop("cutoff cannot be empty.", call. = FALSE)
  if (length(cutoff) == 1L) lim <- rep(.ep09_num(cutoff), nrow(d)) else {
    if (is.null(names(cutoff)) || any(is.na(names(cutoff)) | !nzchar(names(cutoff)))) stop("A multi-value cutoff must be named by feature.", call. = FALSE)
    pos <- match(d$feature, names(cutoff))
    if (anyNA(pos)) stop("A named cutoff is required for every feature.", call. = FALSE)
    lim <- .ep09_num(cutoff[pos])
  }
  if (any(!is.finite(lim))) stop("cutoff values must be finite.", call. = FALSE)
  out <- data.frame(feature = d$feature, available_at = .ep09_num(d$available_at), cutoff = lim,
                    available = .ep09_num(d$available_at) <= lim, stringsAsFactors = FALSE)
  out
}

#' Audit whether candidate predictors can be constructed without an outcome column
#'
#' @param data Data frame.
#' @param outcome Outcome column name.
#' @param feature_fun Function receiving outcome-hidden data and returning features.
#' @export
outcome_blind_feature_audit <- function(data, outcome, feature_fun) {
  d <- .ep09_as_df(data)
  if (!outcome %in% names(d)) stop("outcome column not found.", call. = FALSE)
  if (!is.function(feature_fun)) stop("feature_fun must be a function.", call. = FALSE)
  hidden <- d
  hidden[[outcome]] <- NULL
  cap <- .ep09_capture(feature_fun(hidden))
  out <- list(
    status = if (is.na(cap$error)) "pass" else "failed",
    outcome = outcome,
    feature_result = if (is.na(cap$error)) cap$value else NULL,
    error = cap$error,
    warnings = cap$warnings,
    input_columns = names(hidden),
    interpretation = "Passing establishes that this feature-construction call executed without the supplied outcome column; it does not prove absence of all indirect leakage."
  )
  class(out) <- "eye_outcome_blind_feature_audit"
  out
}

#' Permutation negative control
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param seed Random seed.
#' @param within Optional grouping columns within which to permute.
#' @export
process_negative_control_permute <- function(data, outcome, seed = 1L, within = NULL) {
  d <- .ep09_as_df(data)
  if (length(outcome) != 1L || is.na(outcome) || !nzchar(outcome)) stop("outcome must name one column.", call. = FALSE)
  .ep09_req_cols(d, outcome)
  seed <- .ep09_num(seed)[1L]
  if (!is.finite(seed) || seed < 0) stop("seed must be a non-negative finite scalar.", call. = FALSE)
  seed <- as.integer(seed %% .Machine$integer.max)
  set.seed(seed)
  if (is.null(within) || !length(within)) {
    d[[outcome]] <- sample(d[[outcome]], replace = FALSE)
  } else {
    .ep09_req_cols(d, within)
    key <- interaction(d[within], drop = TRUE, lex.order = TRUE)
    idxs <- split(seq_len(nrow(d)), key)
    for (ii in idxs) d[[outcome]][ii] <- sample(d[[outcome]][ii], replace = FALSE)
  }
  attr(d, "negative_control") <- list(type = "permutation", outcome = outcome, seed = seed, within = within)
  d
}

#' Temporal-shift negative control
#' @param data Data frame.
#' @param column Column to shift.
#' @param lag Number of rows to shift within each group.
#' @param by Optional grouping columns.
#' @export
process_negative_control_shift <- function(data, column, lag = 1L, by = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(column, by))
  lag <- .ep09_num(lag)[1L]
  if (!is.finite(lag) || lag != round(lag)) stop("lag must be a finite integer.", call. = FALSE)
  lag <- as.integer(lag)
  shift_one <- function(x) {
    n <- length(x); if (!n || lag == 0L) return(x)
    if (abs(lag) >= n) return(rep(NA, n))
    if (lag > 0L) c(rep(NA, lag), head(x, n - lag)) else c(tail(x, n + lag), rep(NA, -lag))
  }
  if (is.null(by) || !length(by)) d[[column]] <- shift_one(d[[column]]) else {
    key <- interaction(d[by], drop = TRUE, lex.order = TRUE)
    idxs <- split(seq_len(nrow(d)), key)
    for (ii in idxs) d[[column]][ii] <- shift_one(d[[column]][ii])
  }
  attr(d, "negative_control") <- list(type = "temporal_shift", column = column, lag = lag, by = by)
  d
}

#' Audit a placebo/pre-event window
#' @param data Data frame.
#' @param time Time column.
#' @param value Value column.
#' @param window Two-element placebo window.
#' @param expected Optional expected mean, typically zero.
#' @param by Optional grouping columns.
#' @export
placebo_window_audit <- function(data, time, value, window, expected = 0, by = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(time, value, by))
  if (length(window) != 2L || any(!is.finite(.ep09_num(window)))) stop("window must contain two finite values.", call. = FALSE)
  lo <- min(.ep09_num(window)); hi <- max(.ep09_num(window))
  expected <- .ep09_num(expected)[1L]
  if (!is.finite(expected)) stop("expected must be a finite scalar.", call. = FALSE)
  z <- d[.ep09_num(d[[time]]) >= lo & .ep09_num(d[[time]]) <= hi, , drop = FALSE]
  summarise <- function(q) {
    v <- .ep09_num(q[[value]]); v <- v[is.finite(v)]
    n <- length(v); m <- if (n) mean(v) else NA_real_; se <- if (n > 1L) stats::sd(v) / sqrt(n) else NA_real_
    data.frame(n = n, mean = m, sd = if (n > 1L) stats::sd(v) else NA_real_, se = se,
               difference_from_expected = m - expected, stringsAsFactors = FALSE)
  }
  if (is.null(by) || !length(by)) out <- summarise(z) else {
    key <- interaction(z[by], drop = TRUE, lex.order = TRUE); idxs <- split(seq_len(nrow(z)), key)
    out <- .ep09_rbind_fill(lapply(idxs, function(ii) cbind(z[ii[1L], by, drop = FALSE], summarise(z[ii, , drop = FALSE]))))
  }
  attr(out, "placebo_window") <- c(lo, hi)
  attr(out, "expected") <- expected
  class(out) <- c("eye_placebo_window_audit", class(out))
  out
}

.ep09_default_control_extract <- function(x) {
  if (is.numeric(x) && length(x) == 1L) return(data.frame(effect = as.numeric(x)))
  if (is.list(x) && !is.null(x$effect)) return(data.frame(effect = as.numeric(x$effect)[1L]))
  if (is.data.frame(x)) return(x)
  stop("analysis_fun output must be numeric, a list with $effect, or a data.frame, unless extract_fun is supplied.", call. = FALSE)
}

#' Run repeated process negative controls
#' @param data Data frame.
#' @param outcome Outcome column.
#' @param analysis_fun Function applied to each negative-control dataset.
#' @param controls Character vector among `permutation` and `shift`.
#' @param replications Number of controls per type.
#' @param seed Seed.
#' @param extract_fun Function converting analysis result to a data.frame.
#' @param shift_lags Lags sampled for shift controls.
#' @param within Optional permutation groups.
#' @export
run_process_negative_controls <- function(data, outcome, analysis_fun,
                                          controls = c("permutation", "shift"),
                                          replications = 100L, seed = 1L,
                                          extract_fun = .ep09_default_control_extract,
                                          shift_lags = c(-3L, -2L, -1L, 1L, 2L, 3L), within = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, outcome)
  controls <- match.arg(controls, c("permutation", "shift"), several.ok = TRUE)
  replications0 <- .ep09_num(replications)[1L]
  if (!is.finite(replications0) || replications0 < 1L || replications0 != round(replications0)) stop("replications must be a positive integer.", call. = FALSE)
  replications <- as.integer(replications0)
  if ("shift" %in% controls && !length(shift_lags)) stop("shift_lags cannot be empty when shift controls are requested.", call. = FALSE)
  shift_lags <- .ep09_num(shift_lags)
  if ("shift" %in% controls && any(!is.finite(shift_lags) | shift_lags != round(shift_lags) | shift_lags == 0)) stop("shift_lags must contain non-zero finite integers.", call. = FALSE)
  if (!is.function(analysis_fun) || !is.function(extract_fun)) stop("analysis_fun and extract_fun must be functions.", call. = FALSE)
  seed <- .ep09_num(seed)[1L]
  if (!is.finite(seed) || seed < 0) stop("seed must be a non-negative finite scalar.", call. = FALSE)
  seed <- as.integer(seed %% .Machine$integer.max)
  set.seed(seed); rows <- list(); k <- 0L
  for (ctrl in controls) for (r in seq_len(as.integer(replications))) {
    k <- k + 1L
    dd <- if (ctrl == "permutation") process_negative_control_permute(d, outcome, seed = (as.double(seed) + k) %% .Machine$integer.max, within = within) else
      process_negative_control_shift(d, outcome, lag = shift_lags[((r - 1L) %% length(shift_lags)) + 1L], by = within)
    cap <- .ep09_capture(analysis_fun(dd))
    ext <- if (is.na(cap$error)) .ep09_capture(extract_fun(cap$value)) else list(value = NULL, error = cap$error, warnings = cap$warnings)
    rr <- if (is.na(ext$error)) .ep09_as_df(ext$value) else data.frame()
    if (!nrow(rr)) rr <- data.frame(effect = NA_real_)
    rr$control <- ctrl; rr$replication <- r; rr$error <- ext$error
    rr$warnings <- paste(unique(c(cap$warnings, ext$warnings)), collapse = " | ")
    rows[[k]] <- rr
  }
  out <- list(results = .ep09_rbind_fill(rows), outcome = outcome, controls = controls,
              replications = as.integer(replications), seed = as.integer(seed),
              interpretation = "Negative controls probe whether the analysis produces signal after deliberately disrupting the declared outcome/process relation; they do not prove validity when null-like.")
  class(out) <- "eye_process_negative_controls"
  out
}

#' Summarise process negative controls
#' @param x Negative-control result.
#' @param effect Effect column.
#' @param threshold Optional absolute effect threshold.
#' @export
summarise_process_negative_controls <- function(x, effect = "effect", threshold = 0) {
  if (!inherits(x, "eye_process_negative_controls")) stop("x must be an eye_process_negative_controls object.", call. = FALSE)
  d <- x$results; .ep09_req_cols(d, c("control", effect))
  threshold <- .ep09_num(threshold)[1L]
  if (!is.finite(threshold)) stop("threshold must be a finite scalar.", call. = FALSE)
  splitd <- split(d, d$control)
  out <- .ep09_rbind_fill(lapply(names(splitd), function(nm) {
    z <- .ep09_num(splitd[[nm]][[effect]]); ok <- is.finite(z)
    data.frame(control = nm, n = length(z), n_finite = sum(ok), mean = if (any(ok)) mean(z[ok]) else NA_real_,
               sd = if (sum(ok) > 1L) stats::sd(z[ok]) else NA_real_, median = if (any(ok)) stats::median(z[ok]) else NA_real_,
               q025 = .ep09_quantile(z, .025), q975 = .ep09_quantile(z, .975),
               exceedance_rate = if (any(ok)) mean(abs(z[ok]) > abs(threshold)) else NA_real_, stringsAsFactors = FALSE)
  }))
  out
}

#' Compare an observed effect against a negative-control null distribution
#' @param observed Observed scalar effect.
#' @param controls Negative-control result or numeric vector.
#' @param effect Effect column when controls is an object.
#' @export
process_null_benchmark <- function(observed, controls, effect = "effect") {
  null <- if (inherits(controls, "eye_process_negative_controls")) {
    .ep09_req_cols(controls$results, effect); .ep09_num(controls$results[[effect]])
  } else .ep09_num(controls)
  null <- null[is.finite(null)]; obs <- .ep09_num(observed)[1L]
  if (!length(null) || !is.finite(obs)) return(list(observed = obs, n_null = length(null), percentile = NA_real_, two_sided_tail = NA_real_))
  list(observed = obs, n_null = length(null), null_mean = mean(null), null_sd = if (length(null) > 1L) stats::sd(null) else NA_real_,
       percentile = mean(null <= obs), two_sided_tail = mean(abs(null) >= abs(obs)),
       standardized_distance = if (length(null) > 1L && stats::sd(null) > 0) (obs - mean(null))/stats::sd(null) else NA_real_)
}

#' Concordance of multiple negative-control families
#' @param x Negative-control result.
#' @param effect Effect column.
#' @param tolerance Absolute mean-effect tolerance.
#' @export
negative_control_concordance <- function(x, effect = "effect", tolerance = 0.05) {
  tolerance <- .ep09_num(tolerance)[1L]
  if (!is.finite(tolerance) || tolerance < 0) stop("tolerance must be a non-negative finite scalar.", call. = FALSE)
  s <- summarise_process_negative_controls(x, effect = effect)
  s$within_tolerance <- ifelse(is.finite(s$mean), abs(s$mean) <= tolerance, NA)
  finite <- !is.na(s$within_tolerance)
  list(summary = s, all_within_tolerance = if (any(finite)) all(s$within_tolerance[finite]) else NA, tolerance = tolerance)
}

#' @export
print.eye_temporal_leakage_audit <- function(x, ...) {
  cat("<eye_temporal_leakage_audit>\n", " status: ", x$status, "\n", " features: ", x$n_features,
      "\n flagged: ", x$n_flagged, "\n", sep = "")
  invisible(x)
}

#' @export
print.eye_process_negative_controls <- function(x, ...) {
  cat("<eye_process_negative_controls>\n", " controls: ", paste(x$controls, collapse = ", "),
      "\n rows: ", nrow(x$results), "\n", sep = "")
  invisible(x)
}
