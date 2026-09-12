# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Unified process-measure registry and reliability/repeatability diagnostics.

.ep09_builtin_process_registry <- function() {
  data.frame(
    name = c(
      "fixation_count", "fixation_duration_mean", "first_fixation_duration", "dwell_time", "regression_count",
      "saccade_amplitude", "scanpath_length", "aoi_transition_count", "aoi_transition_entropy", "time_to_first_fixation",
      "pupil_mean", "pupil_peak", "pupil_baseline", "pupil_change", "pupil_velocity_activity",
      "response_time", "omission_indicator", "accuracy_indicator", "gaze_validity", "pupil_validity",
      "sampling_rate_effective", "calibration_error", "gaze_precision_rms_s2s", "data_loss",
      "process_profile_score", "streaming_theta", "process_anomaly_distance", "presentation_sensitivity_index"
    ),
    channel = c(
      rep("gaze", 10), rep("pupil", 5), "behavior", "behavior", "behavior",
      "quality", "quality", "quality", "quality", "quality", "quality",
      "derived", "psychometric", "quality", "presentation"
    ),
    unit = c(
      "count", "ms", "ms", "ms", "count", "visual-angle/user units", "coordinate units", "count", "bits", "ms",
      "device/user units", "device/user units", "device/user units", "device/user units", "device/user units per time",
      "ms", "binary", "binary", "proportion", "proportion", "Hz", "coordinate/visual-angle units", "coordinate/visual-angle units", "proportion",
      "model-specific", "latent-score units", "distance", "index"
    ),
    level = c(
      rep("trial/AOI/person", 10), rep("trial/person", 5), "trial", "trial", "trial", "recording", "recording", "recording", "recording", "recording", "recording",
      "person/trial", "person/step", "person", "trial/person"
    ),
    interpretation = c(
      "Observed count of classified fixations.", "Observed mean duration of classified fixations.", "Duration of the first classified fixation.",
      "Observed accumulated dwell within an AOI.", "Observed count of backward/revisit movements under the declared rule.",
      "Observed saccade amplitude under declared event detection.", "Observed spatial path length.", "Observed AOI-to-AOI transition count.",
      "Dispersion of observed AOI transitions.", "Latency to first qualifying fixation within an AOI.",
      "Observed pupil-size summary after declared preprocessing.", "Observed pupil maximum after declared preprocessing.",
      "Observed pre-event pupil baseline.", "Observed difference from the declared baseline.", "Observed pupil change/activity feature.",
      "Observed response latency.", "Observed response omission.", "Observed scoring indicator.",
      "Proportion of gaze samples meeting the declared validity rule.", "Proportion of pupil samples meeting the declared validity rule.",
      "Empirical sample frequency inferred from timestamps.", "Observed validation-target offset under the declared coordinate system.",
      "Successive-sample gaze dispersion metric.", "Proportion of expected/recorded gaze samples unavailable under the declared denominator.",
      "Model-derived process-profile coordinate.", "Partial latent score from a calibrated response model.",
      "Multivariate review statistic for process-feature unusualness.", "Observed sensitivity to presentation variants under the declared audit."
    ),
    guardrail = c(
      rep("Do not interpret as attention, cognition, motivation, diagnosis, or intent without external construct-validity evidence.", 15),
      "Response time is behavioral timing, not a direct measure of latent cognition.",
      "Omission status does not by itself identify disengagement, misconduct, or inability.",
      "Accuracy is a scored response property; construct interpretation depends on the assessment design.",
      rep("Quality metrics characterize measurement conditions and should not be converted into participant labels without a justified protocol.", 6),
      "Profile scores are model-derived summaries and require external validation before person-level interpretation.",
      "Streaming scores require calibrated banks and operational validation before consequential use.",
      "Large distances are review statistics, not misconduct/diagnostic labels.",
      "Presentation sensitivity is a measurement/presentation diagnostic, not a clinical accessibility classification."
    ),
    status = c(rep("reference", 24), "experimental", "experimental", "reference", "experimental"),
    stringsAsFactors = FALSE
  )
}

#' Unified process-measure registry
#' @param include_experimental Include experimental registry entries.
#' @return A data frame containing unified process-measure registry. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
process_measure_registry <- function(include_experimental = TRUE) {
  x <- .ep09_builtin_process_registry()
  if (!isTRUE(include_experimental)) x <- x[x$status != "experimental", , drop = FALSE]
  class(x) <- c("eye_process_measure_registry", "data.frame")
  x
}

#' Validate a process-measure registry
#' @param registry Registry data frame.
#' @return A logical value or vector indicating a process-measure registry.
#' @export
validate_process_measure_registry <- function(registry) {
  registry <- .ep09_as_df(registry)
  req <- c("name", "channel", "unit", "level", "interpretation", "guardrail", "status")
  .ep09_req_cols(registry, req, "registry")
  registry[req] <- lapply(registry[req], as.character)
  if (anyDuplicated(registry$name)) stop("Process registry contains duplicate measure names.", call. = FALSE)
  if (any(vapply(registry[req], function(z) anyNA(z) || any(!nzchar(z)), logical(1))))
    stop("Every process measure requires non-missing name, channel, unit, level, interpretation, guardrail, and status metadata.", call. = FALSE)
  invisible(TRUE)
}

#' Add a process measure to a registry without global mutation
#' @param registry Registry.
#' @param name,channel,unit,level,interpretation,guardrail,status Measure metadata.
#' @return A tabular R object containing add a process measure to a registry without global mutation; rows represent analysis units and columns contain the returned quantities.
#' @export
register_process_measure <- function(registry = process_measure_registry(), name, channel, unit, level,
                                     interpretation, guardrail, status = "user_defined") {
  reg <- .ep09_as_df(registry)
  vals <- list(name = name, channel = channel, unit = unit, level = level, interpretation = interpretation, guardrail = guardrail, status = status)
  if (any(vapply(vals, length, integer(1)) != 1L)) stop("Process-measure metadata fields must be scalar.", call. = FALSE)
  row <- data.frame(name = as.character(name), channel = as.character(channel), unit = as.character(unit), level = as.character(level),
                    interpretation = as.character(interpretation), guardrail = as.character(guardrail), status = as.character(status),
                    stringsAsFactors = FALSE)
  validate_process_measure_registry(row)
  reg <- reg[reg$name != row$name[[1L]], , drop = FALSE]
  out <- rbind(reg, row)
  validate_process_measure_registry(out)
  class(out) <- c("eye_process_measure_registry", "data.frame")
  out
}

#' Find process measures by channel, level, status, or text
#' @param registry Registry.
#' @param channel Optional channel filter.
#' @param level Optional level pattern.
#' @param status Optional status filter.
#' @param query Optional text query.
#' @return An R object containing find process measures by channel, level, status, or text. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
find_process_measures <- function(registry = process_measure_registry(), channel = NULL,
                                  level = NULL, status = NULL, query = NULL) {
  reg <- .ep09_as_df(registry); validate_process_measure_registry(reg)
  keep <- rep(TRUE, nrow(reg))
  if (!is.null(channel)) keep <- keep & reg$channel %in% channel
  if (!is.null(level)) keep <- keep & grepl(paste(level, collapse = "|"), reg$level, ignore.case = TRUE)
  if (!is.null(status)) keep <- keep & reg$status %in% status
  if (!is.null(query)) {
    txt <- apply(reg[c("name", "channel", "interpretation", "guardrail")], 1L, paste, collapse = " ")
    keep <- keep & grepl(query, txt, ignore.case = TRUE)
  }
  reg[keep, , drop = FALSE]
}

#' Return a one-measure process card
#' @param name Measure name.
#' @param registry Registry.
#' @return An object of class "eye_process_measure_card", stored as a named list, containing return a one-measure process card and associated metadata needed to interpret the result.
#' @export
process_measure_card <- function(name, registry = process_measure_registry()) {
  reg <- .ep09_as_df(registry); validate_process_measure_registry(reg)
  name <- as.character(name)
  if (length(name) != 1L || is.na(name) || !nzchar(name)) stop("name must be one non-empty measure name.", call. = FALSE)
  hit <- reg[reg$name == name, , drop = FALSE]
  if (nrow(hit) != 1L) stop("Expected exactly one registry entry for '", name, "'.", call. = FALSE)
  structure(as.list(hit[1L, , drop = FALSE]), class = "eye_process_measure_card")
}

#' Process-measure guardrail table
#' @param registry Registry.
#' @return An R object containing process-measure guardrail table. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
process_measure_guardrails <- function(registry = process_measure_registry()) {
  reg <- .ep09_as_df(registry); validate_process_measure_registry(reg)
  reg[c("name", "channel", "interpretation", "guardrail", "status")]
}

#' Process-measure coverage for an observed dataset
#' @param data Data frame.
#' @param registry Registry.
#' @return A data frame containing process-measure coverage for an observed dataset. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
process_measure_coverage <- function(data, registry = process_measure_registry()) {
  d <- .ep09_as_df(data); reg <- .ep09_as_df(registry); validate_process_measure_registry(reg)
  data.frame(
    name = reg$name,
    registered = TRUE,
    present = reg$name %in% names(d),
    nonmissing_fraction = vapply(reg$name, function(nm) {
      if (!nm %in% names(d) || !nrow(d)) return(NA_real_)
      mean(!is.na(d[[nm]]))
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Process-measure lineage table
#' @param measure Measure name.
#' @param inputs Required input variable names.
#' @param transformations Ordered transformation labels.
#' @param output_level Output aggregation level.
#' @return An object of class "eye_process_measure_lineage", stored as a named list, with components "measure", "inputs", "transformations", "output_level", "lineage_hash". It contains process-measure lineage table and associated metadata or diagnostics needed to interpret the result.
#' @export
process_measure_lineage <- function(measure, inputs, transformations = character(), output_level = NA_character_) {
  structure(list(
    measure = as.character(measure)[1L],
    inputs = unique(as.character(inputs)),
    transformations = as.character(transformations),
    output_level = as.character(output_level)[1L],
    lineage_hash = .ep09_hash_object(list(measure, inputs, transformations, output_level))
  ), class = "eye_process_measure_lineage")
}

#' List units used by registered process measures
#' @param registry Registry.
#' @return An R object containing list units used by registered process measures. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
process_measure_units <- function(registry = process_measure_registry()) {
  reg <- .ep09_as_df(registry)
  unique(reg[c("channel", "unit")])
}

#' Split-half reliability for a trial-level process measure
#'
#' @param data Long trial-level data.
#' @param person Participant column.
#' @param trial Trial column.
#' @param measure Measure column.
#' @param split Odd/even or repeated random split.
#' @param repetitions Number of random splits.
#' @param seed Seed.
#' @param aggregate_fun Within-half aggregation function.
#' @return A tabular R object containing split-half reliability for a trial-level process measure; rows represent analysis units and columns contain the returned quantities.
#' @export
split_half_process_reliability <- function(data, person, trial, measure,
                                           split = c("odd_even", "random"), repetitions = 100L,
                                           seed = 1L, aggregate_fun = mean) {
  split <- match.arg(split); d <- .ep09_as_df(data)
  .ep09_req_cols(d, c(person, trial, measure), "data")
  if (!is.function(aggregate_fun)) stop("aggregate_fun must be a function.", call. = FALSE)
  if (length(repetitions) != 1L || !is.finite(repetitions) || repetitions < 1 || repetitions != as.integer(repetitions))
    stop("repetitions must be a positive integer.", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  compute <- function(half) {
    d$.half <- half
    agg <- stats::aggregate(d[[measure]], by = list(person = d[[person]], half = d$.half),
                            FUN = function(z) {
                              tryCatch(aggregate_fun(z, na.rm = TRUE), error = function(e) aggregate_fun(z))
                            })
    a <- agg[agg$half == 1L, c("person", "x"), drop = FALSE]
    b <- agg[agg$half == 2L, c("person", "x"), drop = FALSE]
    m <- merge(a, b, by = "person", suffixes = c("_h1", "_h2"))
    r <- if (nrow(m) >= 3L) suppressWarnings(stats::cor(m$x_h1, m$x_h2, use = "complete.obs")) else NA_real_
    sb <- if (is.finite(r) && r > -1) 2 * r / (1 + r) else NA_real_
    c(raw_r = r, spearman_brown = sb, n_persons = nrow(m))
  }
  if (split == "odd_even") {
    half <- ifelse(as.integer(as.factor(d[[trial]])) %% 2L == 1L, 1L, 2L)
    z <- compute(half)
    return(data.frame(split = "odd_even", replication = 1L, raw_r = z[["raw_r"]],
                      spearman_brown = z[["spearman_brown"]], n_persons = z[["n_persons"]], stringsAsFactors = FALSE))
  }
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  rows <- lapply(seq_len(as.integer(repetitions)), function(r) {
    half <- integer(nrow(d))
    for (p in unique(d[[person]])) {
      idx <- which(d[[person]] == p); half[idx] <- sample(rep(1:2, length.out = length(idx)))
    }
    z <- compute(half)
    data.frame(split = "random", replication = r, raw_r = z[["raw_r"]],
               spearman_brown = z[["spearman_brown"]], n_persons = z[["n_persons"]], stringsAsFactors = FALSE)
  })
  .ep09_rbind_fill(rows)
}

.ep09_icc_a1_matrix <- function(Y) {
  Y <- as.matrix(Y); storage.mode(Y) <- "double"
  keep <- stats::complete.cases(Y); Y <- Y[keep, , drop = FALSE]
  n <- nrow(Y); k <- ncol(Y)
  if (n < 3L || k < 2L) return(c(icc = NA_real_, n = n, sessions = k, MSR = NA_real_, MSC = NA_real_, MSE = NA_real_))
  gm <- mean(Y); rowm <- rowMeans(Y); colm <- colMeans(Y)
  msr <- k * sum((rowm - gm)^2) / (n - 1)
  msc <- n * sum((colm - gm)^2) / (k - 1)
  resid <- Y - rowm - rep(colm, each = n) + gm
  mse <- sum(resid^2) / ((n - 1) * (k - 1))
  den <- msr + (k - 1) * mse + k * (msc - mse) / n
  icc <- if (den == 0) NA_real_ else (msr - mse) / den
  c(icc = icc, n = n, sessions = k, MSR = msr, MSC = msc, MSE = mse)
}

#' Absolute-agreement ICC(A,1) for repeated process measures
#' @param data Long data.
#' @param person Participant column.
#' @param session Session/repetition column.
#' @param measure Measure column.
#' @return A data frame containing absolute-agreement ICC(A,1) for repeated process measures. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
process_icc <- function(data, person, session, measure) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(person, session, measure), "data")
  agg <- stats::aggregate(d[[measure]], by = list(person = d[[person]], session = d[[session]]), FUN = .ep09_mean)
  persons <- unique(agg$person); sessions <- unique(agg$session)
  Y <- matrix(NA_real_, nrow = length(persons), ncol = length(sessions), dimnames = list(persons, sessions))
  Y[cbind(match(agg$person, persons), match(agg$session, sessions))] <- agg$x
  z <- .ep09_icc_a1_matrix(Y)
  data.frame(icc_a1 = z[["icc"]], n_persons = z[["n"]], n_sessions = z[["sessions"]],
             ms_person = z[["MSR"]], ms_session = z[["MSC"]], ms_error = z[["MSE"]], stringsAsFactors = FALSE)
}

#' Bland-Altman repeatability summary for two sessions
#' @param data Long data.
#' @param person Participant column.
#' @param session Session column containing exactly two selected sessions.
#' @param measure Measure column.
#' @param sessions Optional two session labels.
#' @return An object of class "eye_process_bland_altman", stored as a named list, with components "pairs", "summary", "sessions". It contains bland-Altman repeatability summary for two sessions and associated metadata or diagnostics needed to interpret the result.
#' @export
process_bland_altman <- function(data, person, session, measure, sessions = NULL) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(person, session, measure), "data")
  if (is.null(sessions)) sessions <- unique(d[[session]][!is.na(d[[session]])])
  if (length(sessions) != 2L || anyNA(sessions)) stop("Bland-Altman summary requires exactly two non-missing sessions.", call. = FALSE)
  d <- d[d[[session]] %in% sessions, , drop = FALSE]
  agg <- stats::aggregate(d[[measure]], by = list(person = d[[person]], session = d[[session]]), FUN = .ep09_mean)
  a <- agg[agg$session == sessions[[1L]], c("person", "x")]; b <- agg[agg$session == sessions[[2L]], c("person", "x")]
  m <- merge(a, b, by = "person", suffixes = c("_1", "_2")); diffv <- m$x_2 - m$x_1
  finite_diff <- diffv[is.finite(diffv)]
  bias <- if (length(finite_diff)) mean(finite_diff) else NA_real_
  sd_diff <- if (length(finite_diff) >= 2L) stats::sd(finite_diff) else NA_real_
  m$pair_mean <- rowMeans(m[c("x_1", "x_2")]); m$difference <- diffv
  structure(list(
    pairs = m,
    summary = data.frame(n = nrow(m), bias = bias, sd_difference = sd_diff,
                         loa_lower = bias - 1.96 * sd_diff, loa_upper = bias + 1.96 * sd_diff,
                         stringsAsFactors = FALSE),
    sessions = sessions
  ), class = "eye_process_bland_altman")
}

#' Test-retest process reliability profile
#' @param data Long data.
#' @param person,session,measure Column names.
#' @return An object of class "eye_process_reliability_profile", stored as a named list, with components "measure", "icc", "bland_altman", "caveat". It contains test-retest process reliability profile and associated metadata or diagnostics needed to interpret the result.
#' @export
process_reliability_profile <- function(data, person, session, measure) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(person, session, measure), "data")
  icc <- process_icc(d, person, session, measure)
  sessions <- unique(d[[session]][!is.na(d[[session]])])
  ba <- if (length(sessions) >= 2L) process_bland_altman(d, person, session, measure, sessions = sessions[1:2]) else NULL
  structure(list(
    measure = measure,
    icc = icc,
    bland_altman = ba,
    caveat = "Reliability is design- and population-dependent; high reliability does not establish construct validity."
  ), class = "eye_process_reliability_profile")
}

#' Pairwise temporal stability across sessions
#' @param data Long data.
#' @param person,session,measure Column names.
#' @param method Correlation method.
#' @return A logical value or vector indicating pairwise temporal stability across sessions.
#' @export
process_temporal_stability <- function(data, person, session, measure,
                                       method = c("pearson", "spearman")) {
  method <- match.arg(method); d <- .ep09_as_df(data)
  .ep09_req_cols(d, c(person, session, measure), "data")
  agg <- stats::aggregate(d[[measure]], by = list(person = d[[person]], session = d[[session]]), FUN = .ep09_mean)
  sess <- unique(agg$session[!is.na(agg$session)])
  if (length(sess) < 2L) return(data.frame())
  cmb <- utils::combn(sess, 2L, simplify = FALSE)
  .ep09_rbind_fill(lapply(cmb, function(s) {
    a <- agg[agg$session == s[[1L]], c("person", "x")]; b <- agg[agg$session == s[[2L]], c("person", "x")]
    m <- merge(a, b, by = "person")
    data.frame(session_1 = as.character(s[[1L]]), session_2 = as.character(s[[2L]]), n = nrow(m),
               correlation = if (nrow(m) >= 3L) suppressWarnings(stats::cor(m$x.x, m$x.y, method = method, use = "complete.obs")) else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

#' Bootstrap ICC reliability by resampling participants
#' @param data Long data.
#' @param person,session,measure Column names.
#' @param replications Bootstrap replications.
#' @param seed Seed.
#' @return A data frame containing bootstrap ICC reliability by resampling participants. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
bootstrap_process_reliability <- function(data, person, session, measure, replications = 500L, seed = 1L) {
  d <- .ep09_as_df(data); .ep09_req_cols(d, c(person, session, measure), "data")
  if (length(replications) != 1L || !is.finite(replications) || replications < 1 || replications != as.integer(replications))
    stop("replications must be a positive integer.", call. = FALSE)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  ids <- unique(d[[person]][!is.na(d[[person]])]); if (!length(ids)) stop("No non-missing participant identifiers are available.", call. = FALSE)
  set.seed(as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1))
  vals <- replicate(as.integer(replications), {
    samp <- sample(ids, length(ids), replace = TRUE)
    boot <- .ep09_rbind_fill(lapply(seq_along(samp), function(i) {
      z <- d[d[[person]] == samp[[i]], , drop = FALSE]
      z[[person]] <- paste0("B", i)
      z
    }))
    process_icc(boot, person, session, measure)$icc_a1[[1L]]
  })
  data.frame(
    estimate = process_icc(d, person, session, measure)$icc_a1[[1L]],
    bootstrap_median = stats::median(vals, na.rm = TRUE),
    lower = .ep09_quantile(vals, .025), upper = .ep09_quantile(vals, .975),
    replications = as.integer(replications), stringsAsFactors = FALSE
  )
}

#' @export
print.eye_process_measure_registry <- function(x, ...) {
  cat("eyeprocess process-measure registry\n")
  cat("  measures:", nrow(x), "\n")
  cat("  channels:", paste(sort(unique(x$channel)), collapse = ", "), "\n")
  invisible(x)
}

#' @export
print.eye_process_reliability_profile <- function(x, ...) {
  cat("eyeprocess process reliability profile\n")
  cat("  measure:", x$measure, "\n")
  cat("  ICC(A,1):", format(x$icc$icc_a1[[1L]], digits = 3), "\n")
  invisible(x)
}
