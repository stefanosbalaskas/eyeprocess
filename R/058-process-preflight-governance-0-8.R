# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Process pre-flight, multivariate anomaly review, and presentation-accessibility
# governance. These APIs are deliberately non-diagnostic: they audit signal and
# process conditions; they do not infer cheating, diagnosis, motivation, or ability.

.ep08_as_df <- function(x, name = "data") {
  if (!is.data.frame(x)) {
    x <- tryCatch(as.data.frame(x), error = function(e) NULL)
  }
  if (is.null(x)) stop(name, " must be coercible to a data.frame.", call. = FALSE)
  x
}

.ep08_req_cols <- function(data, cols, name = "data") {
  miss <- setdiff(cols, names(data))
  if (length(miss)) {
    stop(name, " is missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.ep08_num <- function(x) suppressWarnings(as.numeric(x))

.ep08_mean <- function(x) {
  x <- .ep08_num(x)
  if (!length(x) || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

.ep08_sd <- function(x) {
  x <- .ep08_num(x)
  if (sum(is.finite(x)) < 2L) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

.ep08_z <- function(x) {
  x <- .ep08_num(x)
  sx <- .ep08_sd(x)
  if (all(!is.finite(x))) return(rep(NA_real_, length(x)))
  if (!is.finite(sx) || sx == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / sx
}

.ep08_scalar_chr <- function(x, name) {
  if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    stop(name, " must be a non-empty scalar string.", call. = FALSE)
  }
  as.character(x)
}

.ep08_bool <- function(x) {
  out <- as.logical(x)
  out[is.na(out)] <- FALSE
  out
}

.ep08_split_rows <- function(data, by) {
  .ep08_req_cols(data, by)
  if (!nrow(data)) stop("data must contain at least one row.", call. = FALSE)
  if (!length(by)) return(list(all = seq_len(nrow(data))))
  if (any(vapply(data[by], anyNA, logical(1)))) {
    stop("Grouping columns must not contain missing values; repair or explicitly label missing IDs before analysis.", call. = FALSE)
  }
  key <- interaction(data[by], drop = TRUE, lex.order = TRUE, sep = "\r")
  split(seq_len(nrow(data)), key, drop = TRUE)
}

.ep08_group_values <- function(data, rows, by) {
  if (!length(by)) return(data.frame(.group = "all", stringsAsFactors = FALSE))
  out <- data[rows[1L], by, drop = FALSE]
  rownames(out) <- NULL
  out
}

.ep08_rbind_fill <- function(xs) {
  xs <- Filter(function(z) is.data.frame(z) && nrow(z) >= 0L, xs)
  if (!length(xs)) return(data.frame())
  all_names <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(z) {
    for (nm in setdiff(all_names, names(z))) z[[nm]] <- NA
    z[, all_names, drop = FALSE]
  })
  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

#' Specify a biometric process pre-flight gate
#'
#' Defines transparent, review-oriented thresholds for incoming gaze/pupil data.
#' The specification is a data-quality governance object, not a behavioral or
#' clinical classifier.
#'
#' @param min_gaze_validity Minimum mean gaze-validity proportion.
#' @param min_pupil_validity Minimum mean pupil-validity proportion.
#' @param max_gaze_missingness Maximum mean gaze-missingness proportion.
#' @param max_pupil_missingness Maximum mean pupil-missingness proportion.
#' @param min_valid_trial_fraction Minimum fraction of trials meeting the
#'   trial-level gaze-validity threshold.
#' @param trial_gaze_validity_threshold Gaze-validity threshold used to count an
#'   acceptable trial.
#' @param min_rt_ms,max_rt_ms Plausible mean response-time bounds in milliseconds.
#' @param sampling_rate_tolerance Fractional deviation from the cohort median
#'   sampling rate that triggers review.
#' @param blink_quantile Cohort quantile used for an extreme blink-cluster flag.
#' @param caution_flags Number of flags yielding `use_with_caution`.
#' @param review_flags Number of flags yielding `review_or_exclude_from_biometric_models`.
#' @return An `eye_process_preflight_spec` object.
#' @export
process_preflight_spec <- function(
    min_gaze_validity = 0.80,
    min_pupil_validity = 0.70,
    max_gaze_missingness = 0.25,
    max_pupil_missingness = 0.30,
    min_valid_trial_fraction = 0.70,
    trial_gaze_validity_threshold = 0.75,
    min_rt_ms = 200,
    max_rt_ms = 10000,
    sampling_rate_tolerance = 0.20,
    blink_quantile = 0.95,
    caution_flags = 1L,
    review_flags = 2L) {
  props <- c(min_gaze_validity, min_pupil_validity, max_gaze_missingness,
             max_pupil_missingness, min_valid_trial_fraction,
             trial_gaze_validity_threshold, sampling_rate_tolerance, blink_quantile)
  if (any(!is.finite(props))) stop("All proportion/threshold values must be finite.", call. = FALSE)
  if (any(props[1:6] < 0 | props[1:6] > 1)) stop("Validity/missingness proportions must lie in [0, 1].", call. = FALSE)
  if (sampling_rate_tolerance < 0) stop("sampling_rate_tolerance must be non-negative.", call. = FALSE)
  if (blink_quantile <= 0 || blink_quantile >= 1) stop("blink_quantile must lie strictly between 0 and 1.", call. = FALSE)
  if (!is.finite(min_rt_ms) || !is.finite(max_rt_ms) || min_rt_ms >= max_rt_ms)
    stop("min_rt_ms must be smaller than max_rt_ms.", call. = FALSE)
  caution_flags <- as.integer(caution_flags)
  review_flags <- as.integer(review_flags)
  if (caution_flags < 1L || review_flags <= caution_flags)
    stop("Require 1 <= caution_flags < review_flags.", call. = FALSE)
  structure(list(
    min_gaze_validity = min_gaze_validity,
    min_pupil_validity = min_pupil_validity,
    max_gaze_missingness = max_gaze_missingness,
    max_pupil_missingness = max_pupil_missingness,
    min_valid_trial_fraction = min_valid_trial_fraction,
    trial_gaze_validity_threshold = trial_gaze_validity_threshold,
    min_rt_ms = min_rt_ms,
    max_rt_ms = max_rt_ms,
    sampling_rate_tolerance = sampling_rate_tolerance,
    blink_quantile = blink_quantile,
    caution_flags = caution_flags,
    review_flags = review_flags,
    interpretation = paste(
      "Pre-flight thresholds protect downstream biometric/process analyses.",
      "Flags indicate review needs, not behavioral, clinical, or ability labels."
    )
  ), class = "eye_process_preflight_spec")
}

#' Print a process preflight spec object
#' @return Invisibly returns the input object after printing its summary; the object's class and contents are unchanged.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
print.eye_process_preflight_spec <- function(x, ...) {
  cat("<eye_process_preflight_spec>\n")
  cat(" gaze validity >=", x$min_gaze_validity, "\n")
  cat(" pupil validity >=", x$min_pupil_validity, "\n")
  cat(" gaze missingness <=", x$max_gaze_missingness, "\n")
  cat(" pupil missingness <=", x$max_pupil_missingness, "\n")
  cat(" acceptable-trial fraction >=", x$min_valid_trial_fraction, "\n")
  cat(" decisions: 0 flags = pass; ", x$caution_flags, " flag(s) = caution; ",
      x$review_flags, "+ flags = review/exclude\n", sep = "")
  invisible(x)
}

.ep08_col_or_na <- function(data, name, default = NA_real_) {
  if (is.null(name) || !nzchar(name) || !name %in% names(data)) return(rep(default, nrow(data)))
  data[[name]]
}

#' Audit incoming biometric/process data before modelling
#'
#' Aggregates trial-level signal-quality indicators by person/recording (or other
#' supplied grouping columns) and returns review-oriented flags. No rows are
#' automatically deleted.
#'
#' @param data Trial- or recording-level data.
#' @param by Grouping columns, usually participant and optionally recording/session.
#' @param spec A `process_preflight_spec()` object.
#' @param valid_gaze_prop,valid_pupil_prop Column names for validity proportions.
#' @param missing_gaze,missing_pupil Column names for missingness indicators/proportions.
#' @param rt_ms Response-time column.
#' @param blink_cluster_count Blink-cluster count column.
#' @param sampling_rate_hz Sampling-rate column.
#' @return An `eye_biometric_preflight` object.
#' @export
audit_biometric_preflight <- function(
    data,
    by = c("person_id"),
    spec = process_preflight_spec(),
    valid_gaze_prop = "valid_gaze_prop",
    valid_pupil_prop = "valid_pupil_prop",
    missing_gaze = "missing_gaze",
    missing_pupil = "missing_pupil",
    rt_ms = "rt_ms",
    blink_cluster_count = "blink_cluster_count",
    sampling_rate_hz = "sampling_rate_hz") {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, by)
  if (!inherits(spec, "eye_process_preflight_spec"))
    stop("spec must be created by process_preflight_spec().", call. = FALSE)

  groups <- .ep08_split_rows(data, by)
  rows <- lapply(groups, function(idx) {
    d <- data[idx, , drop = FALSE]
    gv <- .ep08_num(.ep08_col_or_na(d, valid_gaze_prop))
    pv <- .ep08_num(.ep08_col_or_na(d, valid_pupil_prop))
    mg <- .ep08_col_or_na(d, missing_gaze)
    mp <- .ep08_col_or_na(d, missing_pupil)
    mg <- if (is.logical(mg)) as.numeric(mg) else .ep08_num(mg)
    mp <- if (is.logical(mp)) as.numeric(mp) else .ep08_num(mp)
    rt <- .ep08_num(.ep08_col_or_na(d, rt_ms))
    bl <- .ep08_num(.ep08_col_or_na(d, blink_cluster_count))
    sr <- .ep08_num(.ep08_col_or_na(d, sampling_rate_hz))
    acceptable <- is.finite(gv) & gv >= spec$trial_gaze_validity_threshold
    cbind(
      .ep08_group_values(data, idx, by),
      data.frame(
        n_rows = length(idx),
        mean_valid_gaze_prop = .ep08_mean(gv),
        mean_valid_pupil_prop = .ep08_mean(pv),
        mean_missing_gaze = .ep08_mean(mg),
        mean_missing_pupil = .ep08_mean(mp),
        mean_rt_ms = .ep08_mean(rt),
        mean_blink_cluster_count = .ep08_mean(bl),
        mean_sampling_rate_hz = .ep08_mean(sr),
        proportion_trials_with_acceptable_gaze = if (any(is.finite(gv))) mean(acceptable[is.finite(gv)]) else NA_real_,
        stringsAsFactors = FALSE
      )
    )
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL

  blink_cutoff <- if (all(!is.finite(tab$mean_blink_cluster_count))) NA_real_ else
    stats::quantile(tab$mean_blink_cluster_count, spec$blink_quantile, na.rm = TRUE, names = FALSE)
  target_sampling_rate <- if (all(!is.finite(tab$mean_sampling_rate_hz))) NA_real_ else
    stats::median(tab$mean_sampling_rate_hz, na.rm = TRUE)

  tab$low_valid_gaze_flag <- is.finite(tab$mean_valid_gaze_prop) & tab$mean_valid_gaze_prop < spec$min_gaze_validity
  tab$low_valid_pupil_flag <- is.finite(tab$mean_valid_pupil_prop) & tab$mean_valid_pupil_prop < spec$min_pupil_validity
  tab$excessive_gaze_missingness_flag <- is.finite(tab$mean_missing_gaze) & tab$mean_missing_gaze > spec$max_gaze_missingness
  tab$excessive_pupil_missingness_flag <- is.finite(tab$mean_missing_pupil) & tab$mean_missing_pupil > spec$max_pupil_missingness
  tab$implausible_rt_flag <- is.finite(tab$mean_rt_ms) & (tab$mean_rt_ms < spec$min_rt_ms | tab$mean_rt_ms > spec$max_rt_ms)
  tab$too_few_valid_trials_flag <- is.finite(tab$proportion_trials_with_acceptable_gaze) &
    tab$proportion_trials_with_acceptable_gaze < spec$min_valid_trial_fraction
  tab$extreme_blink_cluster_flag <- is.finite(blink_cutoff) & is.finite(tab$mean_blink_cluster_count) &
    tab$mean_blink_cluster_count > blink_cutoff
  tab$sampling_rate_instability_flag <- is.finite(target_sampling_rate) & target_sampling_rate > 0 &
    is.finite(tab$mean_sampling_rate_hz) &
    abs(tab$mean_sampling_rate_hz - target_sampling_rate) > spec$sampling_rate_tolerance * target_sampling_rate

  flag_cols <- grep("_flag$", names(tab), value = TRUE)
  tab$preflight_flag_count <- rowSums(tab[flag_cols], na.rm = TRUE)
  tab$preflight_decision <- ifelse(
    tab$preflight_flag_count >= spec$review_flags,
    "review_or_exclude_from_biometric_models",
    ifelse(tab$preflight_flag_count >= spec$caution_flags, "use_with_caution", "pass_preflight")
  )

  structure(list(
    table = tab,
    flag_columns = flag_cols,
    by = by,
    spec = spec,
    blink_cutoff = blink_cutoff,
    target_sampling_rate_hz = target_sampling_rate,
    source_n = nrow(data),
    interpretation = paste(
      "Pre-flight decisions are quality-governance recommendations.",
      "They must not be interpreted as evidence about motivation, cheating, diagnosis, or ability."
    )
  ), class = "eye_biometric_preflight")
}

#' Extract pre-flight decisions
#' @param x An `eye_biometric_preflight` object.
#' @return An R object containing pre-flight decisions. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
preflight_decisions <- function(x) {
  if (!inherits(x, "eye_biometric_preflight")) stop("x must be an eye_biometric_preflight object.", call. = FALSE)
  x$table
}

#' Extract pre-flight failures/review cases
#' @return A tabular R object containing pre-flight failures/review cases; rows represent analysis units and columns contain the returned quantities.
#' @export
#' @param x Object to process, inspect, compare, or plot.
preflight_failures <- function(x) {
  tab <- preflight_decisions(x)
  tab[tab$preflight_decision != "pass_preflight", , drop = FALSE]
}

#' Extract pre-flight passes
#' @return A tabular R object containing pre-flight passes; rows represent analysis units and columns contain the returned quantities.
#' @export
#' @param x Object to process, inspect, compare, or plot.
preflight_passed <- function(x) {
  tab <- preflight_decisions(x)
  tab[tab$preflight_decision == "pass_preflight", , drop = FALSE]
}

#' Create an explicit pre-flight exclusion/review manifest
#'
#' The manifest records recommendations only. It does not remove observations.
#' @return A tabular R object containing an explicit pre-flight exclusion/review manifest; rows represent analysis units and columns contain the returned quantities.
#' @export
#' @param x Object to process, inspect, compare, or plot.
preflight_exclusion_manifest <- function(x) {
  tab <- preflight_decisions(x)
  keep <- c(x$by, x$flag_columns, "preflight_flag_count", "preflight_decision")
  out <- tab[, keep, drop = FALSE]
  out$recommended_action <- ifelse(
    out$preflight_decision == "review_or_exclude_from_biometric_models",
    "manual_review_before_biometric_model_inclusion",
    ifelse(out$preflight_decision == "use_with_caution", "retain_with_sensitivity_analysis", "retain")
  )
  out
}

#' Apply a pre-flight decision to data explicitly
#'
#' @param data Original data.
#' @param audit Pre-flight audit.
#' @param keep_decisions Decisions to retain.
#' @return Filtered data with an attached `preflight_application` attribute.
#' @export
apply_preflight_decision <- function(
    data, audit,
    keep_decisions = c("pass_preflight", "use_with_caution")) {
  data <- .ep08_as_df(data)
  if (!inherits(audit, "eye_biometric_preflight")) stop("audit must be eye_biometric_preflight.", call. = FALSE)
  .ep08_req_cols(data, audit$by)
  if (any(vapply(data[audit$by], anyNA, logical(1)))) {
    stop("Grouping columns used by the pre-flight audit must not contain missing values.", call. = FALSE)
  }
  tab <- audit$table[, c(audit$by, "preflight_decision"), drop = FALSE]
  data$.ep08_original_row <- seq_len(nrow(data))
  merged <- merge(data, tab, by = audit$by, all.x = TRUE, sort = FALSE)
  merged <- merged[order(merged$.ep08_original_row), , drop = FALSE]
  if (anyNA(merged$preflight_decision)) {
    stop("Some rows could not be matched to a pre-flight decision; ensure the grouping identifiers match the audited data.", call. = FALSE)
  }
  out <- merged[merged$preflight_decision %in% keep_decisions, , drop = FALSE]
  out$.ep08_original_row <- NULL
  attr(out, "preflight_application") <- list(
    keep_decisions = keep_decisions,
    n_input = nrow(data),
    n_output = nrow(out),
    caution = "Rows were filtered only because apply_preflight_decision() was explicitly called."
  )
  out
}

#' Audit multivariate process/data-quality anomalies
#'
#' Computes a regularized Mahalanobis distance over selected person-level process
#' metrics. Flags indicate review needs only; they are not cheating, identity,
#' diagnosis, or intent classifications.
#'
#' @param data Data frame.
#' @param person Person identifier column.
#' @param metrics Numeric process metrics. If omitted, usable numeric columns are selected.
#' @param alpha Chi-square review quantile.
#' @param aggregate If TRUE, aggregate metrics to person level before auditing.
#' @param ridge Diagonal covariance regularization.
#' @return An object of class "eye_process_anomaly_audit", stored as a named list, with components "table", "metrics", "alpha", "threshold", "center", "covariance", "caveat". It contains multivariate process/data-quality anomalies and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_process_anomalies <- function(data, person = "person_id", metrics = NULL,
                                    alpha = 0.975, aggregate = TRUE, ridge = 1e-6) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, person)
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1).", call. = FALSE)
  if (is.null(metrics)) {
    candidates <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], person)
    metrics <- candidates[vapply(data[candidates], function(z) {
      sz <- .ep08_sd(z)
      sum(is.finite(.ep08_num(z))) >= 10L && is.finite(sz) && sz > 0
    }, logical(1))]
  }
  metrics <- intersect(metrics, names(data))
  if (length(metrics) < 2L) stop("At least two usable numeric metrics are required.", call. = FALSE)
  d <- data[, c(person, metrics), drop = FALSE]
  for (m in metrics) d[[m]] <- .ep08_num(d[[m]])
  if (isTRUE(aggregate)) {
    d <- stats::aggregate(d[metrics], by = list(person_id = as.character(d[[person]])), FUN = .ep08_mean)
    names(d)[1L] <- person
  }
  X <- as.matrix(d[metrics])
  for (j in seq_len(ncol(X))) {
    mu <- mean(X[, j], na.rm = TRUE)
    if (!is.finite(mu)) mu <- 0
    X[!is.finite(X[, j]), j] <- mu
  }
  center <- colMeans(X)
  V <- stats::cov(X)
  if (!is.matrix(V) || any(!is.finite(V))) stop("Could not estimate a finite covariance matrix.", call. = FALSE)
  V <- V + diag(ridge, ncol(V))
  dist <- stats::mahalanobis(X, center = center, cov = V)
  threshold <- stats::qchisq(alpha, df = ncol(X))
  d$mahalanobis_process_distance <- as.numeric(dist)
  d$review_threshold <- threshold
  d$review_required <- d$mahalanobis_process_distance > threshold
  d$review_label <- ifelse(d$review_required,
                           "review_required_process_or_data_quality_anomaly",
                           "no_multivariate_process_flag")
  d <- d[order(-d$mahalanobis_process_distance), , drop = FALSE]
  structure(list(
    table = d, metrics = metrics, alpha = alpha, threshold = threshold,
    center = center, covariance = V,
    caveat = paste(
      "Multivariate distance is a process/data-quality review statistic.",
      "It is not evidence of cheating, spoofing, diagnosis, motivation, or intent."
    )
  ), class = "eye_process_anomaly_audit")
}

#' Alias emphasizing data-quality interpretation of process anomaly auditing
#' @return An R object containing alias emphasizing data-quality interpretation of process anomaly auditing. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
audit_multivariate_process_quality <- function(...) audit_process_anomalies(...)

#' Extract multivariate process anomaly distances
#' @return An R object containing multivariate process anomaly distances. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
process_anomaly_distance <- function(x) {
  if (!inherits(x, "eye_process_anomaly_audit")) stop("x must be an eye_process_anomaly_audit object.", call. = FALSE)
  x$table[, c(setdiff(names(x$table), x$metrics), x$metrics), drop = FALSE]
}

#' Audit presentation/accessibility sensitivity without clinical inference
#'
#' Creates a transparent presentation-review score from response time/dwell,
#' revisit/entropy, pupil-effort proxies, and gaze quality. It is an experimental
#' design/fairness audit only.
#'
#' @param data Process data.
#' @param person Person identifier.
#' @param rt,dwell,revisits,entropy,pupil,gaze_validity Optional column names.
#' @param review_quantile Quantile for a presentation-review flag.
#' @return An object of class "eye_presentation_accessibility", stored as a named list, with components "table", "threshold", "review_quantile", "status", "caveat". It contains presentation/accessibility sensitivity without clinical inference and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_presentation_accessibility <- function(
    data, person = "person_id", rt = "rt_ms", dwell = "dwell_ms",
    revisits = "revisits", entropy = "aoi_entropy", pupil = "pupil_peak",
    gaze_validity = "valid_gaze_prop", review_quantile = 0.90) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, person)
  if (!is.finite(review_quantile) || review_quantile <= 0 || review_quantile >= 1)
    stop("review_quantile must be in (0,1).", call. = FALSE)
  cols <- intersect(c(rt, dwell, revisits, entropy, pupil, gaze_validity), names(data))
  if (length(cols) < 3L) stop("At least three process columns are required.", call. = FALSE)
  tmp <- data[, c(person, cols), drop = FALSE]
  for (cc in cols) tmp[[cc]] <- .ep08_num(tmp[[cc]])
  agg <- stats::aggregate(tmp[cols], by = list(person_id = as.character(tmp[[person]])), FUN = .ep08_mean)
  names(agg)[1L] <- person
  getv <- function(name) if (!is.null(name) && name %in% names(agg)) agg[[name]] else rep(NA_real_, nrow(agg))
  reading <- .ep08_z(getv(rt)) + .ep08_z(getv(dwell)) + .ep08_z(getv(revisits))
  search <- .ep08_z(getv(entropy)) - .ep08_z(getv(gaze_validity))
  phys <- .ep08_z(getv(pupil))
  comp <- cbind(reading, search, phys)
  score <- apply(comp, 1L, function(z) if (all(!is.finite(z))) NA_real_ else mean(z[is.finite(z)]))
  if (!any(is.finite(score))) stop("No finite presentation-sensitivity scores could be computed.", call. = FALSE)
  threshold <- stats::quantile(score, review_quantile, na.rm = TRUE, names = FALSE)
  agg$reading_effort_proxy <- reading
  agg$visual_search_instability_proxy <- search
  agg$physiological_load_proxy <- phys
  agg$presentation_sensitivity_index <- score
  agg$presentation_review_flag <- is.finite(score) & score >= threshold
  agg$interpretation_label <- ifelse(
    agg$presentation_review_flag,
    "presentation_accessibility_review_not_clinical_diagnosis",
    "no_presentation_review_flag"
  )
  structure(list(
    table = agg, threshold = threshold, review_quantile = review_quantile,
    status = "experimental_design_audit",
    caveat = paste(
      "This audit evaluates presentation/accessibility sensitivity only.",
      "It must not be used to infer ADHD, dyslexia, visual impairment, neurodivergence, or another diagnosis."
    )
  ), class = "eye_presentation_accessibility")
}

#' Simulate pre-registered presentation variants for review
#'
#' @param audit An accessibility audit.
#' @param line_spacing_multiplier Example line-spacing multiplier for flagged rows.
#' @param key_term_highlighting Whether the simulated review variant highlights key terms.
#' @return An R object containing pre-registered presentation variants for review. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
simulate_presentation_variants <- function(
    audit, line_spacing_multiplier = 1.25, key_term_highlighting = TRUE) {
  if (!inherits(audit, "eye_presentation_accessibility"))
    stop("audit must be created by audit_presentation_accessibility().", call. = FALSE)
  tab <- audit$table
  tab$simulated_variant <- ifelse(tab$presentation_review_flag,
                                  "calibrated_accessibility_variant_for_review",
                                  "standard_presentation")
  tab$simulated_line_spacing_multiplier <- ifelse(tab$presentation_review_flag,
                                                   line_spacing_multiplier, 1)
  tab$simulated_key_term_highlighting <- tab$presentation_review_flag & isTRUE(key_term_highlighting)
  tab$operational_status <- "simulation_only_requires_calibration_and_fairness_testing"
  tab
}

#' Compare outcomes across presentation variants
#'
#' @param data Data containing presentation version and an outcome.
#' @param variant Presentation-version column.
#' @param outcome Numeric outcome.
#' @param person Optional participant column for descriptive aggregation.
#' @return An object of class "eye_presentation_fairness_comparison", stored as a named list, with components "model", "summary", "status", "caveat". It contains outcomes across presentation variants and associated metadata or diagnostics needed to interpret the result.
#' @export
compare_presentation_fairness <- function(data, variant, outcome, person = NULL) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(variant, outcome))
  y <- .ep08_num(data[[outcome]])
  v <- as.factor(data[[variant]])
  ok <- is.finite(y) & !is.na(v)
  d <- data.frame(outcome = y[ok], variant = droplevels(v[ok]), stringsAsFactors = FALSE)
  if (!is.null(person)) {
    .ep08_req_cols(data, person)
    d[[person]] <- as.character(data[[person]][ok])
    d <- stats::aggregate(d["outcome"], by = list(person_id = d[[person]], variant = d$variant), FUN = .ep08_mean)
    names(d)[1L] <- person
    d$variant <- as.factor(d$variant)
  }
  if (nlevels(d$variant) < 2L) stop("At least two presentation variants are required.", call. = FALSE)
  model <- stats::lm(outcome ~ variant, data = d)
  summary_tab <- stats::aggregate(outcome ~ variant, data = d,
                                  FUN = function(z) c(n = length(z), mean = mean(z), sd = stats::sd(z)))
  structure(list(
    model = model, summary = summary_tab,
    status = "descriptive_fairness_sensitivity",
    caveat = "Presentation differences require calibrated designs and substantive fairness interpretation; this comparison is not causal by itself."
  ), class = "eye_presentation_fairness_comparison")
}
