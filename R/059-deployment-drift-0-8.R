# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Post-deployment psychometric-biometric drift governance.

#' Specify a process-deployment drift audit
#'
#' @param baseline Baseline rule: first observed batch or an explicitly supplied reference batch.
#' @param difficulty_limit Absolute item-difficulty change triggering review.
#' @param discrimination_limit Absolute discrimination change triggering review.
#' @param gaze_validity_drop Maximum tolerated decrease in gaze validity.
#' @param luminance_limit Absolute luminance change triggering review.
#' @param relative_metric_quantile Quantile of absolute deltas used for process
#'   metrics without an externally meaningful absolute threshold.
#' @param min_batches Minimum number of batches per item for drift assessment.
#' @return An `eye_process_drift_spec` object.
#' @export
process_drift_spec <- function(
    baseline = c("first_batch", "reference_batch"),
    difficulty_limit = 0.40,
    discrimination_limit = 0.35,
    gaze_validity_drop = 0.10,
    luminance_limit = 25,
    relative_metric_quantile = 0.90,
    min_batches = 2L) {
  baseline <- match.arg(baseline)
  vals <- c(difficulty_limit, discrimination_limit, gaze_validity_drop, luminance_limit)
  if (any(!is.finite(vals)) || any(vals < 0)) stop("Drift limits must be finite and non-negative.", call. = FALSE)
  if (!is.finite(relative_metric_quantile) || relative_metric_quantile <= 0 || relative_metric_quantile >= 1)
    stop("relative_metric_quantile must be in (0,1).", call. = FALSE)
  min_batches <- as.integer(min_batches)
  if (min_batches < 2L) stop("min_batches must be at least 2.", call. = FALSE)
  structure(list(
    baseline = baseline,
    difficulty_limit = difficulty_limit,
    discrimination_limit = discrimination_limit,
    gaze_validity_drop = gaze_validity_drop,
    luminance_limit = luminance_limit,
    relative_metric_quantile = relative_metric_quantile,
    min_batches = min_batches,
    interpretation = "Drift flags indicate review needs; they do not establish item compromise or content leakage."
  ), class = "eye_process_drift_spec")
}

#' Print a process drift spec object
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
print.eye_process_drift_spec <- function(x, ...) {
  cat("<eye_process_drift_spec>\n")
  cat(" baseline:", x$baseline, "\n")
  cat(" difficulty limit:", x$difficulty_limit, "\n")
  cat(" discrimination limit:", x$discrimination_limit, "\n")
  cat(" gaze validity drop:", x$gaze_validity_drop, "\n")
  cat(" luminance limit:", x$luminance_limit, "\n")
  invisible(x)
}

.ep08_first_finite <- function(x) {
  x <- .ep08_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else x[1L]
}

.ep08_last_finite <- function(x) {
  x <- .ep08_num(x)
  x <- x[is.finite(x)]
  if (!length(x)) NA_real_ else x[length(x)]
}

.ep08_drift_baseline_value <- function(d, metric, spec, batch_col, reference_batch = NULL) {
  d <- d[order(d$batch_order), , drop = FALSE]
  if (spec$baseline == "first_batch") return(.ep08_first_finite(d[[metric]]))
  if (is.null(reference_batch)) stop("reference_batch is required when baseline='reference_batch'.", call. = FALSE)
  ref <- d[d[[batch_col]] %in% reference_batch, metric, drop = TRUE]
  .ep08_mean(ref)
}

#' Audit post-deployment psychometric and biometric drift
#'
#' @param data Item-by-batch or trial-level deployment data.
#' @param item Item identifier column.
#' @param batch Ordered deployment batch/date column.
#' @param metrics Numeric metrics to monitor.
#' @param spec Drift specification.
#' @param reference_batch Optional reference batch value(s).
#' @param aggregate_fun Aggregation function used when multiple rows occur within
#'   item x batch.
#' @export
audit_process_drift <- function(
    data, item = "item_id", batch = "deployment_batch",
    metrics = c("irt_difficulty", "irt_discrimination", "rt_ms", "dwell_ms",
                "pupil_bc", "valid_gaze_prop", "screen_luminance"),
    spec = process_drift_spec(), reference_batch = NULL,
    aggregate_fun = .ep08_mean) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(item, batch))
  if (!inherits(spec, "eye_process_drift_spec")) stop("spec must be process_drift_spec().", call. = FALSE)
  metrics <- intersect(metrics, names(data))
  metrics <- metrics[vapply(data[metrics], function(z) any(is.finite(.ep08_num(z))), logical(1))]
  if (length(metrics) < 1L) stop("No usable drift metrics were found.", call. = FALSE)

  d <- data[, c(item, batch, metrics), drop = FALSE]
  d[[item]] <- as.character(d[[item]])
  # Preserve dates/ordered labels by creating a numeric order only for sorting.
  batch_levels <- unique(d[[batch]])
  if (inherits(d[[batch]], c("Date", "POSIXct", "POSIXt"))) {
    d$.batch_order <- as.numeric(d[[batch]])
  } else if (is.numeric(d[[batch]])) {
    d$.batch_order <- .ep08_num(d[[batch]])
  } else {
    d$.batch_order <- match(d[[batch]], batch_levels)
  }
  for (m in metrics) d[[m]] <- .ep08_num(d[[m]])

  key <- interaction(d[[item]], d[[batch]], drop = TRUE, lex.order = TRUE, sep = "\r")
  ag_rows <- lapply(split(seq_len(nrow(d)), key), function(idx) {
    out <- data.frame(item_value = d[[item]][idx[1L]], batch_value = d[[batch]][idx[1L]],
                      batch_order = .ep08_mean(d$.batch_order[idx]), stringsAsFactors = FALSE)
    names(out)[1:2] <- c(item, batch)
    for (m in metrics) out[[m]] <- aggregate_fun(d[[m]][idx])
    out
  })
  ag <- do.call(rbind, ag_rows)
  rownames(ag) <- NULL

  item_ids <- unique(ag[[item]])
  rows <- lapply(item_ids, function(id) {
    z <- ag[ag[[item]] == id, , drop = FALSE]
    z <- z[order(z$batch_order), , drop = FALSE]
    out <- data.frame(item_value = id, n_batches = nrow(z), stringsAsFactors = FALSE)
    names(out)[1L] <- item
    for (m in metrics) {
      b <- .ep08_drift_baseline_value(z, m, spec, batch, reference_batch)
      latest <- .ep08_last_finite(z[[m]])
      out[[paste0(m, "_baseline")]] <- b
      out[[paste0(m, "_latest")]] <- latest
      out[[paste0(m, "_delta")]] <- latest - b
    }
    out
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL

  delta_col <- function(m) paste0(m, "_delta")
  flag <- function(m, rule) {
    dc <- delta_col(m)
    if (!dc %in% names(tab)) return(rep(FALSE, nrow(tab)))
    v <- tab[[dc]]
    is.finite(v) & rule(v)
  }
  tab$difficulty_drift_flag <- flag("irt_difficulty", function(v) abs(v) > spec$difficulty_limit)
  tab$discrimination_drift_flag <- flag("irt_discrimination", function(v) abs(v) > spec$discrimination_limit)
  tab$gaze_quality_drift_flag <- flag("valid_gaze_prop", function(v) v < -spec$gaze_validity_drop)
  tab$screen_luminance_drift_flag <- flag("screen_luminance", function(v) abs(v) > spec$luminance_limit)

  relative_metrics <- setdiff(metrics, c("irt_difficulty", "irt_discrimination", "valid_gaze_prop", "screen_luminance"))
  for (m in relative_metrics) {
    dc <- delta_col(m)
    vals <- abs(tab[[dc]])
    cutoff <- if (all(!is.finite(vals))) NA_real_ else
      stats::quantile(vals, spec$relative_metric_quantile, na.rm = TRUE, names = FALSE)
    tab[[paste0(m, "_drift_flag")]] <- is.finite(cutoff) & is.finite(vals) & vals > cutoff
  }

  insufficient <- tab$n_batches < spec$min_batches
  tab$insufficient_batches_flag <- insufficient
  flag_cols <- grep("_flag$", names(tab), value = TRUE)
  review_cols <- setdiff(flag_cols, "insufficient_batches_flag")
  tab$drift_review_count <- rowSums(tab[review_cols], na.rm = TRUE)
  tab$drift_status <- ifelse(
    tab$insufficient_batches_flag,
    "insufficient_batches_for_drift_review",
    ifelse(tab$drift_review_count > 0L, "review_item_or_stimulus_context", "no_review_flag")
  )

  structure(list(
    table = tab,
    trajectories = ag,
    item = item,
    batch = batch,
    metrics = metrics,
    spec = spec,
    reference_batch = reference_batch,
    flag_columns = flag_cols,
    caveat = paste(
      "Drift may reflect item exposure, stimulus redesign, device/luminance changes,",
      "population shift, or data quality. Flags do not prove compromise or leakage."
    )
  ), class = "eye_process_drift_audit")
}

#' Extract drift alerts
#' @export
#' @param x Object to process, inspect, compare, or plot.
process_drift_alerts <- function(x) {
  if (!inherits(x, "eye_process_drift_audit")) stop("x must be eye_process_drift_audit.", call. = FALSE)
  x$table[x$table$drift_status == "review_item_or_stimulus_context", , drop = FALSE]
}

#' Compare two deployment batches descriptively
#'
#' @param data Deployment data.
#' @param batch Batch column.
#' @param batch_a,batch_b Values to compare.
#' @param metrics Metrics to compare.
#' @param item Optional item identifier for item-matched differences.
#' @export
compare_deployment_batches <- function(data, batch = "deployment_batch", batch_a, batch_b,
                                       metrics = NULL, item = "item_id") {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, batch)
  if (is.null(metrics)) metrics <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], c(batch, item))
  metrics <- setdiff(intersect(metrics, names(data)), c(batch, item))
  metrics <- metrics[vapply(data[metrics], function(z) any(is.finite(.ep08_num(z))), logical(1))]
  if (!length(metrics)) stop("No usable numeric metrics were found for batch comparison.", call. = FALSE)
  a <- data[data[[batch]] %in% batch_a, , drop = FALSE]
  b <- data[data[[batch]] %in% batch_b, , drop = FALSE]
  if (!nrow(a) || !nrow(b)) stop("Both comparison batches must contain data.", call. = FALSE)
  if (item %in% names(data)) {
    agg <- function(d) stats::aggregate(d[metrics], by = list(item_id = as.character(d[[item]])), FUN = .ep08_mean)
    aa <- agg(a); bb <- agg(b)
    names(aa)[1L] <- item; names(bb)[1L] <- item
    m <- merge(aa, bb, by = item, suffixes = c("_a", "_b"))
    for (metric in metrics) {
      m[[paste0(metric, "_delta")]] <- m[[paste0(metric, "_b")]] - m[[paste0(metric, "_a")]]
    }
    return(m)
  }
  data.frame(
    metric = metrics,
    batch_a_mean = vapply(metrics, function(m) .ep08_mean(a[[m]]), numeric(1)),
    batch_b_mean = vapply(metrics, function(m) .ep08_mean(b[[m]]), numeric(1)),
    delta = vapply(metrics, function(m) .ep08_mean(b[[m]]) - .ep08_mean(a[[m]]), numeric(1)),
    stringsAsFactors = FALSE
  )
}

.ep08_grouped_drift <- function(data, group, ...) {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, group)
  vals <- unique(data[[group]][!is.na(data[[group]])])
  if (!length(vals)) stop("No non-missing grouping values are available for drift stratification.", call. = FALSE)
  out <- lapply(vals, function(v) {
    z <- audit_process_drift(data[data[[group]] == v, , drop = FALSE], ...)
    z$table[[group]] <- v
    z$table
  })
  do.call(rbind, out)
}

#' Drift audit stratified by device
#' @export
#' @param data Data frame containing the required process variables.
#' @param device Name of the column identifying device.
#' @param ... Additional arguments passed to the underlying method or helper.
drift_by_device <- function(data, device = "device_id", ...) .ep08_grouped_drift(data, device, ...)

#' Drift audit stratified by study site
#' @export
#' @param data Data frame containing the required process variables.
#' @param site Name of the column identifying site.
#' @param ... Additional arguments passed to the underlying method or helper.
drift_by_site <- function(data, site = "site_id", ...) .ep08_grouped_drift(data, site, ...)

#' Drift audit stratified by vendor
#' @export
#' @param data Data frame containing the required process variables.
#' @param vendor Name of the column identifying vendor.
#' @param ... Additional arguments passed to the underlying method or helper.
drift_by_vendor <- function(data, vendor = "vendor", ...) .ep08_grouped_drift(data, vendor, ...)

#' Drift audit stratified by stimulus version
#' @export
#' @param data Data frame containing the required process variables.
#' @param stimulus_version Name of the column identifying stimulus version.
#' @param ... Additional arguments passed to the underlying method or helper.
drift_by_stimulus_version <- function(data, stimulus_version = "stimulus_version", ...)
  .ep08_grouped_drift(data, stimulus_version, ...)
