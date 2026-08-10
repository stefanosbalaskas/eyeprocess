# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Defensible multiverse/sensitivity analysis and decision stability.

#' Construct an explicit process-analysis sensitivity grid
#'
#' @param ... Named vectors of defensible analysis options.
#' @param label Grid label.
#' @param max_specifications Safety cap.
#' @export
process_sensitivity_grid <- function(..., label = "process_sensitivity", max_specifications = 100000L) {
  opts <- list(...)
  if (!length(opts) || is.null(names(opts)) || any(is.na(names(opts))) || any(!nzchar(names(opts))) || anyDuplicated(names(opts)))
    stop("Supply one or more uniquely named analysis-decision vectors.", call. = FALSE)
  if (any(lengths(opts) == 0L)) stop("Every analysis-decision vector must contain at least one option.", call. = FALSE)
  if (!is.numeric(max_specifications) || length(max_specifications) != 1L || is.na(max_specifications) || max_specifications < 0)
    stop("max_specifications must be a non-negative scalar or Inf.", call. = FALSE)
  g <- expand.grid(opts, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (nrow(g) > max_specifications) stop("Sensitivity grid expands to ", nrow(g), " specifications; increase max_specifications deliberately if intended.", call. = FALSE)
  g$specification_id <- sprintf("S%05d", seq_len(nrow(g)))
  attr(g, "label") <- as.character(label)[1L]
  class(g) <- c("eye_process_sensitivity_grid", "data.frame")
  g[, c("specification_id", setdiff(names(g), "specification_id")), drop = FALSE]
}

.ep09_default_sensitivity_extract <- function(fit, specification) {
  if (is.numeric(fit) && length(fit) == 1L) return(data.frame(effect = as.numeric(fit), stringsAsFactors = FALSE))
  if (is.data.frame(fit)) return(fit)
  if (is.list(fit)) {
    keep <- vapply(fit, function(z) length(z) == 1L && (is.atomic(z) || is.factor(z)), logical(1))
    if (any(keep)) return(as.data.frame(fit[keep], stringsAsFactors = FALSE))
  }
  stop("Provide extract_fun for analysis results that are not scalar/list/data.frame summaries.", call. = FALSE)
}

#' Run an explicit process-analysis multiverse
#'
#' @param data Analysis data.
#' @param grid Sensitivity grid.
#' @param analysis_fun Function `(data, specification)`.
#' @param extract_fun Function `(fit, specification)` returning one or more rows.
#' @param progress Print progress.
#' @export
run_process_sensitivity <- function(data, grid, analysis_fun, extract_fun = .ep09_default_sensitivity_extract,
                                    progress = interactive()) {
  if (!is.function(analysis_fun)) stop("analysis_fun must be a function.", call. = FALSE)
  if (!is.function(extract_fun)) stop("extract_fun must be a function.", call. = FALSE)
  grid <- .ep09_as_df(grid)
  .ep09_req_cols(grid, "specification_id", "grid")
  rows <- list(); failures <- list(); warnings <- list(); k <- 0L; fk <- 0L; wk <- 0L
  decision_cols <- setdiff(names(grid), "specification_id")
  for (i in seq_len(nrow(grid))) {
    spec <- grid[i, , drop = FALSE]
    if (isTRUE(progress)) message("sensitivity ", spec$specification_id[[1L]], " (", i, "/", nrow(grid), ")")
    cap <- .ep09_capture(analysis_fun(data, spec))
    if (length(cap$warnings)) {
      wk <- wk + 1L
      warnings[[wk]] <- data.frame(specification_id = spec$specification_id[[1L]], stage = "analysis",
                                   warning = paste(cap$warnings, collapse = " | "), stringsAsFactors = FALSE)
    }
    if (!is.na(cap$error)) {
      fk <- fk + 1L
      failures[[fk]] <- data.frame(specification_id = spec$specification_id[[1L]], error = cap$error, stringsAsFactors = FALSE)
      next
    }
    ext <- .ep09_capture(extract_fun(cap$value, spec))
    if (length(ext$warnings)) {
      wk <- wk + 1L
      warnings[[wk]] <- data.frame(specification_id = spec$specification_id[[1L]], stage = "extract",
                                   warning = paste(ext$warnings, collapse = " | "), stringsAsFactors = FALSE)
    }
    if (!is.na(ext$error)) {
      fk <- fk + 1L
      failures[[fk]] <- data.frame(specification_id = spec$specification_id[[1L]], error = ext$error, stringsAsFactors = FALSE)
      next
    }
    tab <- .ep09_as_df(ext$value)
    if (!nrow(tab)) next
    tab$specification_id <- spec$specification_id[[1L]]
    for (nm in decision_cols) tab[[nm]] <- spec[[nm]][[1L]]
    if (length(cap$warnings)) tab$warnings <- paste(cap$warnings, collapse = " | ")
    k <- k + 1L; rows[[k]] <- tab
  }
  out <- structure(list(
    grid = grid,
    results = .ep09_rbind_fill(rows),
    failures = .ep09_rbind_fill(failures),
    warnings = .ep09_rbind_fill(warnings),
    grid_hash = .ep09_hash_object(grid),
    created_at = as.character(Sys.time()),
    status = "defensible_multiverse",
    caveat = "Sensitivity results describe the supplied defensible specification set; they do not validate specifications omitted from that set."
  ), class = "eye_process_sensitivity")
  out
}

#' Summarise process sensitivity results
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @param p_value Optional p-value column.
#' @param threshold Optional substantive effect threshold.
#' @param alpha Significance threshold used only when `p_value` is supplied.
#' @export
summarise_process_sensitivity <- function(x, effect = "effect", p_value = NULL,
                                          threshold = 0, alpha = .05) {
  if (!inherits(x, "eye_process_sensitivity")) stop("x must be an eye_process_sensitivity.", call. = FALSE)
  d <- x$results
  if (!nrow(d)) return(data.frame())
  .ep09_req_cols(d, effect, "x$results")
  e <- .ep09_num(d[[effect]])
  ef <- e[is.finite(e)]
  out <- data.frame(
    specifications = nrow(d),
    failures = nrow(x$failures),
    median_effect = if (length(ef)) stats::median(ef) else NA_real_,
    mean_effect = if (length(ef)) mean(ef) else NA_real_,
    min_effect = if (length(ef)) min(ef) else NA_real_,
    max_effect = if (length(ef)) max(ef) else NA_real_,
    sign_stability = sensitivity_sign_stability(x, effect),
    threshold_stability = sensitivity_threshold_stability(x, effect, threshold),
    stringsAsFactors = FALSE
  )
  if (!is.null(p_value)) out$significance_stability <- sensitivity_significance_stability(x, p_value, alpha)
  out
}

#' Effect-sign stability across specifications
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @export
sensitivity_sign_stability <- function(x, effect = "effect") {
  d <- x$results; .ep09_req_cols(d, effect, "x$results")
  e <- .ep09_num(d[[effect]]); e <- e[is.finite(e) & e != 0]
  if (!length(e)) return(NA_real_)
  max(mean(e > 0), mean(e < 0))
}

#' Significance-decision stability across specifications
#' @param x Sensitivity result.
#' @param p_value P-value column.
#' @param alpha Decision threshold.
#' @export
sensitivity_significance_stability <- function(x, p_value = "p_value", alpha = .05) {
  if (length(alpha) != 1L || !is.finite(alpha) || alpha < 0 || alpha > 1) stop("alpha must lie in [0, 1].", call. = FALSE)
  d <- x$results; .ep09_req_cols(d, p_value, "x$results")
  p <- .ep09_num(d[[p_value]]); p <- p[is.finite(p)]
  if (!length(p)) return(NA_real_)
  max(mean(p < alpha), mean(p >= alpha))
}

#' Substantive-threshold stability across specifications
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @param threshold Threshold.
#' @param direction `above`, `below`, or `absolute`.
#' @export
sensitivity_threshold_stability <- function(x, effect = "effect", threshold = 0,
                                            direction = c("above", "below", "absolute")) {
  direction <- match.arg(direction)
  if (length(threshold) != 1L || !is.finite(threshold)) stop("threshold must be a finite scalar.", call. = FALSE)
  d <- x$results; .ep09_req_cols(d, effect, "x$results")
  e <- .ep09_num(d[[effect]]); e <- e[is.finite(e)]
  if (!length(e)) return(NA_real_)
  dec <- switch(direction, above = e >= threshold, below = e <= threshold, absolute = abs(e) >= abs(threshold))
  max(mean(dec), mean(!dec))
}

#' Overall decision-stability summary
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @param p_value Optional p-value column.
#' @param alpha Significance threshold.
#' @param threshold Substantive threshold.
#' @export
decision_stability <- function(x, effect = "effect", p_value = NULL, alpha = .05, threshold = 0) {
  s <- summarise_process_sensitivity(x, effect = effect, p_value = p_value, threshold = threshold, alpha = alpha)
  if (!nrow(s)) {
    return(structure(list(
      summary = s, stable_sign = NA, stable_threshold = NA, stable_significance = NA,
      thresholds = list(sign = .90, threshold = .90, significance = .90),
      caveat = "No successful specifications were available; stability is undefined."
    ), class = "eye_decision_stability"))
  }
  structure(list(
    summary = s,
    stable_sign = isTRUE(s$sign_stability[[1L]] >= .90),
    stable_threshold = isTRUE(s$threshold_stability[[1L]] >= .90),
    stable_significance = if (is.null(p_value)) NA else isTRUE(s$significance_stability[[1L]] >= .90),
    thresholds = list(sign = .90, threshold = .90, significance = .90),
    caveat = "Stability thresholds are reporting conventions, not universal validity cutoffs."
  ), class = "eye_decision_stability")
}

#' Prepare ordered specification-curve data
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @param lower Optional lower interval column.
#' @param upper Optional upper interval column.
#' @export
specification_curve_data <- function(x, effect = "effect", lower = NULL, upper = NULL) {
  d <- x$results; .ep09_req_cols(d, effect, "x$results")
  d$.effect <- .ep09_num(d[[effect]])
  d <- d[order(d$.effect), , drop = FALSE]
  d$curve_order <- seq_len(nrow(d))
  if (!is.null(lower) && lower %in% names(d)) d$.lower <- .ep09_num(d[[lower]])
  if (!is.null(upper) && upper %in% names(d)) d$.upper <- .ep09_num(d[[upper]])
  d
}

#' Fraction of planned specifications successfully evaluated
#' @param x Sensitivity result.
#' @export
specification_coverage <- function(x) {
  if (!inherits(x, "eye_process_sensitivity")) stop("x must be an eye_process_sensitivity.", call. = FALSE)
  if (!nrow(x$grid)) return(NA_real_)
  length(unique(x$results$specification_id)) / nrow(x$grid)
}

#' Decision leverage of each analytical choice
#'
#' Leverage is descriptive variation in mean effect across option levels; it is
#' not causal attribution of researcher decisions.
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @export
sensitivity_decision_leverage <- function(x, effect = "effect") {
  d <- x$results; .ep09_req_cols(d, c("specification_id", effect), "x$results")
  decision_cols <- intersect(setdiff(names(x$grid), "specification_id"), names(d))
  rows <- lapply(decision_cols, function(nm) {
    means <- tapply(.ep09_num(d[[effect]]), d[[nm]], mean, na.rm = TRUE)
    data.frame(
      decision = nm,
      levels = length(means),
      effect_range = { mf <- .ep09_num(means); mf <- mf[is.finite(mf)]; if (length(mf) > 1L) diff(range(mf)) else if (length(mf) == 1L) 0 else NA_real_ },
      effect_sd_across_levels = { mf <- .ep09_num(means); mf <- mf[is.finite(mf)]; if (length(mf) > 1L) stats::sd(mf) else if (length(mf) == 1L) 0 else NA_real_ },
      stringsAsFactors = FALSE
    )
  })
  out <- .ep09_rbind_fill(rows)
  out[order(-out$effect_range), , drop = FALSE]
}

#' Fragility index across analysis specifications
#' @param x Sensitivity result.
#' @param effect Effect column.
#' @param threshold Decision threshold.
#' @export
sensitivity_fragility_index <- function(x, effect = "effect", threshold = 0) {
  d <- x$results; .ep09_req_cols(d, effect, "x$results")
  e <- .ep09_num(d[[effect]]); e <- e[is.finite(e)]
  if (!length(e)) return(NA_real_)
  majority <- mean(e >= threshold) >= .5
  mean((e >= threshold) != majority)
}

#' Rank stability across specifications
#' @param x Data frame or list of ranking vectors.
#' @param id Optional item identifier when x is a long data frame.
#' @param rank Optional rank/value column when x is a long data frame.
#' @param specification Optional specification column.
#' @export
sensitivity_rank_stability <- function(x, id = NULL, rank = NULL, specification = NULL) {
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) < 2L) return(NA_real_)
    if (length(unique(lengths(x))) != 1L) stop("Ranking vectors supplied as a list must have equal length.", call. = FALSE)
    ranks <- lapply(x, base::rank, ties.method = "average", na.last = "keep")
    cmb <- utils::combn(seq_along(ranks), 2L)
    cors <- apply(cmb, 2L, function(ii) suppressWarnings(stats::cor(ranks[[ii[1L]]], ranks[[ii[2L]]], method = "spearman", use = "pairwise.complete.obs")))
    cors <- cors[is.finite(cors)]
    return(if (length(cors)) mean(cors) else NA_real_)
  }
  d <- .ep09_as_df(x); .ep09_req_cols(d, c(id, rank, specification), "x")
  wide <- split(d, d[[specification]])
  common <- Reduce(intersect, lapply(wide, function(z) as.character(z[[id]])))
  if (length(common) < 2L || length(wide) < 2L) return(NA_real_)
  rv <- lapply(wide, function(z) {
    z <- z[match(common, as.character(z[[id]])), , drop = FALSE]
    .ep09_num(z[[rank]])
  })
  sensitivity_rank_stability(rv)
}

#' Stable fingerprint of a sensitivity branch
#' @param specification One-row specification table or named list.
#' @export
sensitivity_branch_fingerprint <- function(specification) {
  .ep09_hash_object(specification)
}

#' Machine-readable multiverse manifest
#' @param x Sensitivity result.
#' @export
sensitivity_multiverse_manifest <- function(x) {
  if (!inherits(x, "eye_process_sensitivity")) stop("x must be an eye_process_sensitivity.", call. = FALSE)
  g <- x$grid
  data.frame(
    specification_id = g$specification_id,
    branch_hash = vapply(seq_len(nrow(g)), function(i) .ep09_hash_object(g[i, , drop = FALSE]), character(1)),
    evaluated = g$specification_id %in% x$results$specification_id,
    failed = g$specification_id %in% x$failures$specification_id,
    stringsAsFactors = FALSE
  )
}

.ep09_compare_methods <- function(data, methods, analysis_fun, extract_fun, dimension) {
  if (is.null(names(methods)) || any(!nzchar(names(methods)))) stop("methods must be a named list/vector.", call. = FALSE)
  grid <- process_sensitivity_grid(method = names(methods), label = paste0(dimension, "_comparison"))
  wrapped <- function(d, spec) analysis_fun(d, methods[[as.character(spec$method[[1L]])]], spec)
  run_process_sensitivity(data, grid, wrapped, extract_fun = extract_fun)
}

#' Compare explicit AOI assignment methods
#' @param data Data.
#' @param methods Named methods/specifications.
#' @param analysis_fun Function `(data, method, specification)`.
#' @param extract_fun Result extractor.
#' @export
compare_aoi_methods <- function(data, methods, analysis_fun, extract_fun = .ep09_default_sensitivity_extract) {
  .ep09_compare_methods(data, methods, analysis_fun, extract_fun, "aoi")
}

#' Compare explicit fixation-detection methods
#' @inheritParams compare_aoi_methods
#' @export
compare_fixation_methods <- function(data, methods, analysis_fun, extract_fun = .ep09_default_sensitivity_extract) {
  .ep09_compare_methods(data, methods, analysis_fun, extract_fun, "fixation")
}

#' Compare explicit pupil-preprocessing methods
#' @inheritParams compare_aoi_methods
#' @export
compare_pupil_preprocessing <- function(data, methods, analysis_fun, extract_fun = .ep09_default_sensitivity_extract) {
  .ep09_compare_methods(data, methods, analysis_fun, extract_fun, "pupil_preprocessing")
}

#' Compare explicit process-model specifications
#' @inheritParams compare_aoi_methods
#' @export
compare_process_models <- function(data, methods, analysis_fun, extract_fun = .ep09_default_sensitivity_extract) {
  .ep09_compare_methods(data, methods, analysis_fun, extract_fun, "process_model")
}

#' @export
print.eye_process_sensitivity <- function(x, ...) {
  cat("eyeprocess defensible multiverse\n")
  cat("  planned specifications :", nrow(x$grid), "\n")
  cat("  evaluated specifications:", length(unique(x$results$specification_id)), "\n")
  cat("  failures               :", nrow(x$failures), "\n")
  cat("  captured warnings      :", if (is.null(x$warnings)) 0L else nrow(x$warnings), "\n")
  invisible(x)
}
