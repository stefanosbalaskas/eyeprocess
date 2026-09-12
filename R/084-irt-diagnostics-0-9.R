# eyeprocess 0.9 Milestone #2: IRT diagnostics and model-checking summaries

.ep09m2_binary_matrix <- function(x, name = "responses", allow_na = TRUE) {
  x <- as.matrix(x)
  storage.mode(x) <- "numeric"
  vals <- unique(as.numeric(x))
  vals <- vals[!is.na(vals)]
  if (any(!vals %in% c(0, 1))) stop(name, " must contain only 0, 1", if (allow_na) ", and NA" else "", ".", call. = FALSE)
  if (!allow_na && anyNA(x)) stop(name, " cannot contain NA.", call. = FALSE)
  x
}

.ep09m2_prob_matrix <- function(x, dim_ref, name = "probabilities") {
  x <- as.matrix(x); storage.mode(x) <- "numeric"
  if (!identical(dim(x), dim_ref)) stop(name, " must have the same dimensions as responses.", call. = FALSE)
  if (any(!is.finite(x[!is.na(x)])) || any(x[!is.na(x)] <= 0 | x[!is.na(x)] >= 1))
    stop(name, " must contain probabilities strictly between 0 and 1 (or NA where responses are missing).", call. = FALSE)
  x
}

.ep09m2_std_residual <- function(y, p) (y - p) / sqrt(pmax(p * (1 - p), .Machine$double.eps))

#' Compute item residual fit summaries from observed and predicted probabilities
#' @param responses Response matrix or response data.
#' @param probabilities Probability matrix or vector, with dimensions appropriate to the model.
#' @param item_ids Optional item identifiers.
#' @return An object of class "eye_irt_item_fit", "data.frame", stored as a data frame, containing item residual fit summaries from observed and predicted probabilities and associated metadata needed to interpret the result.
#' @export
eyeprocess_irt_item_fit_residuals <- function(responses, probabilities, item_ids = colnames(responses)) {
  y <- .ep09m2_binary_matrix(responses)
  p <- .ep09m2_prob_matrix(probabilities, dim(y))
  if (is.null(item_ids)) item_ids <- paste0("item_", seq_len(ncol(y)))
  if (length(item_ids) != ncol(y)) stop("item_ids length must equal number of items.", call. = FALSE)
  out <- lapply(seq_len(ncol(y)), function(j) {
    keep <- !is.na(y[, j]) & !is.na(p[, j])
    if (!any(keep)) return(data.frame(item_id = item_ids[j], n = 0L, mean_residual = NA_real_, rms_standardized_residual = NA_real_, outfit = NA_real_, infit = NA_real_))
    yy <- y[keep, j]; pp <- p[keep, j]
    r <- yy - pp; sr <- .ep09m2_std_residual(yy, pp); var <- pp * (1 - pp)
    data.frame(item_id = item_ids[j], n = length(yy), mean_residual = mean(r),
               rms_standardized_residual = sqrt(mean(sr^2)), outfit = mean(sr^2),
               infit = sum(r^2) / sum(var), stringsAsFactors = FALSE)
  })
  structure(do.call(rbind, out), class = c("eye_irt_item_fit", "data.frame"))
}

#' Compute person residual fit summaries
#' @param responses Response matrix or response data.
#' @param probabilities Probability matrix or vector, with dimensions appropriate to the model.
#' @param person_ids Optional person identifiers.
#' @return An object of class "eye_irt_person_fit", "data.frame", stored as a data frame, containing person residual fit summaries and associated metadata needed to interpret the result.
#' @export
eyeprocess_irt_person_fit_residuals <- function(responses, probabilities, person_ids = rownames(responses)) {
  y <- .ep09m2_binary_matrix(responses)
  p <- .ep09m2_prob_matrix(probabilities, dim(y))
  if (is.null(person_ids)) person_ids <- paste0("person_", seq_len(nrow(y)))
  if (length(person_ids) != nrow(y)) stop("person_ids length must equal number of persons.", call. = FALSE)
  out <- lapply(seq_len(nrow(y)), function(i) {
    keep <- !is.na(y[i, ]) & !is.na(p[i, ])
    if (!any(keep)) return(data.frame(person_id = person_ids[i], n = 0L, raw_score = NA_real_, expected_score = NA_real_, outfit = NA_real_, infit = NA_real_))
    yy <- y[i, keep]; pp <- p[i, keep]; r <- yy - pp; sr <- .ep09m2_std_residual(yy, pp); var <- pp * (1 - pp)
    data.frame(person_id = person_ids[i], n = length(yy), raw_score = sum(yy), expected_score = sum(pp),
               outfit = mean(sr^2), infit = sum(r^2) / sum(var), stringsAsFactors = FALSE)
  })
  structure(do.call(rbind, out), class = c("eye_irt_person_fit", "data.frame"))
}

#' Compute Yen-style Q3 residual correlations
#' @param responses Response matrix or response data.
#' @param probabilities Probability matrix or vector, with dimensions appropriate to the model.
#' @param use Missing-data handling mode passed to the residual correlation calculation.
#' @return An R object containing yen-style Q3 residual correlations. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
eyeprocess_irt_q3 <- function(responses, probabilities, use = "pairwise.complete.obs") {
  y <- .ep09m2_binary_matrix(responses); p <- .ep09m2_prob_matrix(probabilities, dim(y))
  resid <- y - p
  q3 <- suppressWarnings(stats::cor(resid, use = use))
  diag(q3) <- NA_real_
  class(q3) <- c("eye_irt_q3_matrix", class(q3))
  q3
}

#' Extract high residual-dependence item pairs
#' @param q3 Q3 residual-correlation matrix or summary.
#' @param threshold Decision or diagnostic threshold.
#' @param absolute Whether diagnostic thresholds apply to absolute values.
#' @return An R object containing high residual-dependence item pairs. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
eyeprocess_irt_local_dependence_pairs <- function(q3, threshold = 0.20, absolute = TRUE) {
  q3 <- as.matrix(q3); threshold <- as.numeric(threshold)
  if (nrow(q3) != ncol(q3) || length(threshold) != 1L || !is.finite(threshold) || threshold < 0) stop("q3 must be square and threshold non-negative.", call. = FALSE)
  ids <- colnames(q3); if (is.null(ids)) ids <- paste0("item_", seq_len(ncol(q3)))
  idx <- which(upper.tri(q3) & is.finite(q3) & (if (absolute) abs(q3) >= threshold else q3 >= threshold), arr.ind = TRUE)
  if (!nrow(idx)) return(data.frame(item_1 = character(), item_2 = character(), q3 = numeric(), abs_q3 = numeric()))
  out <- data.frame(item_1 = ids[idx[, 1]], item_2 = ids[idx[, 2]], q3 = q3[idx], abs_q3 = abs(q3[idx]), stringsAsFactors = FALSE)
  out[order(out$abs_q3, decreasing = TRUE), , drop = FALSE]
}

#' Audit extreme response scores without assigning behavioral labels
#' @param responses Response matrix or response data.
#' @param lower_fraction Lower extreme-score fraction.
#' @param upper_fraction Upper extreme-score fraction.
#' @return A data frame containing extreme response scores without assigning behavioral labels. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
eyeprocess_irt_extreme_score_audit <- function(responses, lower_fraction = 0.02, upper_fraction = 0.98) {
  y <- .ep09m2_binary_matrix(responses)
  lower_fraction <- as.numeric(lower_fraction); upper_fraction <- as.numeric(upper_fraction)
  if (length(lower_fraction) != 1L || length(upper_fraction) != 1L || !is.finite(lower_fraction) || !is.finite(upper_fraction) || lower_fraction < 0 || upper_fraction > 1 || lower_fraction >= upper_fraction)
    stop("fractions must satisfy 0 <= lower < upper <= 1.", call. = FALSE)
  n_answered <- rowSums(!is.na(y)); score <- rowSums(y, na.rm = TRUE); frac <- ifelse(n_answered > 0, score / n_answered, NA_real_)
  data.frame(person_id = if (is.null(rownames(y))) paste0("person_", seq_len(nrow(y))) else rownames(y), n_answered = n_answered,
             raw_score = score, score_fraction = frac, lower_extreme = frac <= lower_fraction, upper_extreme = frac >= upper_fraction,
             stringsAsFactors = FALSE)
}

#' Audit ordered category thresholds
#' @param item_id Item identifier or vector of item identifiers.
#' @param thresholds Ordered response-category thresholds.
#' @return An object of class "eye_irt_threshold_audit", stored as a named list, with components "item_id", "thresholds", "ordered", "minimum_gap", "reversals". It contains ordered category thresholds and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_threshold_order_audit <- function(item_id, thresholds) {
  item_id <- as.character(item_id); thresholds <- as.numeric(thresholds)
  if (length(item_id) != 1L || is.na(item_id) || !length(thresholds) || any(!is.finite(thresholds))) stop("invalid item_id or thresholds.", call. = FALSE)
  diffs <- diff(thresholds)
  structure(list(item_id = item_id, thresholds = thresholds, ordered = all(diffs > 0), minimum_gap = if (length(diffs)) min(diffs) else NA_real_, reversals = which(diffs <= 0)),
            class = "eye_irt_threshold_audit")
}

#' Audit monotonicity of an item response curve
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param probability Model-implied probability vector.
#' @param tolerance Numerical or decision tolerance.
#' @return An object of class "eye_irt_monotonicity_audit", stored as a named list, with components "monotone_non_decreasing", "n_decreases", "largest_decrease", "theta", "probability". It contains monotonicity of an item response curve and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_monotonicity_audit <- function(theta, probability, tolerance = 1e-8) {
  theta <- as.numeric(theta); probability <- as.numeric(probability); tolerance <- as.numeric(tolerance)
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) stop("tolerance must be a finite non-negative scalar.", call. = FALSE)
  if (length(theta) != length(probability) || length(theta) < 2L || any(!is.finite(theta)) || any(!is.finite(probability)) || any(probability < 0 | probability > 1)) stop("theta/probability must be finite equal-length vectors; probabilities in [0,1].", call. = FALSE)
  ord <- order(theta); theta <- theta[ord]; probability <- probability[ord]
  dp <- diff(probability)
  structure(list(monotone_non_decreasing = all(dp >= -tolerance), n_decreases = sum(dp < -tolerance), largest_decrease = if (any(dp < 0)) min(dp) else 0,
                 theta = theta, probability = probability), class = "eye_irt_monotonicity_audit")
}

#' Audit category probability functions
#' @param probabilities Probability matrix or vector, with dimensions appropriate to the model.
#' @param tolerance Numerical or decision tolerance.
#' @return An object of class "eye_irt_category_audit", stored as a named list, with components "valid_bounds", "rows_sum_to_one", "max_sum_error", "min_probability", "max_probability". It contains category probability functions and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_category_function_audit <- function(probabilities, tolerance = 1e-8) {
  p <- as.matrix(probabilities); storage.mode(p) <- "numeric"; tolerance <- as.numeric(tolerance)
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) stop("tolerance must be a finite non-negative scalar.", call. = FALSE)
  if (!nrow(p) || ncol(p) < 2L || any(!is.finite(p))) stop("probabilities must be a finite matrix with >= 2 categories.", call. = FALSE)
  row_sum <- rowSums(p)
  structure(list(valid_bounds = all(p >= -tolerance & p <= 1 + tolerance), rows_sum_to_one = all(abs(row_sum - 1) <= tolerance),
                 max_sum_error = max(abs(row_sum - 1)), min_probability = min(p), max_probability = max(p)), class = "eye_irt_category_audit")
}

#' Audit basic plausibility of dichotomous item parameters
#' @param items Item-parameter data frame or item collection.
#' @param discrimination Discrimination vector or matrix.
#' @param difficulty Item difficulty or location parameter.
#' @param lower_asymptote Lower-asymptote parameter values.
#' @param upper_asymptote Upper-asymptote parameter values.
#' @return A data frame containing basic plausibility of dichotomous item parameters. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
eyeprocess_irt_parameter_plausibility_audit <- function(items, discrimination = c(0.2, 4), difficulty = c(-6, 6), lower_asymptote = c(0, 0.5), upper_asymptote = c(0.5, 1)) {
  items <- .ep09m2_item_pars(items)
  rngs <- list(discrimination = discrimination, difficulty = difficulty, lower_asymptote = lower_asymptote, upper_asymptote = upper_asymptote)
  if (any(!vapply(rngs, function(z) length(z) == 2L && all(is.finite(z)) && z[1] < z[2], logical(1)))) stop("all ranges must be increasing finite length-2 vectors.", call. = FALSE)
  out <- data.frame(item_id = items$item_id,
                    a_flag = items$a < discrimination[1] | items$a > discrimination[2],
                    b_flag = items$b < difficulty[1] | items$b > difficulty[2],
                    c_flag = items$c < lower_asymptote[1] | items$c > lower_asymptote[2],
                    d_flag = items$d < upper_asymptote[1] | items$d > upper_asymptote[2], stringsAsFactors = FALSE)
  out$any_flag <- rowSums(out[c("a_flag", "b_flag", "c_flag", "d_flag")]) > 0
  attr(out, "guardrail") <- "Ranges are review conventions, not universal psychometric cutoffs."
  out
}

#' Compare observed and replicated IRT discrepancy statistics
#' @param observed Observed responses or observed values.
#' @param replicated Replicated data or replicated statistic values.
#' @param statistic Discrepancy statistic or statistic function.
#' @return An object of class "eye_irt_ppc_discrepancy", stored as a named list, with components "statistic", "observed", "replicated", "posterior_predictive_p", "interval". It contains observed and replicated IRT discrepancy statistics and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_ppc_discrepancy <- function(observed, replicated, statistic = c("mean_score", "score_sd", "item_means", "max_item_residual")) {
  statistic <- match.arg(statistic); obs <- .ep09m2_binary_matrix(observed)
  if (length(dim(replicated)) != 3L || !identical(dim(replicated)[2:3], dim(obs))) stop("replicated must be an array [draw, person, item] matching observed dimensions.", call. = FALSE)
  stat_fun <- switch(statistic,
    mean_score = function(z) mean(rowSums(z, na.rm = TRUE)),
    score_sd = function(z) stats::sd(rowSums(z, na.rm = TRUE)),
    item_means = function(z) mean(colMeans(z, na.rm = TRUE)),
    max_item_residual = function(z) max(abs(colMeans(z, na.rm = TRUE) - colMeans(obs, na.rm = TRUE)))
  )
  obs_stat <- if (statistic == "max_item_residual") 0 else stat_fun(obs)
  rep_stats <- vapply(seq_len(dim(replicated)[1L]), function(i) stat_fun(replicated[i, , ]), numeric(1))
  structure(list(statistic = statistic, observed = obs_stat, replicated = rep_stats,
                 posterior_predictive_p = mean(rep_stats >= obs_stat), interval = .ep09m2_safe_quantile(rep_stats, c(.025, .5, .975))),
            class = "eye_irt_ppc_discrepancy")
}

#' Build an integrated IRT diagnostic dashboard object
#' @param item_fit Item-fit diagnostic object or table.
#' @param person_fit Person-fit diagnostic object or table.
#' @param q3 Q3 residual-correlation matrix or summary.
#' @param parameter_audit Item-parameter plausibility audit.
#' @param identification Identification specification or identification audit.
#' @return An object of class "eye_irt_fit_dashboard", stored as a named list, with components "components", "present", "n_components", "interpretation". It contains an integrated IRT diagnostic dashboard object and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_fit_dashboard <- function(item_fit = NULL, person_fit = NULL, q3 = NULL, parameter_audit = NULL, identification = NULL) {
  components <- list(item_fit = item_fit, person_fit = person_fit, q3 = q3, parameter_audit = parameter_audit, identification = identification)
  present <- names(components)[!vapply(components, is.null, logical(1))]
  structure(list(components = components, present = present, n_components = length(present),
                 interpretation = "Diagnostics identify model/data tensions; they are not evidence for behavioral or clinical labels."), class = "eye_irt_fit_dashboard")
}

#' @export
print.eye_irt_fit_dashboard <- function(x, ...) {
  cat("eyeprocess IRT fit dashboard\n")
  cat("  components:", x$n_components, "\n")
  cat("  present   :", paste(x$present, collapse = ", "), "\n")
  invisible(x)
}
