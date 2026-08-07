# Pupil phase-amplitude registration -----------------------------------------

#' Register pupil response curves in time
#'
#' @param x Long-format pupil data.
#' @param time,pupil Column names.
#' @param anchor Stimulus, response, or event anchor declaration.
#' @param method Elastic-style peak registration or landmark registration.
#' @param id_col Curve identifier.
#' @param grid_size Number of registered time points.
#' @return An `eye_pupil_registration` object.
#' @export
register_pupil_curves <- function(x, time, pupil, anchor = c("stimulus", "response", "event"), method = c("elastic", "landmark"), id_col = "person_id", grid_size = 101) {
  .mi_assert_data(x)
  anchor <- match.arg(anchor)
  method <- match.arg(method)
  .mi_assert_columns(x, c(id_col, time, pupil))
  ids <- unique(as.character(x[[id_col]]))
  normalized_grid <- seq(0, 1, length.out = as.integer(grid_size))
  raw_matrix <- registered_matrix <- matrix(NA_real_, nrow = length(ids), ncol = length(normalized_grid), dimnames = list(ids, NULL))
  shifts <- numeric(length(ids))
  peak_positions <- numeric(length(ids))
  curves <- vector("list", length(ids)); names(curves) <- ids
  for (i in seq_along(ids)) {
    data <- x[as.character(x[[id_col]]) == ids[[i]], c(time, pupil), drop = FALSE]
    data[[time]] <- .mi_numeric(data[[time]]); data[[pupil]] <- .mi_numeric(data[[pupil]])
    data <- data[stats::complete.cases(data), ]; data <- data[order(data[[time]]), ]
    if (nrow(data) < 3L) next
    t_norm <- (data[[time]] - min(data[[time]])) / pmax(diff(range(data[[time]])), 1e-12)
    raw_curve <- stats::approx(t_norm, data[[pupil]], xout = normalized_grid, rule = 2, ties = "ordered")$y
    peak <- normalized_grid[which.max(raw_curve)]
    peak_positions[[i]] <- peak
    raw_matrix[i, ] <- raw_curve
    curves[[i]] <- data
  }
  reference_peak <- stats::median(peak_positions[is.finite(peak_positions)], na.rm = TRUE)
  if (!is.finite(reference_peak)) reference_peak <- 0.5
  for (i in seq_along(ids)) {
    if (all(!is.finite(raw_matrix[i, ]))) next
    shifts[[i]] <- reference_peak - peak_positions[[i]]
    warped_grid <- normalized_grid - shifts[[i]]
    if (method == "elastic") {
      strength <- 0.75
      warped_grid <- normalized_grid - strength * shifts[[i]] * (4 * normalized_grid * (1 - normalized_grid))
    }
    registered_matrix[i, ] <- stats::approx(normalized_grid, raw_matrix[i, ], xout = pmin(1, pmax(0, warped_grid)), rule = 2, ties = "ordered")$y
  }
  summary <- data.frame(curve_id = ids, peak_position = peak_positions, phase_shift = shifts, stringsAsFactors = FALSE)
  .mi_new(
    "eye_pupil_registration",
    data = x,
    ids = ids,
    time_col = time,
    pupil_col = pupil,
    id_col = id_col,
    anchor = anchor,
    method = method,
    grid = normalized_grid,
    raw = raw_matrix,
    registered = registered_matrix,
    summary = summary,
    status = "Pupil curves registered and separated from latency variation."
  )
}

#' Decompose registered pupil curves into phase and amplitude scores
#'
#' @param x Pupil-registration object.
#' @param components Number of amplitude components.
#' @return An `eye_pupil_phase_amplitude` object.
#' @export
decompose_pupil_phase_amplitude <- function(x, components = 3) {
  if (!inherits(x, "eye_pupil_registration")) .mi_stop("`x` must be an `eye_pupil_registration` object.")
  complete <- rowSums(is.finite(x$registered)) >= max(3L, ncol(x$registered) / 2)
  matrix <- x$registered[complete, , drop = FALSE]
  if (nrow(matrix) < 2L) .mi_stop("At least two complete registered curves are required.")
  for (i in seq_len(nrow(matrix))) {
    missing <- !is.finite(matrix[i, ])
    if (any(missing)) matrix[i, missing] <- .mi_safe_mean(matrix[i, ])
  }
  pca <- stats::prcomp(matrix, center = TRUE, scale. = FALSE)
  components <- min(as.integer(components), ncol(pca$x))
  amplitude_scores <- as.data.frame(pca$x[, seq_len(components), drop = FALSE])
  names(amplitude_scores) <- paste0("amplitude_pc", seq_len(components))
  amplitude_scores[[x$id_col]] <- rownames(matrix)
  phase_scores <- x$summary[x$summary$curve_id %in% rownames(matrix), c("curve_id", "phase_shift", "peak_position")]
  names(phase_scores)[names(phase_scores) == "curve_id"] <- x$id_col
  scores <- merge(phase_scores, amplitude_scores, by = x$id_col, all = TRUE)
  .mi_new(
    "eye_pupil_phase_amplitude",
    registration = x,
    pca = pca,
    scores = scores,
    amplitude_variance = pca$sdev^2 / sum(pca$sdev^2),
    summary = scores,
    status = "Pupil phase and amplitude components decomposed."
  )
}

#' Fit a phase-amplitude response model
#'
#' @param responses Person-by-item response matrix or person-level outcome.
#' @param phase_scores Phase scores or decomposition object.
#' @param amplitude_scores Optional amplitude-score table.
#' @param person_id Optional person identifiers.
#' @param family Gaussian or binomial outcome model.
#' @param ... Additional arguments reserved for external IRT engines.
#' @return An `eye_phase_amplitude_irt` object.
#' @export
fit_phase_amplitude_irt <- function(responses, phase_scores, amplitude_scores = NULL, person_id = NULL, family = c("gaussian", "binomial"), ...) {
  family <- match.arg(family)
  if (inherits(phase_scores, "eye_pupil_phase_amplitude")) {
    scores <- phase_scores$scores
  } else {
    scores <- as.data.frame(phase_scores)
    if (!is.null(amplitude_scores)) scores <- cbind(scores, as.data.frame(amplitude_scores))
  }
  response_matrix <- as.matrix(responses)
  outcome <- if (ncol(response_matrix) > 1L) rowMeans(response_matrix, na.rm = TRUE) else .mi_numeric(response_matrix[, 1L])
  if (is.null(person_id)) person_id <- rownames(response_matrix) %||% seq_along(outcome)
  id_col <- intersect(c("person_id", "participant_id", "curve_id"), names(scores))
  if (length(id_col)) {
    data <- merge(data.frame(person_id = as.character(person_id), outcome = outcome), transform(scores, person_id = as.character(scores[[id_col[[1L]]]])), by = "person_id")
  } else {
    if (nrow(scores) != length(outcome)) .mi_stop("Phase/amplitude scores must align with the response rows.")
    data <- cbind(data.frame(person_id = as.character(person_id), outcome = outcome), scores)
  }
  predictors <- names(data)[vapply(data, is.numeric, logical(1))]
  predictors <- setdiff(predictors, "outcome")
  formula <- .mi_formula("outcome", predictors)
  model <- if (family == "binomial") stats::glm(formula, data = data, family = stats::binomial()) else stats::lm(formula, data = data)
  .mi_new(
    "eye_phase_amplitude_irt",
    model = model,
    data = data,
    family = family,
    summary = as.data.frame(summary(model)$coefficients),
    status = "Phase-amplitude response model fitted. For confirmatory IRT, use a validated external engine or custom Stan model."
  )
}

#' Audit pupil registration quality
#'
#' @param x Pupil-registration object.
#' @return An `eye_pupil_registration_audit` object.
#' @export
audit_pupil_registration <- function(x) {
  if (!inherits(x, "eye_pupil_registration")) .mi_stop("`x` must be an `eye_pupil_registration` object.")
  raw_peak <- apply(x$raw, 1L, function(curve) if (all(!is.finite(curve))) NA_real_ else which.max(curve))
  registered_peak <- apply(x$registered, 1L, function(curve) if (all(!is.finite(curve))) NA_real_ else which.max(curve))
  before <- stats::var(raw_peak, na.rm = TRUE)
  after <- stats::var(registered_peak, na.rm = TRUE)
  improvement <- (before - after) / pmax(before, 1e-12)
  .mi_new(
    "eye_pupil_registration_audit",
    summary = data.frame(peak_variance_before = before, peak_variance_after = after, relative_reduction = improvement),
    raw_peak = raw_peak,
    registered_peak = registered_peak,
    status = "Pupil registration audit completed."
  )
}

#' @export
plot.eye_pupil_registration <- function(x, type = c("registration", "warping", "item_delay", "registered_effects", "diagnostics"), curves = 20, ...) {
  type <- match.arg(type)
  n <- min(as.integer(curves), nrow(x$raw))
  if (type == "registration") {
    graphics::matplot(x$grid, t(x$raw[seq_len(n), , drop = FALSE]), type = "l", lty = 1, xlab = "Normalized time", ylab = "Pupil", main = "Raw pupil curves")
    graphics::matlines(x$grid, t(x$registered[seq_len(n), , drop = FALSE]), lty = 2)
  } else if (type == "warping") {
    graphics::plot(x$summary$peak_position, x$summary$phase_shift, xlab = "Raw peak position", ylab = "Estimated phase shift", main = "Pupil warping functions")
  } else if (type == "item_delay") {
    graphics::barplot(x$summary$phase_shift, names.arg = x$summary$curve_id, las = 2, ylab = "Phase shift", main = "Pupil response delay")
  } else {
    mean_curve <- colMeans(x$registered, na.rm = TRUE)
    graphics::plot(x$grid, mean_curve, type = "l", xlab = "Registered time", ylab = "Mean pupil", main = "Registered pupil effect")
  }
  invisible(x)
}
#' @export
plot.eye_pupil_phase_amplitude <- function(x, type = c("scores", "variance"), ...) {
  type <- match.arg(type)
  if (type == "scores" && all(c("amplitude_pc1", "phase_shift") %in% names(x$scores))) {
    graphics::plot(x$scores$phase_shift, x$scores$amplitude_pc1, xlab = "Phase shift", ylab = "Amplitude PC1", main = "Pupil phase-amplitude scores")
  } else {
    graphics::barplot(x$amplitude_variance, ylab = "Variance proportion", main = "Amplitude component variance")
  }
  invisible(x)
}
#' @export
plot.eye_phase_amplitude_irt <- function(x, type = c("registered_effects", "diagnostics"), ...) {
  type <- match.arg(type)
  if (type == "diagnostics") {
    graphics::plot(stats::fitted(x$model), stats::residuals(x$model), xlab = "Fitted", ylab = "Residual", main = "Phase-amplitude model diagnostics")
    graphics::abline(h = 0, lty = 2)
  } else {
    coefficients <- stats::coef(x$model)
    graphics::barplot(coefficients, las = 2, ylab = "Coefficient", main = "Phase-amplitude effects")
  }
  invisible(x)
}
#' @export
plot_pupil_registration <- function(x, ...) plot(x, type = "registration", ...)
#' @export
plot_warping_functions <- function(x, ...) plot(x, type = "warping", ...)
#' @export
plot_phase_amplitude_scores <- function(x, ...) plot(x, type = "scores", ...)
#' @export
plot_item_phase_delay <- function(x, ...) plot(x, type = "item_delay", ...)
#' @export
plot_registered_pupil_effects <- function(x, ...) plot(x, type = "registered_effects", ...)
