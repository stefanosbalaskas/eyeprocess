# Compositional AOI attention -------------------------------------------------

.mi_close_composition <- function(matrix, zero_method = "multiplicative") {
  matrix <- as.matrix(matrix)
  storage.mode(matrix) <- "double"
  matrix[!is.finite(matrix) | matrix < 0] <- NA_real_
  if (anyNA(matrix)) {
    for (j in seq_len(ncol(matrix))) {
      replacement <- .mi_safe_mean(matrix[, j])
      if (!is.finite(replacement)) replacement <- 0
      matrix[is.na(matrix[, j]), j] <- replacement
    }
  }
  positive <- matrix[matrix > 0 & is.finite(matrix)]
  global_delta <- if (length(positive)) min(positive) * 0.5 else 1e-6
  for (i in seq_len(nrow(matrix))) {
    zeros <- !is.finite(matrix[i, ]) | matrix[i, ] <= 0
    if (any(zeros)) {
      delta <- if (zero_method == "bayesian") {
        (sum(matrix[i, !zeros], na.rm = TRUE) + 1) / (1000 * ncol(matrix))
      } else {
        global_delta
      }
      matrix[i, zeros] <- delta
    }
    total <- sum(matrix[i, ])
    if (!is.finite(total) || total <= 0) matrix[i, ] <- rep(1 / ncol(matrix), ncol(matrix)) else matrix[i, ] <- matrix[i, ] / total
  }
  matrix
}

.mi_ilr_basis <- function(parts) {
  basis <- stats::contr.helmert(parts)
  basis <- apply(basis, 2L, function(column) column / sqrt(sum(column^2)))
  as.matrix(basis)
}

#' Derive closed AOI dwell compositions
#'
#' @param x Wide data with AOI dwell columns, or long data with `aoi_col` and `value_col`.
#' @param aois AOI dwell columns or AOI labels.
#' @param denominator Total AOI dwell or trial duration.
#' @param zero_method Multiplicative or Bayesian-style zero replacement.
#' @param id_cols Identifiers retained in the result.
#' @param aoi_col,value_col Long-format columns.
#' @param trial_duration_col Optional trial-duration column.
#' @return An `eye_aoi_composition` object.
#' @export
derive_aoi_composition <- function(
    x,
    aois,
    denominator = c("total_aoi_dwell", "trial_duration"),
    zero_method = c("multiplicative", "bayesian"),
    id_cols = NULL,
    aoi_col = "aoi",
    value_col = "dwell_ms",
    trial_duration_col = NULL) {
  .mi_assert_data(x)
  denominator <- match.arg(denominator)
  zero_method <- match.arg(zero_method)
  aois <- as.character(aois)
  if (all(aois %in% names(x))) {
    raw <- as.matrix(x[, aois, drop = FALSE])
    ids <- x[, id_cols[id_cols %in% names(x)], drop = FALSE]
    duration <- if (!is.null(trial_duration_col) && trial_duration_col %in% names(x)) .mi_numeric(x[[trial_duration_col]]) else rowSums(raw, na.rm = TRUE)
  } else {
    .mi_assert_columns(x, c(aoi_col, value_col))
    id_cols <- id_cols[id_cols %in% names(x)]
    if (!length(id_cols)) {
      x$.composition_row_id <- seq_len(nrow(x))
      id_cols <- ".composition_row_id"
    }
    key <- interaction(x[, id_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
    levels_key <- levels(key)
    raw <- matrix(0, nrow = length(levels_key), ncol = length(aois), dimnames = list(levels_key, aois))
    for (i in seq_along(levels_key)) {
      rows <- key == levels_key[[i]]
      for (j in seq_along(aois)) {
        raw[i, j] <- sum(.mi_numeric(x[[value_col]][rows & as.character(x[[aoi_col]]) == aois[[j]]]), na.rm = TRUE)
      }
    }
    first_rows <- match(levels_key, key)
    ids <- x[first_rows, id_cols, drop = FALSE]
    duration <- if (!is.null(trial_duration_col) && trial_duration_col %in% names(x)) {
      vapply(levels_key, function(level) .mi_safe_mean(x[[trial_duration_col]][key == level]), numeric(1))
    } else rowSums(raw, na.rm = TRUE)
  }
  proportions <- .mi_close_composition(raw, zero_method = zero_method)
  if (denominator == "trial_duration") {
    duration[!is.finite(duration) | duration <= 0] <- rowSums(raw, na.rm = TRUE)[!is.finite(duration) | duration <= 0]
    proportions <- raw / pmax(duration, 1e-8)
    proportions <- .mi_close_composition(proportions, zero_method = zero_method)
  }
  table <- cbind(ids, as.data.frame(proportions, check.names = FALSE))
  .mi_new(
    "eye_aoi_composition",
    raw = raw,
    proportions = proportions,
    table = table,
    parts = colnames(proportions),
    id_cols = names(ids),
    denominator = denominator,
    zero_method = zero_method,
    status = "AOI dwell composition derived and closed to one."
  )
}

#' Transform an AOI composition
#'
#' @param x AOI composition object or numeric matrix.
#' @param method ILR, CLR, or ALR.
#' @param reference ALR reference part.
#' @return An `eye_aoi_logratio` object.
#' @export
transform_aoi_composition <- function(x, method = c("ilr", "clr", "alr"), reference = NULL) {
  method <- match.arg(method)
  composition <- if (inherits(x, "eye_aoi_composition")) x$proportions else .mi_close_composition(x)
  log_composition <- log(pmax(composition, 1e-15))
  if (method == "clr") {
    transformed <- log_composition - rowMeans(log_composition)
    colnames(transformed) <- paste0("clr_", colnames(composition))
    basis <- NULL
  } else if (method == "alr") {
    reference <- reference %||% colnames(composition)[[ncol(composition)]]
    if (!reference %in% colnames(composition)) .mi_stop("`reference` is not an AOI part.")
    keep <- setdiff(colnames(composition), reference)
    transformed <- sweep(log_composition[, keep, drop = FALSE], 1L, log_composition[, reference], "-")
    colnames(transformed) <- paste0("alr_", keep, "_vs_", reference)
    basis <- reference
  } else {
    basis <- .mi_ilr_basis(ncol(composition))
    transformed <- log_composition %*% basis
    colnames(transformed) <- paste0("ilr_", seq_len(ncol(transformed)))
    rownames(transformed) <- rownames(composition)
  }
  .mi_new(
    "eye_aoi_logratio",
    source = x,
    transformed = transformed,
    method = method,
    basis = basis,
    parts = colnames(composition),
    status = paste(toupper(method), "AOI log-ratio transform completed.")
  )
}

#' Fit a compositional AOI regression model
#'
#' @param composition AOI composition or log-ratio object.
#' @param formula Model formula. The response must name a transformed coordinate.
#' @param random Reserved random-effects formula; recorded but not silently fitted.
#' @param data Optional covariate table.
#' @param method Log-ratio transform used when `composition` is untransformed.
#' @return An `eye_aoi_composition_model` object.
#' @export
fit_aoi_compositional_model <- function(composition, formula, random = NULL, data = NULL, method = "ilr") {
  transformed <- if (inherits(composition, "eye_aoi_logratio")) composition else transform_aoi_composition(composition, method = method)
  model_data <- as.data.frame(transformed$transformed)
  if (!is.null(data)) {
    .mi_assert_data(data)
    if (nrow(data) != nrow(model_data)) .mi_stop("`data` must have one row per composition.")
    model_data <- cbind(data, model_data)
  }
  model <- stats::lm(formula, data = model_data)
  if (!is.null(random)) .mi_warn("`random` is recorded for audit but the dependency-free engine fits a fixed-effects model. Use an external mixed-model adapter for confirmatory random effects.")
  .mi_new(
    "eye_aoi_composition_model",
    model = model,
    composition = transformed,
    formula = formula,
    random = random,
    data = model_data,
    summary = as.data.frame(summary(model)$coefficients),
    status = "Compositional AOI model fitted on log-ratio coordinates."
  )
}

.mi_composition_pseudo_f <- function(z, group) {
  group <- factor(group)
  grand <- colMeans(z)
  within <- 0
  between <- 0
  for (level in levels(group)) {
    rows <- group == level
    center <- colMeans(z[rows, , drop = FALSE])
    between <- between + sum(rows) * sum((center - grand)^2)
    within <- within + sum(rowSums((z[rows, , drop = FALSE] - matrix(center, sum(rows), ncol(z), byrow = TRUE))^2))
  }
  df_between <- max(1, nlevels(group) - 1L)
  df_within <- max(1, nrow(z) - nlevels(group))
  (between / df_between) / pmax(within / df_within, 1e-12)
}

#' Compare AOI compositions across groups
#'
#' @param x AOI composition object.
#' @param group Group vector or column name in `x$table`.
#' @param method Permutation MANOVA approximation.
#' @param permutations Number of label permutations.
#' @param seed Random seed.
#' @return An `eye_aoi_composition_comparison` object.
#' @export
compare_aoi_compositions <- function(
    x,
    group,
    method = c("permanova", "compositional_manova"),
    permutations = 499,
    seed = 20260807) {
  if (!inherits(x, "eye_aoi_composition")) .mi_stop("`x` must be an `eye_aoi_composition` object.")
  method <- match.arg(method)
  group_values <- if (length(group) == 1L && is.character(group) && group %in% names(x$table)) x$table[[group]] else group
  if (length(group_values) != nrow(x$proportions)) .mi_stop("`group` must contain one value per composition.")
  z <- transform_aoi_composition(x, "ilr")$transformed
  observed <- .mi_composition_pseudo_f(z, group_values)
  set.seed(seed)
  null <- replicate(as.integer(permutations), .mi_composition_pseudo_f(z, sample(group_values)))
  p_value <- (1 + sum(null >= observed)) / (length(null) + 1)
  centroids <- aggregate(as.data.frame(x$proportions), by = list(group = group_values), FUN = .mi_safe_mean)
  .mi_new(
    "eye_aoi_composition_comparison",
    observed = observed,
    null = null,
    p_value = p_value,
    centroids = centroids,
    method = method,
    summary = data.frame(statistic = observed, p_value = p_value, permutations = length(null)),
    status = "Group comparison of AOI compositions completed."
  )
}

#' Calculate user-defined AOI balance coordinates
#'
#' @param x AOI composition object or matrix.
#' @param balances Named list with numerator and denominator AOI names, or contrast matrix.
#' @return Data frame of balance coordinates.
#' @export
aoi_balance_coordinates <- function(x, balances) {
  composition <- if (inherits(x, "eye_aoi_composition")) x$proportions else .mi_close_composition(x)
  if (is.matrix(balances)) {
    if (nrow(balances) != ncol(composition)) .mi_stop("A balance matrix must have one row per AOI part.")
    out <- log(composition) %*% balances
    colnames(out) <- colnames(balances) %||% paste0("balance_", seq_len(ncol(out)))
    return(as.data.frame(out))
  }
  if (!is.list(balances) || !length(balances)) .mi_stop("`balances` must be a named list or matrix.")
  out <- vapply(balances, function(balance) {
    numerator <- balance$numerator %||% balance[[1L]]
    denominator <- balance$denominator %||% balance[[2L]]
    numerator <- intersect(as.character(numerator), colnames(composition))
    denominator <- intersect(as.character(denominator), colnames(composition))
    if (!length(numerator) || !length(denominator)) .mi_stop("Each balance requires valid numerator and denominator parts.")
    r <- length(numerator); s <- length(denominator)
    coefficient <- sqrt((r * s) / (r + s))
    coefficient * (rowMeans(log(composition[, numerator, drop = FALSE])) - rowMeans(log(composition[, denominator, drop = FALSE])))
  }, numeric(nrow(composition)))
  as.data.frame(out, check.names = FALSE)
}

#' @export
plot.eye_aoi_composition <- function(x, type = c("ternary", "balance_biplot", "variation_matrix", "trajectory"), group = NULL, ...) {
  type <- match.arg(type)
  composition <- x$proportions
  if (type == "ternary") {
    if (ncol(composition) != 3L) return(.mi_plot_empty("Ternary plots require exactly three AOI parts."))
    a <- composition[, 1L]; b <- composition[, 2L]; c <- composition[, 3L]
    xx <- b + 0.5 * c
    yy <- sqrt(3) / 2 * c
    graphics::plot(c(0, 1, 0.5, 0), c(0, 0, sqrt(3) / 2, 0), type = "l", asp = 1,
                   axes = FALSE, xlab = "", ylab = "", main = "AOI dwell composition")
    graphics::points(xx, yy, pch = 16)
    graphics::text(c(0, 1, 0.5), c(0, 0, sqrt(3) / 2), labels = colnames(composition), pos = c(2, 4, 3))
  } else if (type == "balance_biplot") {
    z <- transform_aoi_composition(x, "clr")$transformed
    pca <- stats::prcomp(z, center = TRUE, scale. = FALSE)
    stats::biplot(pca, main = "AOI compositional balance biplot")
  } else if (type == "variation_matrix") {
    log_comp <- log(composition)
    variation <- matrix(0, ncol(composition), ncol(composition), dimnames = list(colnames(composition), colnames(composition)))
    for (i in seq_len(ncol(composition))) for (j in seq_len(ncol(composition))) variation[i, j] <- stats::var(log_comp[, i] - log_comp[, j])
    graphics::image(seq_len(nrow(variation)), seq_len(ncol(variation)), variation, axes = FALSE,
                    xlab = "AOI part", ylab = "AOI part", main = "AOI log-ratio variation matrix")
    graphics::axis(1, at = seq_len(nrow(variation)), labels = rownames(variation), las = 2)
    graphics::axis(2, at = seq_len(ncol(variation)), labels = colnames(variation), las = 2)
  } else {
    graphics::matplot(seq_len(nrow(composition)), composition, type = "l", lty = 1,
                      xlab = "Ordered observation", ylab = "AOI proportion", main = "AOI composition trajectory")
    graphics::legend("topright", legend = colnames(composition), lty = 1, bty = "n")
  }
  invisible(x)
}

#' @export
plot.eye_aoi_composition_comparison <- function(x, type = c("group_difference", "null"), ...) {
  type <- match.arg(type)
  if (type == "group_difference") {
    values <- as.matrix(x$centroids[, setdiff(names(x$centroids), "group"), drop = FALSE])
    graphics::barplot(t(values), beside = TRUE, names.arg = x$centroids$group, las = 2,
                      ylab = "Mean AOI proportion", main = "Compositional group differences")
    graphics::legend("topright", legend = colnames(values), fill = seq_len(ncol(values)), bty = "n")
  } else {
    graphics::hist(x$null, main = "Permutation reference distribution", xlab = "Pseudo-F statistic")
    graphics::abline(v = x$observed, lty = 2, lwd = 2)
  }
  invisible(x)
}

#' @export
plot.eye_aoi_composition_model <- function(x, type = c("diagnostics", "effects"), ...) {
  type <- match.arg(type)
  if (type == "diagnostics") {
    graphics::plot(stats::fitted(x$model), stats::residuals(x$model), xlab = "Fitted", ylab = "Residual",
                   main = "Compositional model residuals")
    graphics::abline(h = 0, lty = 2)
  } else {
    coefficients <- stats::coef(x$model)
    graphics::barplot(coefficients, las = 2, ylab = "Coefficient", main = "Compositional model effects")
  }
  invisible(x)
}

#' @export
plot_aoi_ternary <- function(x, ...) plot(x, type = "ternary", ...)
#' @export
plot_aoi_balance_biplot <- function(x, ...) plot(x, type = "balance_biplot", ...)
#' @export
plot_aoi_variation_matrix <- function(x, ...) plot(x, type = "variation_matrix", ...)
#' @export
plot_compositional_group_difference <- function(x, ...) plot(x, type = "group_difference", ...)
#' @export
plot_aoi_composition_trajectory <- function(x, ...) plot(x, type = "trajectory", ...)
