# Shared infrastructure for measurement-intelligence modules -----------------

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || !length(x) || all(is.na(x))) y else x
  }
}

.mi_stop <- function(message, class = "eyeprocess_measurement_intelligence_error") {
  condition <- structure(
    list(message = as.character(message), call = NULL),
    class = c(class, "error", "condition")
  )
  stop(condition)
}

.mi_warn <- function(message, class = "eyeprocess_measurement_intelligence_warning") {
  condition <- structure(
    list(message = as.character(message), call = NULL),
    class = c(class, "warning", "condition")
  )
  warning(condition)
}

.mi_assert_data <- function(x, min_rows = 1L, name = "x") {
  if (!is.data.frame(x)) .mi_stop(sprintf("`%s` must be a data frame.", name))
  if (nrow(x) < min_rows) .mi_stop(sprintf("`%s` must contain at least %d row(s).", name, min_rows))
  invisible(x)
}

.mi_assert_columns <- function(x, columns, name = "x") {
  missing <- setdiff(columns, names(x))
  if (length(missing)) {
    .mi_stop(sprintf("`%s` is missing required column(s): %s.", name, paste(missing, collapse = ", ")))
  }
  invisible(x)
}

.mi_first_column <- function(x, candidates, required = TRUE, label = "column") {
  found <- candidates[candidates %in% names(x)]
  if (length(found)) return(found[[1L]])
  if (required) .mi_stop(sprintf("Could not identify %s. Tried: %s.", label, paste(candidates, collapse = ", ")))
  NA_character_
}

.mi_numeric <- function(x) {
  original_names <- names(x)
  out <- if (is.numeric(x)) as.numeric(x) else suppressWarnings(as.numeric(as.character(x)))
  if (!is.null(original_names) && length(original_names) == length(out)) names(out) <- original_names
  out
}

.mi_safe_mean <- function(x) {
  x <- .mi_numeric(x)
  if (!length(x) || all(!is.finite(x))) return(NA_real_)
  mean(x[is.finite(x)])
}

.mi_safe_sd <- function(x) {
  x <- .mi_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

.mi_safe_quantile <- function(x, probability, default = NA_real_) {
  x <- .mi_numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(default)
  as.numeric(stats::quantile(x, probability, names = FALSE, na.rm = TRUE, type = 8L))
}

.mi_rescale01 <- function(x) {
  x <- .mi_numeric(x)
  range <- range(x, finite = TRUE)
  if (!all(is.finite(range)) || diff(range) == 0) return(rep(0.5, length(x)))
  (x - range[[1L]]) / diff(range)
}

.mi_z <- function(x) {
  x <- .mi_numeric(x)
  center <- .mi_safe_mean(x)
  scale <- .mi_safe_sd(x)
  if (!is.finite(scale) || scale == 0) return(rep(0, length(x)))
  (x - center) / scale
}

.mi_softmax <- function(logits) {
  logits <- as.matrix(logits)
  if (!nrow(logits) || !ncol(logits)) {
    return(matrix(numeric(), nrow = nrow(logits), ncol = ncol(logits), dimnames = dimnames(logits)))
  }
  row_max <- apply(logits, 1L, function(row) {
    finite <- row[is.finite(row)]
    if (!length(finite)) 0 else max(finite)
  })
  shifted <- sweep(logits, 1L, row_max, FUN = "-")
  finite <- is.finite(shifted)
  shifted[finite & shifted > 700] <- 700
  shifted[finite & shifted < -700] <- -700
  values <- exp(shifted)
  totals <- rowSums(values, na.rm = TRUE)
  totals[!is.finite(totals) | totals <= 0] <- 1
  out <- sweep(values, 1L, totals, FUN = "/")
  dimnames(out) <- dimnames(logits)
  out
}

.mi_entropy <- function(probabilities) {
  p <- .mi_numeric(probabilities)
  p <- p[is.finite(p) & p > 0]
  if (!length(p)) return(0)
  p <- p / sum(p)
  -sum(p * log(p))
}

.mi_new <- function(class, ..., call = match.call()) {
  out <- list(...)
  out$call <- call
  class(out) <- c(class, "eye_mi_result")
  out
}

.mi_table <- function(x, digits = 4L, max_rows = 12L) {
  if (is.null(x)) return(invisible(NULL))
  x <- as.data.frame(x)
  numeric_columns <- vapply(x, is.numeric, logical(1))
  x[numeric_columns] <- lapply(x[numeric_columns], round, digits = digits)
  print(utils::head(x, max_rows), row.names = FALSE)
  if (nrow(x) > max_rows) cat(sprintf("... %d additional row(s)\n", nrow(x) - max_rows))
  invisible(x)
}

.mi_match_method <- function(value, choices, argument = "method") {
  if (length(value) != 1L || is.na(value)) value <- choices[[1L]]
  match.arg(value, choices)
}

.mi_formula <- function(response, predictors) {
  predictors <- unique(as.character(predictors))
  predictors <- predictors[nzchar(predictors)]
  if (!length(predictors)) return(stats::as.formula(paste(response, "~ 1")))
  stats::as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
}

.mi_complete <- function(x, columns) {
  .mi_assert_columns(x, columns)
  x[stats::complete.cases(x[, columns, drop = FALSE]), , drop = FALSE]
}

.mi_plot_empty <- function(message) {
  graphics::plot.new()
  graphics::text(0.5, 0.5, message)
  invisible(NULL)
}

.mi_distance_matrix <- function(paths, distance_function) {
  n <- length(paths)
  out <- matrix(0, nrow = n, ncol = n)
  if (n > 1L) {
    for (i in seq_len(n - 1L)) {
      for (j in (i + 1L):n) {
        value <- distance_function(paths[[i]], paths[[j]])
        out[i, j] <- value
        out[j, i] <- value
      }
    }
  }
  names <- names(paths)
  if (!is.null(names)) dimnames(out) <- list(names, names)
  out
}

.mi_edit_distance <- function(a, b) {
  a <- as.character(a)
  b <- as.character(b)
  n <- length(a)
  m <- length(b)
  d <- matrix(0L, nrow = n + 1L, ncol = m + 1L)
  d[, 1L] <- 0:n
  d[1L, ] <- 0:m
  if (n && m) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        substitution <- if (identical(a[[i]], b[[j]])) 0L else 1L
        d[i + 1L, j + 1L] <- min(
          d[i, j + 1L] + 1L,
          d[i + 1L, j] + 1L,
          d[i, j] + substitution
        )
      }
    }
  }
  as.numeric(d[n + 1L, m + 1L])
}

.mi_dtw_distance <- function(a, b) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  if (!nrow(a) || !nrow(b)) return(Inf)
  d <- matrix(Inf, nrow = nrow(a) + 1L, ncol = nrow(b) + 1L)
  d[1L, 1L] <- 0
  for (i in seq_len(nrow(a))) {
    for (j in seq_len(nrow(b))) {
      cost <- sqrt(sum((a[i, , drop = TRUE] - b[j, , drop = TRUE])^2, na.rm = TRUE))
      d[i + 1L, j + 1L] <- cost + min(d[i, j + 1L], d[i + 1L, j], d[i, j])
    }
  }
  d[nrow(a) + 1L, nrow(b) + 1L] / (nrow(a) + nrow(b))
}

.mi_roll_groups <- function(time, window) {
  time <- .mi_numeric(time)
  if (is.character(window)) {
    number <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", window)))
    if (!is.finite(number)) number <- 30
    if (grepl("min", window, ignore.case = TRUE)) number <- number * 60
    if (grepl("ms", window, ignore.case = TRUE)) number <- number / 1000
    window <- number
  }
  window <- as.numeric(window)
  if (!is.finite(window) || window <= 0) .mi_stop("`window` must be positive.")
  floor((time - min(time, na.rm = TRUE)) / window) + 1L
}

#' Create a shared eyeprocess plot specification
#'
#' @param type Plot type.
#' @param title Plot title.
#' @param xlab,ylabel Axis labels.
#' @param caption Optional interpretation note.
#' @param show_uncertainty Show uncertainty where available.
#' @param show_raw Show raw observations where available.
#' @param facet_by Optional grouping variable.
#' @param label_items Label item identifiers.
#' @param interactive Reserved for downstream interactive adapters.
#' @return An `eye_plot_spec` object.
#' @export
#' @noRd
eye_plot_spec <- function(
    type = "default",
    title = NULL,
    xlab = NULL,
    ylab = NULL,
    caption = NULL,
    show_uncertainty = TRUE,
    show_raw = TRUE,
    facet_by = NULL,
    label_items = FALSE,
    interactive = FALSE) {
  out <- list(
    type = as.character(type), title = title, xlab = xlab, ylab = ylab,
    caption = caption, show_uncertainty = isTRUE(show_uncertainty),
    show_raw = isTRUE(show_raw), facet_by = facet_by,
    label_items = isTRUE(label_items), interactive = isTRUE(interactive)
  )
  class(out) <- "eye_plot_spec"
  out
}

#' @export
print.eye_plot_spec <- function(x, ...) {
  cat("eyeprocess plot specification\n")
  cat("Type: ", x$type, "\n", sep = "")
  if (!is.null(x$title)) cat("Title: ", x$title, "\n", sep = "")
  if (!is.null(x$caption)) cat("Note: ", x$caption, "\n", sep = "")
  invisible(x)
}

#' Print a measurement-intelligence result
#'
#' @param x Result object.
#' @param ... Unused.
#' @export
#' @noRd
print.eye_mi_result <- function(x, ...) {
  primary <- class(x)[[1L]]
  cat(gsub("_", " ", primary), "\n", sep = "")
  if (!is.null(x$status)) cat("Status: ", x$status, "\n", sep = "")
  if (!is.null(x$summary)) .mi_table(x$summary)
  invisible(x)
}

#' Plot diagnostic evidence
#'
#' @param x An eyeprocess result.
#' @param ... Arguments passed to `plot()`.
#' @export
#' @noRd
plot_diagnostics <- function(x, ...) UseMethod("plot_diagnostics")

.mi_plot_with_fallback <- function(x, type, ...) {
  tryCatch(
    plot(x, type = type, ...),
    error = function(error) {
      .mi_warn(sprintf("Plot type `%s` is not specialized for class `%s`; using the default plot for that result.", type, class(x)[[1L]]))
      plot(x, ...)
    }
  )
  invisible(x)
}

#' @export
plot_diagnostics.default <- function(x, ...) .mi_plot_with_fallback(x, "diagnostics", ...)

#' @export
plot_diagnostics.eye_mi_result <- function(x, ...) .mi_plot_with_fallback(x, "diagnostics", ...)

#' Plot scientific evidence
#'
#' @param x An eyeprocess result.
#' @param ... Arguments passed to `plot()`.
#' @export
#' @noRd
plot_evidence <- function(x, ...) UseMethod("plot_evidence")

#' @export
plot_evidence.default <- function(x, ...) .mi_plot_with_fallback(x, "evidence", ...)

#' @export
plot_evidence.eye_mi_result <- function(x, ...) .mi_plot_with_fallback(x, "evidence", ...)

#' Plot sensitivity results
#'
#' @param x An eyeprocess result.
#' @param ... Arguments passed to `plot()`.
#' @export
#' @noRd
plot_sensitivity <- function(x, ...) UseMethod("plot_sensitivity")

#' @export
plot_sensitivity.default <- function(x, ...) .mi_plot_with_fallback(x, "sensitivity", ...)

#' @export
plot_sensitivity.eye_mi_result <- function(x, ...) .mi_plot_with_fallback(x, "sensitivity", ...)

#' Autoplot-compatible eyeprocess wrapper
#'
#' @param object An eyeprocess result.
#' @param ... Arguments passed to `plot()`.
#' @return The plotted object, invisibly.
#' @export
#' @noRd
autoplot_eyeprocess <- function(object, ...) {
  plot(object, ...)
  invisible(object)
}
