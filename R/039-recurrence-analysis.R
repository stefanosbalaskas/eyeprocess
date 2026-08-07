# Recurrence and cross-recurrence analysis -----------------------------------

.mi_diagonal_lengths <- function(matrix, direction = c("diagonal", "vertical")) {
  direction <- match.arg(direction)
  lengths <- integer()
  n <- nrow(matrix); m <- ncol(matrix)
  if (direction == "diagonal") {
    offsets <- seq(-(n - 1L), m - 1L)
    for (offset in offsets) {
      rows <- seq_len(n)
      columns <- rows + offset
      keep <- columns >= 1L & columns <= m
      values <- matrix[cbind(rows[keep], columns[keep])]
      runs <- rle(as.logical(values)); lengths <- c(lengths, runs$lengths[runs$values])
    }
  } else {
    for (j in seq_len(m)) {
      runs <- rle(as.logical(matrix[, j])); lengths <- c(lengths, runs$lengths[runs$values])
    }
  }
  lengths
}

.mi_recurrence_matrix <- function(a, b = NULL, representation = "coordinates", radius = NULL) {
  if (is.null(b)) b <- a
  if (representation == "aoi") {
    return(outer(as.character(a), as.character(b), FUN = "==") * 1L)
  }
  a <- as.matrix(a); b <- as.matrix(b)
  distance <- matrix(0, nrow(a), nrow(b))
  for (i in seq_len(nrow(a))) for (j in seq_len(nrow(b))) distance[i, j] <- sqrt(sum((a[i, ] - b[j, ])^2, na.rm = TRUE))
  radius <- radius %||% .mi_safe_quantile(distance[is.finite(distance) & distance > 0], 0.1, default = 0)
  (distance <= radius) * 1L
}

#' Compute gaze recurrence
#'
#' @param x Coordinates, AOI sequence, or data frame.
#' @param representation Coordinates, AOI, or velocity.
#' @param x_col,y_col,aoi_col Input columns.
#' @param radius Recurrence radius.
#' @return An `eye_recurrence` object.
#' @export
gaze_recurrence <- function(x, representation = c("coordinates", "aoi", "velocity"), x_col = "x", y_col = "y", aoi_col = "aoi", radius = NULL) {
  representation <- match.arg(representation)
  series <- if (is.data.frame(x)) {
    if (representation == "aoi") x[[aoi_col]] else as.matrix(x[, c(x_col, y_col), drop = FALSE])
  } else x
  if (representation == "velocity") {
    matrix <- as.matrix(series)
    series <- rbind(rep(0, ncol(matrix)), apply(matrix, 2L, diff))
  }
  recurrence <- .mi_recurrence_matrix(series, representation = if (representation == "aoi") "aoi" else "coordinates", radius = radius)
  features <- recurrence_features(recurrence)
  .mi_new("eye_recurrence", matrix = recurrence, series = series, representation = representation, radius = radius, summary = features, status = "Gaze recurrence matrix calculated.")
}

#' Compute cross-recurrence between two signals
#'
#' @param x,y Signal series.
#' @param channels Channel declaration.
#' @param radius Recurrence radius.
#' @return An `eye_cross_recurrence` object.
#' @export
cross_recurrence <- function(x, y, channels = c("gaze_pupil", "gaze_eda", "pupil_eda"), radius = NULL) {
  channels <- match.arg(channels)
  standardise_matrix <- function(value) {
    value <- as.matrix(value)
    if (!nrow(value) || !ncol(value)) .mi_stop("Cross-recurrence inputs must contain observations and at least one channel.")
    out <- vapply(seq_len(ncol(value)), function(j) .mi_z(value[, j]), numeric(nrow(value)))
    matrix(out, nrow = nrow(value), ncol = ncol(value), dimnames = dimnames(value))
  }
  a <- standardise_matrix(x)
  b <- standardise_matrix(y)
  dimensions <- min(ncol(a), ncol(b))
  a <- a[, seq_len(dimensions), drop = FALSE]
  b <- b[, seq_len(dimensions), drop = FALSE]
  recurrence <- .mi_recurrence_matrix(a, b, representation = "coordinates", radius = radius)
  .mi_new("eye_cross_recurrence", matrix = recurrence, x = x, y = y, channels = channels, radius = radius, summary = recurrence_features(recurrence), status = "Cross-recurrence matrix calculated.")
}

#' Compute windowed recurrence features
#'
#' @param x Recurrence object or raw series.
#' @param window Window length in observations.
#' @param step Window step.
#' @return An `eye_windowed_recurrence` object.
#' @export
windowed_recurrence <- function(x, window, step) {
  series <- if (inherits(x, "eye_recurrence")) x$series else x
  n <- if (is.matrix(series)) nrow(series) else length(series)
  window <- as.integer(window)
  step <- as.integer(step)
  if (!is.finite(window) || window < 2L) .mi_stop("`window` must be an integer of at least 2.")
  if (!is.finite(step) || step < 1L) .mi_stop("`step` must be a positive integer.")
  if (n < 2L) .mi_stop("At least two observations are required for windowed recurrence.")
  window <- min(window, n)
  starts <- seq.int(1L, max(1L, n - window + 1L), by = step)
  rows <- lapply(starts, function(start) {
    index <- start:min(n, start + window - 1L)
    representation <- if (is.character(series)) "aoi" else "coordinates"
    matrix <- .mi_recurrence_matrix(if (is.matrix(series)) series[index, , drop = FALSE] else series[index], representation = representation)
    cbind(data.frame(start = start, end = max(index)), recurrence_features(matrix))
  })
  summary <- do.call(rbind, rows)
  .mi_new("eye_windowed_recurrence", series = series, window = window, step = step, summary = summary, status = "Windowed recurrence features calculated.")
}

#' Extract recurrence-quantification features
#'
#' @param x Recurrence matrix or recurrence object.
#' @param minimum_line Minimum line length.
#' @return One-row feature data frame.
#' @export
recurrence_features <- function(x, minimum_line = 2L) {
  matrix <- if (inherits(x, c("eye_recurrence", "eye_cross_recurrence"))) x$matrix else as.matrix(x)
  diagonal <- .mi_diagonal_lengths(matrix, "diagonal")
  vertical <- .mi_diagonal_lengths(matrix, "vertical")
  recurrent <- sum(matrix > 0, na.rm = TRUE)
  determinism <- if (recurrent) sum(diagonal[diagonal >= minimum_line]) / recurrent else 0
  laminarity <- if (recurrent) sum(vertical[vertical >= minimum_line]) / recurrent else 0
  diagonal_use <- diagonal[diagonal >= minimum_line]
  probabilities <- if (length(diagonal_use)) table(diagonal_use) / length(diagonal_use) else numeric()
  data.frame(
    recurrence_rate = mean(matrix > 0, na.rm = TRUE),
    determinism = determinism,
    laminarity = laminarity,
    trapping_time = if (any(vertical >= minimum_line)) mean(vertical[vertical >= minimum_line]) else 0,
    diagonal_entropy = .mi_entropy(as.numeric(probabilities)),
    stringsAsFactors = FALSE
  )
}

#' @export
plot.eye_recurrence <- function(x, type = c("matrix", "diagonal_profile", "network", "diagnostics"), ...) {
  type <- match.arg(type)
  if (type == "matrix" || type == "diagnostics") {
    graphics::image(t(x$matrix[nrow(x$matrix):1L, , drop = FALSE]), axes = FALSE, xlab = "Time", ylab = "Time", main = "Gaze recurrence matrix")
  } else if (type == "diagonal_profile") {
    profile <- vapply(seq(-(nrow(x$matrix) - 1L), ncol(x$matrix) - 1L), function(offset) {
      row <- seq_len(nrow(x$matrix)); col <- row + offset; keep <- col >= 1L & col <= ncol(x$matrix); mean(x$matrix[cbind(row[keep], col[keep])])
    }, numeric(1))
    graphics::plot(profile, type = "h", xlab = "Diagonal offset", ylab = "Recurrence", main = "Diagonal recurrence profile")
  } else {
    degree <- rowSums(x$matrix)
    graphics::plot(seq_along(degree), degree, type = "h", xlab = "State index", ylab = "Recurrence degree", main = "Recurrence network degree")
  }
  invisible(x)
}
#' @export
plot.eye_cross_recurrence <- function(x, type = c("crossmodal", "matrix", "diagnostics"), ...) {
  graphics::image(t(x$matrix[nrow(x$matrix):1L, , drop = FALSE]), axes = FALSE, xlab = "Signal x", ylab = "Signal y", main = paste("Cross-recurrence:", x$channels))
  invisible(x)
}
#' @export
plot.eye_windowed_recurrence <- function(x, type = c("windowed", "diagnostics"), metric = "recurrence_rate", ...) {
  metric <- if (metric %in% names(x$summary)) metric else "recurrence_rate"
  graphics::plot(x$summary$start, x$summary[[metric]], type = "b", xlab = "Window start", ylab = metric, main = "Windowed recurrence")
  invisible(x)
}
#' @export
plot_recurrence_matrix <- function(x, ...) plot(x, type = "matrix", ...)
#' @export
plot_windowed_recurrence <- function(x, ...) plot(x, type = "windowed", ...)
#' @export
plot_diagonal_recurrence_profile <- function(x, ...) plot(x, type = "diagonal_profile", ...)
#' @export
plot_crossmodal_recurrence <- function(x, ...) plot(x, type = "crossmodal", ...)
#' @export
plot_recurrence_network <- function(x, ...) plot(x, type = "network", ...)
