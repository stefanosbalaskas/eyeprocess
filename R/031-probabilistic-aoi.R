# Probabilistic AOI assignment ------------------------------------------------

.mi_aoi_columns <- function(aois) {
  id <- .mi_first_column(aois, c("aoi_id", "aoi", "name", "label"), label = "AOI identifier")
  xmin <- .mi_first_column(aois, c("xmin", "x_min", "left"), label = "AOI xmin")
  xmax <- .mi_first_column(aois, c("xmax", "x_max", "right"), label = "AOI xmax")
  ymin <- .mi_first_column(aois, c("ymin", "y_min", "top"), label = "AOI ymin")
  ymax <- .mi_first_column(aois, c("ymax", "y_max", "bottom"), label = "AOI ymax")
  c(id = id, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

.mi_signed_rectangle_margin <- function(x, y, xmin, xmax, ymin, ymax) {
  inside_x <- x >= xmin & x <= xmax
  inside_y <- y >= ymin & y <= ymax
  inside <- inside_x & inside_y
  dx <- pmax(xmin - x, 0, x - xmax)
  dy <- pmax(ymin - y, 0, y - ymax)
  outside_distance <- sqrt(dx^2 + dy^2)
  inside_margin <- pmin(x - xmin, xmax - x, y - ymin, ymax - y)
  ifelse(inside, inside_margin, -outside_distance)
}

#' Assign gaze samples to AOIs probabilistically
#'
#' @param x Data frame containing gaze coordinates.
#' @param aois Rectangular AOI definition table.
#' @param error_model `"empirical"`, `"gaussian"`, or `"ellipse"`.
#' @param accuracy Optional scalar, two-element vector, or per-row calibration error.
#' @param precision Optional scalar or two-element spread parameter.
#' @param x_col,y_col Coordinate columns.
#' @param id_cols Optional columns retained in the membership table.
#' @return An `eye_probabilistic_aoi` object.
#' @export
#' @noRd
assign_aois_probabilistic <- function(
    x,
    aois,
    error_model = c("empirical", "gaussian", "ellipse"),
    accuracy = NULL,
    precision = NULL,
    x_col = NULL,
    y_col = NULL,
    id_cols = NULL) {
  .mi_assert_data(x)
  .mi_assert_data(aois)
  error_model <- match.arg(error_model)
  x_col <- x_col %||% .mi_first_column(x, c("x", "gaze_x", "x_norm", "x_px"), label = "gaze x coordinate")
  y_col <- y_col %||% .mi_first_column(x, c("y", "gaze_y", "y_norm", "y_px"), label = "gaze y coordinate")
  .mi_assert_columns(x, c(x_col, y_col))
  aoi_columns <- .mi_aoi_columns(aois)
  gx <- .mi_numeric(x[[x_col]])
  gy <- .mi_numeric(x[[y_col]])
  if (any(!is.finite(gx) | !is.finite(gy))) .mi_warn("Non-finite gaze coordinates receive outside-AOI probability one.")

  resolve_scale <- function(value, fallback) {
    if (is.null(value)) return(rep(fallback, 2L))
    value <- .mi_numeric(value)
    value <- value[is.finite(value) & value > 0]
    if (!length(value)) return(rep(fallback, 2L))
    if (length(value) == 1L) return(rep(value, 2L))
    value[1:2]
  }
  aoi_width <- abs(.mi_numeric(aois[[aoi_columns[["xmax"]]]]) - .mi_numeric(aois[[aoi_columns[["xmin"]]]]))
  aoi_height <- abs(.mi_numeric(aois[[aoi_columns[["ymax"]]]]) - .mi_numeric(aois[[aoi_columns[["ymin"]]]]))
  fallback <- max(1e-6, stats::median(c(aoi_width, aoi_height), na.rm = TRUE) / 8)
  spread <- resolve_scale(precision, fallback)
  bias <- if (is.null(accuracy)) c(0, 0) else {
    value <- .mi_numeric(accuracy)
    value <- value[is.finite(value)]
    if (!length(value)) c(0, 0) else if (length(value) == 1L) c(value, value) else value[1:2]
  }
  gx_adjusted <- gx - bias[[1L]]
  gy_adjusted <- gy - bias[[2L]]

  logits <- matrix(NA_real_, nrow = nrow(x), ncol = nrow(aois) + 1L)
  aoi_names <- as.character(aois[[aoi_columns[["id"]]]])
  for (j in seq_len(nrow(aois))) {
    margin <- .mi_signed_rectangle_margin(
      gx_adjusted, gy_adjusted,
      .mi_numeric(aois[[aoi_columns[["xmin"]]]][j]),
      .mi_numeric(aois[[aoi_columns[["xmax"]]]][j]),
      .mi_numeric(aois[[aoi_columns[["ymin"]]]][j]),
      .mi_numeric(aois[[aoi_columns[["ymax"]]]][j])
    )
    local_scale <- if (error_model == "ellipse") sqrt(spread[[1L]] * spread[[2L]]) else mean(spread)
    logits[, j] <- margin / max(local_scale, 1e-8)
  }
  logits[, ncol(logits)] <- 0
  probabilities <- .mi_softmax(logits)
  probabilities[!is.finite(gx) | !is.finite(gy), ] <- 0
  probabilities[!is.finite(gx) | !is.finite(gy), ncol(probabilities)] <- 1
  colnames(probabilities) <- c(aoi_names, "outside")

  retained <- unique(c(id_cols, x_col, y_col))
  retained <- retained[retained %in% names(x)]
  wide <- cbind(x[, retained, drop = FALSE], as.data.frame(probabilities, check.names = FALSE))
  sample_id <- seq_len(nrow(x))
  long <- data.frame(
    sample_id = rep(sample_id, each = ncol(probabilities)),
    aoi = rep(colnames(probabilities), times = nrow(probabilities)),
    probability = as.vector(t(probabilities)),
    stringsAsFactors = FALSE
  )
  if (length(id_cols)) {
    id_cols <- id_cols[id_cols %in% names(x)]
    long <- cbind(x[rep(sample_id, each = ncol(probabilities)), id_cols, drop = FALSE], long)
  }
  max_index <- max.col(probabilities, ties.method = "first")
  classification <- data.frame(
    sample_id = sample_id,
    most_likely_aoi = colnames(probabilities)[max_index],
    maximum_probability = probabilities[cbind(sample_id, max_index)],
    ambiguity = 1 - probabilities[cbind(sample_id, max_index)],
    membership_entropy = apply(probabilities, 1L, .mi_entropy),
    stringsAsFactors = FALSE
  )
  .mi_new(
    "eye_probabilistic_aoi",
    data = x,
    aois = aois,
    probabilities = probabilities,
    membership = long,
    wide = wide,
    classification = classification,
    coordinate_columns = c(x = x_col, y = y_col),
    error_model = error_model,
    spread = spread,
    accuracy = bias,
    status = "Probabilistic AOI membership estimated."
  )
}

#' Audit AOI overlap and boundary separation
#'
#' @param x Optional gaze data or probabilistic AOI object.
#' @param aois AOI definition table.
#' @return Pairwise AOI geometry audit.
#' @export
#' @noRd
audit_aoi_separation <- function(x = NULL, aois = NULL) {
  if (inherits(x, "eye_probabilistic_aoi")) {
    aois <- x$aois
    probability_summary <- data.frame(
      mean_maximum_probability = .mi_safe_mean(x$classification$maximum_probability),
      mean_ambiguity = .mi_safe_mean(x$classification$ambiguity),
      mean_membership_entropy = .mi_safe_mean(x$classification$membership_entropy),
      stringsAsFactors = FALSE
    )
  } else {
    probability_summary <- NULL
  }
  if (is.null(aois)) .mi_stop("Supply `aois` or an `eye_probabilistic_aoi` object.")
  .mi_assert_data(aois)
  columns <- .mi_aoi_columns(aois)
  ids <- as.character(aois[[columns[["id"]]]])
  rows <- list()
  k <- 1L
  if (nrow(aois) >= 2L) {
    for (i in seq_len(nrow(aois) - 1L)) {
      for (j in (i + 1L):nrow(aois)) {
        ax1 <- .mi_numeric(aois[[columns[["xmin"]]]][i]); ax2 <- .mi_numeric(aois[[columns[["xmax"]]]][i])
        ay1 <- .mi_numeric(aois[[columns[["ymin"]]]][i]); ay2 <- .mi_numeric(aois[[columns[["ymax"]]]][i])
        bx1 <- .mi_numeric(aois[[columns[["xmin"]]]][j]); bx2 <- .mi_numeric(aois[[columns[["xmax"]]]][j])
        by1 <- .mi_numeric(aois[[columns[["ymin"]]]][j]); by2 <- .mi_numeric(aois[[columns[["ymax"]]]][j])
        overlap_width <- max(0, min(ax2, bx2) - max(ax1, bx1))
        overlap_height <- max(0, min(ay2, by2) - max(ay1, by1))
        overlap_area <- overlap_width * overlap_height
        horizontal_gap <- max(0, max(ax1, bx1) - min(ax2, bx2))
        vertical_gap <- max(0, max(ay1, by1) - min(ay2, by2))
        gap <- sqrt(horizontal_gap^2 + vertical_gap^2)
        rows[[k]] <- data.frame(
          aoi_1 = ids[[i]], aoi_2 = ids[[j]], overlap_area = overlap_area,
          boundary_gap = gap,
          separation_status = if (overlap_area > 0) "overlap" else if (gap == 0) "touching" else "separated",
          stringsAsFactors = FALSE
        )
        k <- k + 1L
      }
    }
  }
  pairwise <- if (length(rows)) do.call(rbind, rows) else data.frame()
  .mi_new(
    "eye_aoi_separation_audit",
    pairwise = pairwise,
    probability_summary = probability_summary,
    summary = if (nrow(pairwise)) data.frame(
      pairs = nrow(pairwise),
      overlapping_pairs = sum(pairwise$separation_status == "overlap"),
      touching_pairs = sum(pairwise$separation_status == "touching"),
      minimum_gap = min(pairwise$boundary_gap),
      stringsAsFactors = FALSE
    ) else data.frame(pairs = 0L, overlapping_pairs = 0L, touching_pairs = 0L, minimum_gap = NA_real_),
    status = "AOI separation audit completed."
  )
}

#' Summarise probabilistic AOI membership
#'
#' @param x Probabilistic AOI object.
#' @param by Optional grouping columns stored in the membership table.
#' @return AOI probability summary.
#' @export
#' @noRd
summarise_aoi_membership <- function(x, by = NULL) {
  if (!inherits(x, "eye_probabilistic_aoi")) .mi_stop("`x` must be an `eye_probabilistic_aoi` object.")
  membership <- x$membership
  by <- by[by %in% names(membership)]
  groups <- c(by, "aoi")
  if (!length(by)) {
    out <- aggregate(probability ~ aoi, data = membership, FUN = .mi_safe_mean)
    names(out)[names(out) == "probability"] <- "mean_probability"
  } else {
    formula <- stats::as.formula(paste("probability ~", paste(groups, collapse = " + ")))
    out <- aggregate(formula, data = membership, FUN = .mi_safe_mean)
    names(out)[names(out) == "probability"] <- "mean_probability"
  }
  out
}

.mi_uncertain_aoi_metrics <- function(labels, data, time_col, duration_col, aoi_levels = NULL) {
  time <- if (!is.null(time_col) && time_col %in% names(data)) .mi_numeric(data[[time_col]]) else seq_along(labels)
  duration <- if (!is.null(duration_col) && duration_col %in% names(data)) .mi_numeric(data[[duration_col]]) else rep(1, length(labels))
  if (is.null(aoi_levels)) aoi_levels <- unique(labels)
  aoi_levels <- as.character(aoi_levels)
  aoi_names <- setdiff(aoi_levels, "outside")
  dwell <- setNames(vapply(aoi_names, function(aoi) sum(duration[labels == aoi], na.rm = TRUE), numeric(1)), aoi_names)
  ttff <- setNames(vapply(aoi_names, function(aoi) {
    values <- time[labels == aoi & is.finite(time)]
    if (!length(values)) NA_real_ else min(values) - min(time, na.rm = TRUE)
  }, numeric(1)), aoi_names)
  transitions <- if (length(labels) > 1L) sum(labels[-1L] != labels[-length(labels)], na.rm = TRUE) else 0
  proportions <- table(factor(labels, levels = aoi_levels)) / length(labels)
  c(
    setNames(dwell, paste0("dwell__", names(dwell))),
    setNames(ttff, paste0("ttff__", names(ttff))),
    transitions = transitions,
    entropy = .mi_entropy(as.numeric(proportions))
  )
}

#' Propagate AOI assignment uncertainty to process metrics
#'
#' @param x Probabilistic AOI object.
#' @param metrics Metrics to retain: dwell, TTFF, transitions, and entropy.
#' @param draws Number of Monte Carlo draws.
#' @param time_col,duration_col Optional timing columns.
#' @param seed Random seed.
#' @return An `eye_aoi_uncertainty` object.
#' @export
#' @noRd
propagate_aoi_uncertainty <- function(
    x,
    metrics = c("dwell", "ttff", "transitions", "entropy"),
    draws = 500,
    time_col = NULL,
    duration_col = NULL,
    seed = 20260807) {
  if (!inherits(x, "eye_probabilistic_aoi")) .mi_stop("`x` must be an `eye_probabilistic_aoi` object.")
  draws <- as.integer(draws)
  if (!is.finite(draws) || draws < 2L) .mi_stop("`draws` must be at least 2.")
  metrics <- intersect(metrics, c("dwell", "ttff", "transitions", "entropy"))
  if (!length(metrics)) .mi_stop("No supported metrics were selected.")
  set.seed(seed)
  probabilities <- x$probabilities
  labels <- colnames(probabilities)
  draw_rows <- vector("list", draws)
  for (d in seq_len(draws)) {
    sampled <- vapply(seq_len(nrow(probabilities)), function(i) {
      sample(labels, size = 1L, prob = probabilities[i, ])
    }, character(1))
    values <- .mi_uncertain_aoi_metrics(
      sampled, x$data, time_col, duration_col, aoi_levels = labels
    )
    keep <- rep(FALSE, length(values))
    names(keep) <- names(values)
    if ("dwell" %in% metrics) keep <- keep | startsWith(names(values), "dwell__")
    if ("ttff" %in% metrics) keep <- keep | startsWith(names(values), "ttff__")
    if ("transitions" %in% metrics) keep[names(values) == "transitions"] <- TRUE
    if ("entropy" %in% metrics) keep[names(values) == "entropy"] <- TRUE
    draw_rows[[d]] <- data.frame(draw = d, as.list(values[keep]), check.names = FALSE)
  }
  draw_table <- do.call(rbind, draw_rows)
  metric_names <- setdiff(names(draw_table), "draw")
  summary <- do.call(rbind, lapply(metric_names, function(metric) {
    values <- .mi_numeric(draw_table[[metric]])
    data.frame(
      metric = metric,
      mean = .mi_safe_mean(values),
      sd = .mi_safe_sd(values),
      lower = .mi_safe_quantile(values, 0.025),
      median = .mi_safe_quantile(values, 0.5),
      upper = .mi_safe_quantile(values, 0.975),
      stringsAsFactors = FALSE
    )
  }))
  .mi_new(
    "eye_aoi_uncertainty",
    source = x,
    draws = draw_table,
    summary = summary,
    metrics = metrics,
    seed = seed,
    status = "AOI metric uncertainty propagated by Monte Carlo sampling."
  )
}

#' @export
plot.eye_probabilistic_aoi <- function(x, type = c("probability_map", "boundary_risk", "scanpath"), ...) {
  type <- match.arg(type)
  coordinates <- x$coordinate_columns
  gx <- .mi_numeric(x$data[[coordinates[["x"]]]])
  gy <- .mi_numeric(x$data[[coordinates[["y"]]]])
  if (type == "probability_map") {
    graphics::plot(gx, gy, asp = 1, xlab = coordinates[["x"]], ylab = coordinates[["y"]],
                   main = "Probabilistic AOI assignment", cex = 0.5 + 1.5 * x$classification$maximum_probability)
    cols <- .mi_aoi_columns(x$aois)
    for (i in seq_len(nrow(x$aois))) {
      graphics::rect(
        .mi_numeric(x$aois[[cols[["xmin"]]]][i]), .mi_numeric(x$aois[[cols[["ymin"]]]][i]),
        .mi_numeric(x$aois[[cols[["xmax"]]]][i]), .mi_numeric(x$aois[[cols[["ymax"]]]][i]),
        border = i + 1L, lwd = 2
      )
      graphics::text(
        mean(c(.mi_numeric(x$aois[[cols[["xmin"]]]][i]), .mi_numeric(x$aois[[cols[["xmax"]]]][i]))),
        mean(c(.mi_numeric(x$aois[[cols[["ymin"]]]][i]), .mi_numeric(x$aois[[cols[["ymax"]]]][i]))),
        labels = as.character(x$aois[[cols[["id"]]]][i]), pos = 3
      )
    }
  } else if (type == "boundary_risk") {
    risk <- aggregate(ambiguity ~ most_likely_aoi, data = x$classification, FUN = .mi_safe_mean)
    graphics::barplot(risk$ambiguity, names.arg = risk$most_likely_aoi, las = 2,
                      ylab = "Mean assignment ambiguity", main = "AOI boundary risk")
  } else {
    graphics::plot(gx, gy, type = "o", asp = 1, xlab = coordinates[["x"]], ylab = coordinates[["y"]],
                   main = "Probabilistic scanpath")
  }
  invisible(x)
}

#' @export
plot.eye_aoi_separation_audit <- function(x, type = c("boundary_risk", "gap"), ...) {
  type <- match.arg(type)
  if (!nrow(x$pairwise)) return(.mi_plot_empty("No AOI pairs available."))
  value <- if (type == "boundary_risk") x$pairwise$overlap_area else x$pairwise$boundary_gap
  labels <- paste(x$pairwise$aoi_1, x$pairwise$aoi_2, sep = " - ")
  graphics::barplot(value, names.arg = labels, las = 2,
                    ylab = if (type == "boundary_risk") "Overlap area" else "Boundary gap",
                    main = "AOI separation audit")
  invisible(x)
}

#' @export
plot.eye_aoi_uncertainty <- function(x, type = c("metric_uncertainty", "fuzzy_transition"), ...) {
  type <- match.arg(type)
  if (type == "metric_uncertainty") {
    metrics <- setdiff(names(x$draws), "draw")
    values <- lapply(metrics, function(metric) .mi_numeric(x$draws[[metric]]))
    graphics::boxplot(values, names = metrics, las = 2, ylab = "Metric value", main = "AOI metric uncertainty")
  } else {
    source <- x$source
    p <- source$probabilities
    transition <- matrix(0, nrow = ncol(p), ncol = ncol(p), dimnames = list(colnames(p), colnames(p)))
    if (nrow(p) > 1L) {
      for (i in seq_len(nrow(p) - 1L)) transition <- transition + tcrossprod(p[i, ], p[i + 1L, ])
    }
    graphics::image(seq_len(nrow(transition)), seq_len(ncol(transition)), transition,
                    xlab = "From AOI", ylab = "To AOI", main = "Fuzzy transition matrix", axes = FALSE)
    graphics::axis(1, at = seq_len(nrow(transition)), labels = rownames(transition), las = 2)
    graphics::axis(2, at = seq_len(ncol(transition)), labels = colnames(transition), las = 2)
  }
  invisible(x)
}

#' Plot an AOI probability map
#' @param x Probabilistic AOI object.
#' @param ... Plot arguments.
#' @export
#' @noRd
plot_aoi_probability_map <- function(x, ...) plot(x, type = "probability_map", ...)

#' @export
plot_aoi_boundary_risk <- function(x, ...) {
  if (inherits(x, "eye_aoi_separation_audit")) plot(x, type = "boundary_risk", ...) else plot(x, type = "boundary_risk", ...)
}

#' @export
plot_probabilistic_scanpath <- function(x, ...) plot(x, type = "scanpath", ...)

#' @export
plot_fuzzy_transition_matrix <- function(x, ...) plot(x, type = "fuzzy_transition", ...)

#' @export
plot_aoi_metric_uncertainty <- function(x, ...) plot(x, type = "metric_uncertainty", ...)
