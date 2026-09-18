#' Plot AOI perturbations
#'
#' Draw nominal and perturbed rectangle/polygon boundaries. When row-level
#' data are supplied for an eye_aoi_sensitivity object, observations whose
#' assignment changed in the selected branch are marked explicitly.
#' @export
plot_aoi_perturbations <- function(
    x, perturbation_id = NULL, data = NULL, x_col = NULL, y_col = NULL, ...) {
  nominal <- if (!is.null(x$nominal_aois)) x$nominal_aois else x$nominal_geometry
  if (!is.null(x$grid_result)) {
    ids <- setdiff(names(x$grid_result$geometries), "baseline")
    pid <- if (is.null(perturbation_id)) if (length(ids)) ids[1L] else "baseline" else perturbation_id
    if (!pid %in% names(x$grid_result$geometries)) .aoi_stop("Unknown perturbation_id: ", pid)
    geom <- x$grid_result$geometries[[pid]]
  } else {
    pid <- x$perturbation_id
    geom <- x$perturbed_geometry
  }

  bounds <- rbind(
    do.call(rbind, lapply(seq_len(nrow(nominal)), function(i) .aoi_bounds(nominal[i, , drop = FALSE]))),
    do.call(rbind, lapply(seq_len(nrow(geom)), function(i) .aoi_bounds(geom[i, , drop = FALSE])))
  )
  graphics::plot(
    range(bounds[, 1:2]), range(bounds[, 3:4]), type = "n", asp = 1,
    xlab = if (is.null(x_col)) "x" else x_col,
    ylab = if (is.null(y_col)) "y" else y_col,
    main = paste("AOI perturbation:", pid)
  )

  draw_one <- function(row, border, lty = 1, lwd = 1) {
    if (identical(row$shape_type, "polygon")) {
      p <- .aoi_polygon(row$polygon[[1L]])
      graphics::polygon(p[, 1L], p[, 2L], border = border, lty = lty, lwd = lwd)
      graphics::text(mean(p[, 1L]), mean(p[, 2L]), labels = row$aoi_id, cex = 0.75)
    } else {
      graphics::rect(row$xmin, row$ymin, row$xmax, row$ymax, border = border, lty = lty, lwd = lwd)
      graphics::text((row$xmin + row$xmax) / 2, (row$ymin + row$ymax) / 2, labels = row$aoi_id, cex = 0.75)
    }
  }
  for (i in seq_len(nrow(nominal))) draw_one(nominal[i, , drop = FALSE], border = 2, lty = 2)
  for (i in seq_len(nrow(geom))) draw_one(geom[i, , drop = FALSE], border = 1, lwd = 2)

  if (!is.null(data)) {
    if (is.null(x_col) || is.null(y_col) || !all(c(x_col, y_col) %in% names(data))) {
      .aoi_stop("Supply valid x_col and y_col when plotting observations.")
    }
    changed <- rep(FALSE, nrow(data))
    if (!is.null(x$assignments) && "baseline" %in% names(x$assignments) && pid %in% names(x$assignments)) {
      base <- x$assignments$baseline
      alt <- x$assignments[[pid]]
      changed <- !(is.na(base) & is.na(alt)) & (is.na(base) | is.na(alt) | base != alt)
    }
    graphics::points(data[[x_col]][!changed], data[[y_col]][!changed], pch = 16, cex = 0.45)
    if (any(changed)) graphics::points(data[[x_col]][changed], data[[y_col]][changed], pch = 4, cex = 0.8)
  }
  invisible(x)
}

#' Plot AOI assignment stability
#' @export
plot_aoi_assignment_stability <- function(x, ...) {
  d <- if (!is.null(x$stability)) x$stability$overall else x$overall
  if (is.null(d) || !nrow(d)) .aoi_stop("No assignment-stability rows are available.")
  graphics::plot(
    seq_len(nrow(d)), d$proportion_unchanged, type = "b", xaxt = "n", ylim = c(0, 1),
    xlab = "", ylab = "Proportion unchanged", main = "AOI assignment stability"
  )
  graphics::axis(1, at = seq_len(nrow(d)), labels = d$perturbation_id, las = 2)
  invisible(x)
}

#' Plot AOI coefficient stability
#' @export
plot_aoi_coefficient_stability <- function(x, term, ...) {
  d <- x$models[as.character(x$models$term) == as.character(term), , drop = FALSE]
  if (!nrow(d)) .aoi_stop("No coefficient rows found for term ", term, ".")
  est <- as.numeric(d$estimate); lo <- as.numeric(d$CI_low); hi <- as.numeric(d$CI_high)
  limits <- range(c(lo, hi, est), na.rm = TRUE)
  if (!all(is.finite(limits))) .aoi_stop("Coefficient/interval values are not finite enough to plot.")
  graphics::plot(
    seq_along(est), est, type = "b", xaxt = "n", xlab = "", ylab = "Coefficient estimate",
    main = paste("Coefficient stability:", term), ylim = limits
  )
  graphics::segments(seq_along(est), lo, seq_along(est), hi)
  graphics::abline(h = 0, lty = 2)
  bad <- is.na(d$model_converged) | !as.logical(d$model_converged)
  if (any(bad)) graphics::points(which(bad), est[bad], pch = 4, cex = 1.2)
  graphics::axis(1, at = seq_along(est), labels = d$perturbation_id, las = 2)
  invisible(x)
}

#' Plot AOI robustness surface
#' @export
plot_aoi_robustness_surface <- function(
    x, value_col = "proportion_unchanged", x_col = "margin_x", y_col = "margin_y", ...) {
  d <- merge(x$stability$overall, x$grid$table, by = "perturbation_id", all.x = TRUE, sort = FALSE)
  if (!all(c(value_col, x_col, y_col) %in% names(d))) .aoi_stop("Requested robustness-surface columns are unavailable.")
  keep <- is.finite(as.numeric(d[[x_col]])) & is.finite(as.numeric(d[[y_col]])) & is.finite(as.numeric(d[[value_col]]))
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) .aoi_stop("Robustness surface has no finite x/y/value combinations.")
  xs <- sort(unique(as.numeric(d[[x_col]]))); ys <- sort(unique(as.numeric(d[[y_col]])))
  z <- matrix(NA_real_, length(xs), length(ys))
  for (i in seq_along(xs)) for (j in seq_along(ys)) {
    v <- as.numeric(d[d[[x_col]] == xs[i] & d[[y_col]] == ys[j], value_col])
    if (length(v)) z[i, j] <- mean(v, na.rm = TRUE)
  }
  graphics::image(xs, ys, z, xlab = x_col, ylab = y_col, main = "AOI robustness surface")
  invisible(x)
}
