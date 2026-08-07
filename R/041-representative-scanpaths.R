# Representative scanpaths and distribution comparison -----------------------

.mi_scanpath_list <- function(x, id_col = "person_id", aoi_col = "aoi", x_col = "x", y_col = "y") {
  if (is.list(x) && !is.data.frame(x)) return(x)
  .mi_assert_data(x)
  .mi_assert_columns(x, id_col)
  split_data <- split(x, as.character(x[[id_col]]))
  if (aoi_col %in% names(x)) lapply(split_data, function(data) as.character(data[[aoi_col]])) else {
    .mi_assert_columns(x, c(x_col, y_col)); lapply(split_data, function(data) as.matrix(data[, c(x_col, y_col), drop = FALSE]))
  }
}

.mi_scanpath_distance_function <- function(distance, sequence) {
  if (distance == "edit" || sequence) return(.mi_edit_distance)
  if (distance == "transport") return(function(a, b) {
    a <- as.matrix(a); b <- as.matrix(b)
    probabilities <- seq(0, 1, length.out = 25)
    sum(abs(stats::quantile(a[, 1L], probabilities) - stats::quantile(b[, 1L], probabilities))) + sum(abs(stats::quantile(a[, 2L], probabilities) - stats::quantile(b[, 2L], probabilities)))
  })
  .mi_dtw_distance
}

#' Derive a representative scanpath
#'
#' @param x Scanpath list or long-format data.
#' @param method Medoid, barycenter, or consensus.
#' @param id_col,aoi_col,x_col,y_col Input columns.
#' @param distance MultiMatch-style DTW, edit, or transport distance.
#' @return An `eye_scanpath_representative` object.
#' @export
representative_scanpath <- function(x, method = c("medoid", "barycenter", "consensus"), id_col = "person_id", aoi_col = "aoi", x_col = "x", y_col = "y", distance = c("multimatch", "edit", "transport")) {
  method <- match.arg(method); distance <- match.arg(distance)
  paths <- .mi_scanpath_list(x, id_col, aoi_col, x_col, y_col)
  if (length(paths) < 1L) .mi_stop("No scanpaths are available.")
  sequence <- is.character(paths[[1L]])
  distance_function <- .mi_scanpath_distance_function(distance, sequence)
  distances <- .mi_distance_matrix(paths, distance_function)
  medoid_index <- which.min(rowMeans(distances))
  representative <- paths[[medoid_index]]
  if (method != "medoid") {
    max_length <- max(vapply(paths, length, integer(1)))
    if (sequence) {
      representative <- vapply(seq_len(max_length), function(position) {
        values <- vapply(paths, function(path) if (length(path) >= position) path[[position]] else NA_character_, character(1))
        values <- values[!is.na(values)]
        if (!length(values)) NA_character_ else names(sort(table(values), decreasing = TRUE))[[1L]]
      }, character(1))
      representative <- representative[!is.na(representative)]
    } else {
      common_length <- round(stats::median(vapply(paths, nrow, integer(1))))
      grid <- seq(0, 1, length.out = max(2L, common_length))
      aligned <- lapply(paths, function(path) {
        original <- seq(0, 1, length.out = nrow(path))
        cbind(stats::approx(original, path[, 1L], xout = grid, rule = 2)$y, stats::approx(original, path[, 2L], xout = grid, rule = 2)$y)
      })
      representative <- Reduce("+", aligned) / length(aligned)
    }
  }
  .mi_new(
    "eye_scanpath_representative",
    paths = paths,
    representative = representative,
    medoid_id = names(paths)[medoid_index] %||% medoid_index,
    distance_matrix = distances,
    method = method,
    distance = distance,
    sequence = sequence,
    summary = data.frame(n_paths = length(paths), mean_dispersion = mean(distances[upper.tri(distances)], na.rm = TRUE), representative_length = if (sequence) length(representative) else nrow(representative)),
    status = "Representative scanpath derived."
  )
}

#' Calculate scanpath dispersion
#'
#' @param x Representative object or scanpath list.
#' @return Scanpath dispersion summary.
#' @export
scanpath_dispersion <- function(x) {
  object <- if (inherits(x, "eye_scanpath_representative")) x else representative_scanpath(x)
  distances <- object$distance_matrix
  data.frame(
    scanpath = rownames(distances) %||% seq_len(nrow(distances)),
    mean_distance = rowMeans(distances),
    distance_to_representative = distances[, if (is.character(object$medoid_id)) match(object$medoid_id, rownames(distances)) else as.integer(object$medoid_id)],
    stringsAsFactors = FALSE
  )
}

#' Compare scanpath distributions across groups
#'
#' @param x Scanpath list or long-format data.
#' @param group Group vector or named group mapping.
#' @param distance Scanpath distance.
#' @param permutations Permutations.
#' @return An `eye_scanpath_comparison` object.
#' @export
compare_scanpath_distributions <- function(x, group, distance = c("multimatch", "edit", "transport"), permutations = 499) {
  object <- representative_scanpath(x, distance = match.arg(distance))
  groups <- if (!is.null(names(group))) group[names(object$paths)] else group
  if (length(groups) != length(object$paths)) .mi_stop("`group` must have one value per scanpath.")
  matrix <- object$distance_matrix
  statistic <- function(labels) {
    within <- matrix[outer(labels, labels, "==") & upper.tri(matrix)]
    between <- matrix[outer(labels, labels, "!=") & upper.tri(matrix)]
    mean(between, na.rm = TRUE) - mean(within, na.rm = TRUE)
  }
  observed <- statistic(groups)
  set.seed(20260807)
  null <- replicate(permutations, statistic(sample(groups)))
  p_value <- (1 + sum(null >= observed)) / (length(null) + 1)
  .mi_new("eye_scanpath_comparison", representative = object, groups = groups, observed = observed, null = null, p_value = p_value,
          summary = data.frame(statistic = observed, p_value = p_value, permutations = permutations), status = "Scanpath distributions compared by permutation.")
}

#' Bootstrap representative scanpaths
#'
#' @param x Representative object or scanpath input.
#' @param draws Bootstrap draws.
#' @param seed Seed.
#' @return An `eye_scanpath_bootstrap` object.
#' @export
bootstrap_representative_scanpath <- function(x, draws = 250, seed = 20260807) {
  object <- if (inherits(x, "eye_scanpath_representative")) x else representative_scanpath(x)
  set.seed(seed)
  medoids <- replicate(draws, {
    index <- sample(seq_along(object$paths), length(object$paths), replace = TRUE)
    sampled <- object$paths[index]; names(sampled) <- paste0(names(object$paths)[index], "_", seq_along(index))
    representative_scanpath(sampled, method = "medoid", distance = object$distance)$medoid_id
  })
  frequencies <- sort(table(medoids), decreasing = TRUE)
  .mi_new("eye_scanpath_bootstrap", source = object, medoid_frequencies = frequencies,
          summary = data.frame(medoid = names(frequencies), count = as.integer(frequencies), probability = as.numeric(frequencies) / sum(frequencies)), status = "Representative scanpath bootstrapped.")
}

#' @export
plot.eye_scanpath_representative <- function(x, type = c("atlas", "representative", "dispersion", "similarity_matrix", "diagnostics"), ...) {
  type <- match.arg(type)
  if (type == "similarity_matrix") {
    graphics::image(t(x$distance_matrix[nrow(x$distance_matrix):1L, , drop = FALSE]), axes = FALSE, main = "Scanpath distance matrix")
  } else if (type == "dispersion" || type == "diagnostics") {
    dispersion <- scanpath_dispersion(x)
    graphics::barplot(dispersion$mean_distance, names.arg = dispersion$scanpath, las = 2, ylab = "Mean distance", main = "Scanpath dispersion")
  } else if (x$sequence) {
    positions <- seq_along(x$representative)
    graphics::plot(positions, rep(1, length(positions)), type = "n", yaxt = "n", xlab = "Sequence position", ylab = "", main = "Representative AOI scanpath")
    graphics::text(positions, rep(1, length(positions)), labels = x$representative)
    if (length(positions) > 1L) graphics::arrows(positions[-length(positions)], rep(1, length(positions) - 1L), positions[-1L], rep(1, length(positions) - 1L), length = 0.05)
  } else {
    representative <- as.matrix(x$representative)
    graphics::plot(representative[, 1L], representative[, 2L], type = "o", asp = 1, xlab = "X", ylab = "Y", main = if (type == "atlas") "Scanpath atlas representative" else "Representative scanpath")
  }
  invisible(x)
}
#' @export
plot.eye_scanpath_comparison <- function(x, type = c("group_transport", "null"), ...) {
  type <- match.arg(type)
  if (type == "null") {
    graphics::hist(x$null, xlab = "Between-minus-within distance", main = "Scanpath permutation distribution"); graphics::abline(v = x$observed, lty = 2)
  } else {
    groups <- factor(x$groups); matrix <- x$representative$distance_matrix
    group_distance <- aggregate(rowMeans(matrix), by = list(group = groups), FUN = mean)
    graphics::barplot(group_distance$x, names.arg = group_distance$group, ylab = "Mean scanpath distance", main = "Group scanpath transport")
  }
  invisible(x)
}
#' @export
plot.eye_scanpath_bootstrap <- function(x, type = c("stability", "diagnostics"), ...) {
  graphics::barplot(x$summary$probability, names.arg = x$summary$medoid, las = 2, ylab = "Bootstrap probability", main = "Representative scanpath stability")
  invisible(x)
}
#' @export
plot_scanpath_atlas <- function(x, ...) plot(x, type = "atlas", ...)
#' @export
plot_representative_scanpath <- function(x, ...) plot(x, type = "representative", ...)
#' @export
plot_scanpath_dispersion <- function(x, ...) plot(x, type = "dispersion", ...)
#' @export
plot_group_scanpath_transport <- function(x, ...) plot(x, type = "group_transport", ...)
#' @export
plot_scanpath_similarity_matrix <- function(x, ...) plot(x, type = "similarity_matrix", ...)
