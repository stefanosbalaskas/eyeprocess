# Cognitive-episode change-point detection -----------------------------------

#' Detect multichannel process change points
#'
#' @param x Ordered process data.
#' @param channels Numeric channel columns.
#' @param time_col Optional time column.
#' @param window Local comparison window.
#' @param threshold_quantile Quantile used to select changes.
#' @param min_segment Minimum segment length.
#' @return An `eye_process_changepoints` object.
#' @export
detect_process_changepoints <- function(x, channels = c("gaze_velocity", "aoi", "pupil", "eda"), time_col = NULL, window = 10, threshold_quantile = 0.9, min_segment = 5) {
  window <- as.integer(window)
  if (!is.finite(window) || window < 2L) .mi_stop("`window` must be an integer of at least 2.")
  min_required <- max(10L, 2L * window + 1L)
  .mi_assert_data(x, min_rows = min_required)
  numeric_channels <- channels[channels %in% names(x) & vapply(x[channels[channels %in% names(x)]], is.numeric, logical(1))]
  if (!length(numeric_channels)) .mi_stop("At least one numeric process channel is required.")
  z <- sapply(x[, numeric_channels, drop = FALSE], .mi_z)
  z <- as.matrix(z)
  n <- nrow(z)
  score <- rep(NA_real_, n)
  candidate_indices <- seq.int(window + 1L, n - window)
  for (i in candidate_indices) {
    before <- colMeans(z[(i - window):(i - 1L), , drop = FALSE], na.rm = TRUE)
    after <- colMeans(z[i:(i + window - 1L), , drop = FALSE], na.rm = TRUE)
    score[[i]] <- sqrt(sum((after - before)^2, na.rm = TRUE))
  }
  threshold <- .mi_safe_quantile(score, threshold_quantile)
  candidates <- which(score >= threshold)
  selected <- integer()
  for (candidate in candidates[order(score[candidates], decreasing = TRUE)]) {
    if (!length(selected) || all(abs(candidate - selected) >= min_segment)) selected <- c(selected, candidate)
  }
  selected <- sort(selected)
  time <- if (!is.null(time_col) && time_col %in% names(x)) x[[time_col]] else seq_len(n)
  summary <- data.frame(index = selected, time = time[selected], score = score[selected], stringsAsFactors = FALSE)
  .mi_new("eye_process_changepoints", data = x, channels = numeric_channels, score = score, threshold = threshold, changepoints = selected, summary = summary, status = "Multichannel process change points detected.")
}

#' Segment observations into cognitive episodes
#'
#' @param x Change-point object or ordered data.
#' @param ... Passed to `detect_process_changepoints()` when needed.
#' @return An `eye_process_episodes` object.
#' @export
segment_process_episodes <- function(x, ...) {
  changes <- if (inherits(x, "eye_process_changepoints")) x else detect_process_changepoints(x, ...)
  n <- nrow(changes$data)
  breaks <- c(1L, changes$changepoints, n + 1L)
  episode_id <- integer(n)
  for (i in seq_len(length(breaks) - 1L)) episode_id[breaks[[i]]:(breaks[[i + 1L]] - 1L)] <- i
  data <- changes$data; data$episode_id <- episode_id
  episode_levels <- sort(unique(episode_id))
  summary <- data.frame(
    episode_id = episode_levels,
    n_observations = vapply(episode_levels, function(id) sum(episode_id == id), integer(1)),
    start_index = vapply(episode_levels, function(id) min(which(episode_id == id)), integer(1)),
    end_index = vapply(episode_levels, function(id) max(which(episode_id == id)), integer(1)),
    stringsAsFactors = FALSE
  )
  .mi_new("eye_process_episodes", changepoints = changes, data = data, summary = summary, status = "Process observations segmented into episodes.")
}

#' Label process episodes with transparent rules
#'
#' @param x Episode object.
#' @param rules Optional named rule function list.
#' @param model Optional external classifier function.
#' @return Updated `eye_process_episodes` object.
#' @export
label_process_episodes <- function(x, rules = NULL, model = NULL) {
  if (!inherits(x, "eye_process_episodes")) .mi_stop("`x` must be an `eye_process_episodes` object.")
  n <- nrow(x$summary)
  default_labels <- c("orientation", "initial_encoding", "option_inspection", "comparison", "reconsideration", "commitment")
  if (is.function(model)) {
    labels <- as.character(model(x$summary, x$data))
  } else if (is.list(rules) && length(rules)) {
    labels <- rep("unclassified", n)
    for (name in names(rules)) {
      result <- rules[[name]](x$summary, x$data)
      labels[as.logical(result)] <- name
    }
  } else {
    positions <- round(seq(1, length(default_labels), length.out = n))
    labels <- default_labels[pmin(length(default_labels), pmax(1, positions))]
    if (n > 2L) labels[[n]] <- "commitment"
  }
  if (length(labels) != n) .mi_stop("Episode labels must match the number of episodes.")
  x$summary$episode_label <- labels
  x$data$episode_label <- labels[match(x$data$episode_id, x$summary$episode_id)]
  x$status <- "Process episodes labelled using transparent rules or an external classifier."
  x
}

#' Compare episode structures between groups
#'
#' @param x Episode data or list of episode objects.
#' @param group Group vector.
#' @return An `eye_episode_comparison` object.
#' @export
compare_episode_structure <- function(x, group) {
  if (inherits(x, "eye_process_episodes")) {
    data <- x$data
    if (length(group) != nrow(data)) .mi_stop("`group` must align with episode-level observations.")
    data$.group <- group
  } else if (is.list(x)) {
    data <- do.call(rbind, lapply(seq_along(x), function(i) transform(x[[i]]$summary, .case = i, .group = group[[i]])))
  } else .mi_stop("`x` must be an episode object or list.")
  label_col <- if ("episode_label" %in% names(data)) "episode_label" else "episode_id"
  table <- as.data.frame(table(group = data$.group, episode = data[[label_col]]))
  totals <- aggregate(Freq ~ group, data = table, FUN = sum); table <- merge(table, totals, by = "group", suffixes = c("", "_total")); table$proportion <- table$Freq / pmax(table$Freq_total, 1)
  .mi_new("eye_episode_comparison", table = table, summary = table, status = "Episode structures compared across groups.")
}

#' @export
plot.eye_process_changepoints <- function(x, type = c("ribbons", "score", "diagnostics"), ...) {
  type <- match.arg(type)
  graphics::plot(x$score, type = "l", xlab = "Observation", ylab = "Change score", main = "Process change-point evidence")
  graphics::abline(h = x$threshold, lty = 2)
  if (length(x$changepoints)) graphics::abline(v = x$changepoints, lty = 3)
  invisible(x)
}
#' @export
plot.eye_process_episodes <- function(x, type = c("episodes", "waterfall", "transition_graph", "duration", "diagnostics"), ...) {
  type <- match.arg(type)
  if (type == "duration" || type == "waterfall") {
    labels <- x$summary$episode_label %||% paste0("episode_", x$summary$episode_id)
    graphics::barplot(x$summary$n_observations, names.arg = labels, las = 2, ylab = "Observations", main = "Episode duration distribution")
  } else if (type == "transition_graph") {
    labels <- x$summary$episode_label %||% paste0("episode_", x$summary$episode_id)
    positions <- seq_along(labels)
    graphics::plot(positions, rep(1, length(labels)), type = "n", yaxt = "n", xlab = "Episode order", ylab = "", main = "Episode transition graph")
    graphics::text(positions, rep(1, length(labels)), labels = labels)
    if (length(labels) > 1L) graphics::arrows(positions[-length(labels)], rep(1, length(labels) - 1L), positions[-1L], rep(1, length(labels) - 1L), length = 0.05)
  } else {
    channel <- x$changepoints$channels[[1L]]
    graphics::plot(x$data[[channel]], type = "l", xlab = "Observation", ylab = channel, main = "Cognitive-process episodes")
    graphics::abline(v = x$changepoints$changepoints, lty = 2)
  }
  invisible(x)
}
#' @export
plot.eye_episode_comparison <- function(x, type = c("duration", "structure"), ...) {
  matrix <- xtabs(proportion ~ episode + group, data = x$table)
  graphics::barplot(matrix, beside = TRUE, las = 2, ylab = "Proportion", main = "Episode structure by group")
  graphics::legend("topright", legend = rownames(matrix), fill = seq_len(nrow(matrix)), bty = "n")
  invisible(x)
}
#' @export
plot_process_episodes <- function(x, ...) plot(x, type = "episodes", ...)
#' @export
plot_changepoint_ribbons <- function(x, ...) plot(x, type = "ribbons", ...)
#' @export
plot_episode_waterfall <- function(x, ...) plot(x, type = "waterfall", ...)
#' @export
plot_episode_transition_graph <- function(x, ...) plot(x, type = "transition_graph", ...)
#' @export
plot_episode_duration_distribution <- function(x, ...) plot(x, type = "duration", ...)
