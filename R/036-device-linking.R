# Cross-device and cross-vendor linking ---------------------------------------

.mi_paired_device_data <- function(x, metric, device_col, reference_device, id_cols) {
  .mi_assert_columns(x, c(metric, device_col, id_cols))
  devices <- unique(as.character(x[[device_col]]))
  if (!reference_device %in% devices) .mi_stop("`reference_device` is absent from the data.")
  reference <- x[as.character(x[[device_col]]) == reference_device, c(id_cols, metric), drop = FALSE]
  names(reference)[names(reference) == metric] <- "reference_value"
  others <- x[as.character(x[[device_col]]) != reference_device, c(id_cols, device_col, metric), drop = FALSE]
  names(others)[names(others) == metric] <- "device_value"
  merge(others, reference, by = id_cols, all = FALSE)
}

#' Fit cross-device metric linking
#'
#' @param x Long-format measurements from two or more devices.
#' @param metric Metric column.
#' @param reference_device Reference device label.
#' @param method Mixed Bland-Altman, hierarchical linear, or equipercentile linking.
#' @param device_col Device column.
#' @param id_cols Paired-observation identifiers.
#' @return An `eye_device_linking` object.
#' @export
fit_device_linking <- function(x, metric, reference_device, method = c("mixed_bland_altman", "hierarchical", "equipercentile"), device_col = "device", id_cols = c("person_id", "item_id")) {
  .mi_assert_data(x)
  method <- match.arg(method)
  id_cols <- id_cols[id_cols %in% names(x)]
  if (!length(id_cols)) .mi_stop("At least one pairing identifier is required.")
  paired <- .mi_paired_device_data(x, metric, device_col, reference_device, id_cols)
  paired$device_value <- .mi_numeric(paired$device_value)
  paired$reference_value <- .mi_numeric(paired$reference_value)
  paired <- paired[stats::complete.cases(paired[, c("device_value", "reference_value")]), ]
  paired$mean_value <- (paired$device_value + paired$reference_value) / 2
  paired$difference <- paired$device_value - paired$reference_value
  models <- lapply(split(paired, paired[[device_col]]), function(data) {
    if (method == "equipercentile") {
      probabilities <- seq(0, 1, length.out = min(101L, max(5L, nrow(data))))
      list(device_quantiles = stats::quantile(data$device_value, probabilities, names = FALSE),
           reference_quantiles = stats::quantile(data$reference_value, probabilities, names = FALSE),
           probabilities = probabilities)
    } else {
      list(
        transfer = stats::lm(reference_value ~ device_value, data = data),
        bias_model = stats::lm(difference ~ mean_value, data = data),
        bias = mean(data$difference),
        limits = mean(data$difference) + c(-1.96, 1.96) * stats::sd(data$difference)
      )
    }
  })
  summary <- do.call(rbind, lapply(names(models), function(device) {
    data <- paired[paired[[device_col]] == device, ]
    data.frame(device = device, n_pairs = nrow(data), bias = mean(data$difference), sd_difference = stats::sd(data$difference), correlation = stats::cor(data$device_value, data$reference_value), stringsAsFactors = FALSE)
  }))
  .mi_new(
    "eye_device_linking",
    data = x,
    paired = paired,
    metric = metric,
    device_col = device_col,
    id_cols = id_cols,
    reference_device = reference_device,
    method = method,
    models = models,
    summary = summary,
    status = "Cross-device linking model fitted."
  )
}

#' Apply a device-linking transform
#'
#' @param x Data frame to transform.
#' @param linking_model Device-linking object.
#' @param metric,device_col Optional column overrides.
#' @param output_col Linked metric column.
#' @return Data frame with linked measurements.
#' @export
apply_device_linking <- function(x, linking_model, metric = NULL, device_col = NULL, output_col = NULL) {
  .mi_assert_data(x)
  if (!inherits(linking_model, "eye_device_linking")) .mi_stop("`linking_model` must be an `eye_device_linking` object.")
  metric <- metric %||% linking_model$metric
  device_col <- device_col %||% linking_model$device_col
  output_col <- output_col %||% paste0(metric, "_linked")
  .mi_assert_columns(x, c(metric, device_col))
  out <- x
  out[[output_col]] <- .mi_numeric(out[[metric]])
  for (device in names(linking_model$models)) {
    rows <- as.character(out[[device_col]]) == device
    model <- linking_model$models[[device]]
    if (linking_model$method == "equipercentile") {
      out[[output_col]][rows] <- stats::approx(model$device_quantiles, model$reference_quantiles, xout = .mi_numeric(out[[metric]][rows]), rule = 2, ties = "ordered")$y
    } else {
      out[[output_col]][rows] <- as.numeric(stats::predict(model$transfer, newdata = data.frame(device_value = .mi_numeric(out[[metric]][rows]))))
    }
  }
  out
}

#' Audit device equivalence
#'
#' @param x Device-linking object or paired long-format data.
#' @param equivalence_margin Symmetric equivalence margin.
#' @param by Grouping dimensions retained when present.
#' @return An `eye_device_equivalence` object.
#' @export
audit_device_equivalence <- function(x, equivalence_margin, by = c("metric", "task", "aoi")) {
  if (!is.numeric(equivalence_margin) || length(equivalence_margin) != 1L || equivalence_margin <= 0) .mi_stop("`equivalence_margin` must be a positive scalar.")
  if (inherits(x, "eye_device_linking")) {
    data <- x$paired
    group_cols <- intersect(by, names(data))
    group_cols <- unique(c(x$device_col, group_cols))
  } else {
    .mi_stop("The dependency-free equivalence audit currently requires an `eye_device_linking` object.")
  }
  key <- interaction(data[, group_cols, drop = FALSE], drop = TRUE)
  rows <- lapply(split(data, key), function(group) {
    difference <- group$difference
    estimate <- mean(difference)
    se <- stats::sd(difference) / sqrt(length(difference))
    ci <- estimate + c(-1, 1) * stats::qt(0.95, df = max(1, length(difference) - 1)) * se
    first <- group[1L, group_cols, drop = FALSE]
    cbind(first, n = length(difference), mean_difference = estimate, lower90 = ci[[1L]], upper90 = ci[[2L]], equivalent = ci[[1L]] > -equivalence_margin & ci[[2L]] < equivalence_margin)
  })
  summary <- do.call(rbind, rows)
  .mi_new("eye_device_equivalence", summary = summary, margin = equivalence_margin, source = x,
          status = "Device-equivalence intervals calculated.")
}

#' Estimate device-specific residual error
#'
#' @param x Device-linking object.
#' @return Device-specific error table.
#' @export
estimate_device_specific_error <- function(x) {
  if (!inherits(x, "eye_device_linking")) .mi_stop("`x` must be an `eye_device_linking` object.")
  do.call(rbind, lapply(split(x$paired, x$paired[[x$device_col]]), function(data) {
    data.frame(device = as.character(data[[x$device_col]][[1L]]), residual_sd = stats::sd(data$difference), residual_variance = stats::var(data$difference), n = nrow(data), stringsAsFactors = FALSE)
  }))
}

#' @export
plot.eye_device_linking <- function(x, type = c("agreement", "bias", "transfer", "matrix", "diagnostics"), device = NULL, ...) {
  type <- match.arg(type)
  device <- device %||% unique(as.character(x$paired[[x$device_col]]))[[1L]]
  data <- x$paired[as.character(x$paired[[x$device_col]]) == device, ]
  if (type == "agreement") {
    graphics::plot(data$mean_value, data$difference, xlab = "Pair mean", ylab = "Device - reference", main = paste("Device agreement:", device))
    graphics::abline(h = mean(data$difference), lty = 2)
  } else if (type == "bias") {
    graphics::plot(data$mean_value, data$difference, xlab = "Pair mean", ylab = "Difference", main = paste("Magnitude-dependent bias:", device))
    graphics::abline(stats::lm(difference ~ mean_value, data = data))
  } else if (type == "transfer") {
    graphics::plot(data$device_value, data$reference_value, xlab = device, ylab = x$reference_device, main = "Device transfer curve")
    if (x$method != "equipercentile") graphics::abline(x$models[[device]]$transfer)
    graphics::abline(0, 1, lty = 2)
  } else {
    devices <- unique(as.character(x$paired[[x$device_col]]))
    matrix <- sapply(devices, function(one) x$paired$difference[as.character(x$paired[[x$device_col]]) == one])
    graphics::boxplot(matrix, las = 2, ylab = "Device - reference", main = "Cross-vendor metric differences")
  }
  invisible(x)
}
#' @export
plot.eye_device_equivalence <- function(x, type = c("equivalence_intervals", "diagnostics"), ...) {
  type <- match.arg(type)
  summary <- x$summary
  labels <- apply(summary[, vapply(summary, function(z) is.character(z) || is.factor(z), logical(1)), drop = FALSE], 1L, paste, collapse = " / ")
  y <- seq_len(nrow(summary))
  graphics::plot(summary$mean_difference, y, xlim = range(c(summary$lower90, summary$upper90, -x$margin, x$margin)), yaxt = "n", xlab = "Mean difference", ylab = "", main = "Device equivalence intervals")
  graphics::segments(summary$lower90, y, summary$upper90, y)
  graphics::abline(v = c(-x$margin, x$margin), lty = 2)
  graphics::axis(2, at = y, labels = labels, las = 2)
  invisible(x)
}
#' @export
plot_device_agreement <- function(x, ...) plot(x, type = "agreement", ...)
#' @export
plot_device_bias_by_magnitude <- function(x, ...) plot(x, type = "bias", ...)
#' @export
plot_device_transfer_curve <- function(x, ...) plot(x, type = "transfer", ...)
#' @export
plot_device_equivalence_intervals <- function(x, ...) plot(x, type = "equivalence_intervals", ...)
#' @export
plot_cross_vendor_metric_matrix <- function(x, ...) plot(x, type = "matrix", ...)
