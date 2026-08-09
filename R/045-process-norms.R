# Normative process centiles ---------------------------------------------------

#' Fit conditional reference distributions for process metrics
#'
#' @param x Reference-sample data.
#' @param metric Metric column.
#' @param covariates Covariate columns.
#' @param family Auto, Gaussian, or lognormal.
#' @return An `eye_process_norms` object.
#' @export
#' @noRd
fit_process_norms <- function(x, metric, covariates, family = c("auto", "gaussian", "lognormal")) {
  .mi_assert_data(x)
  family <- match.arg(family)
  .mi_assert_columns(x, c(metric, covariates))
  data <- x[, c(metric, covariates), drop = FALSE]
  data[[metric]] <- .mi_numeric(data[[metric]])
  data <- data[stats::complete.cases(data), ]
  if (family == "auto") family <- if (all(data[[metric]] > 0) && abs(stats::cor(sort(data[[metric]]), stats::qnorm(stats::ppoints(nrow(data))))) < abs(stats::cor(sort(log(data[[metric]])), stats::qnorm(stats::ppoints(nrow(data)))))) "lognormal" else "gaussian"
  data$.outcome <- if (family == "lognormal") log(data[[metric]]) else data[[metric]]
  mean_model <- stats::lm(.mi_formula(".outcome", covariates), data = data)
  squared <- pmax(stats::residuals(mean_model)^2, .Machine$double.eps)
  variance_data <- data; variance_data$.log_variance <- log(squared)
  variance_model <- stats::lm(.mi_formula(".log_variance", covariates), data = variance_data)
  .mi_new(
    "eye_process_norms",
    mean_model = mean_model,
    variance_model = variance_model,
    data = data,
    metric = metric,
    covariates = covariates,
    family = family,
    summary = data.frame(n = nrow(data), family = family, residual_sd = stats::sd(stats::residuals(mean_model))),
    status = "Conditional process reference distribution fitted. This is not a clinical norm without representative sampling and external validation."
  )
}

#' Predict process centiles
#'
#' @param model Process-norm model.
#' @param newdata Covariate table.
#' @param centiles Requested centiles.
#' @return Data frame of predicted centiles.
#' @export
#' @noRd
predict_process_centiles <- function(model, newdata, centiles = c(2.5, 10, 25, 50, 75, 90, 97.5)) {
  if (!inherits(model, "eye_process_norms")) .mi_stop("`model` must be an `eye_process_norms` object.")
  .mi_assert_data(newdata)
  mean <- as.numeric(stats::predict(model$mean_model, newdata = newdata))
  variance <- exp(as.numeric(stats::predict(model$variance_model, newdata = newdata)))
  sd <- sqrt(pmax(variance, 1e-12))
  out <- newdata
  for (centile in centiles) {
    value <- mean + stats::qnorm(centile / 100) * sd
    if (model$family == "lognormal") value <- exp(value)
    out[[paste0("centile_", gsub("\\.", "_", format(centile, trim = TRUE)))]] <- value
  }
  out
}

#' Score process deviation from a reference distribution
#'
#' @param model Process-norm model.
#' @param newdata Data containing metric and covariates.
#' @param type Z score, centile, or tail probability.
#' @return Data frame with deviation scores.
#' @export
#' @noRd
score_process_deviation <- function(model, newdata, type = c("z", "centile", "tail_probability")) {
  if (!inherits(model, "eye_process_norms")) .mi_stop("`model` must be an `eye_process_norms` object.")
  type <- match.arg(type)
  .mi_assert_columns(newdata, c(model$metric, model$covariates))
  outcome <- .mi_numeric(newdata[[model$metric]])
  if (model$family == "lognormal") outcome <- log(pmax(outcome, .Machine$double.eps))
  mean <- as.numeric(stats::predict(model$mean_model, newdata = newdata))
  sd <- sqrt(exp(as.numeric(stats::predict(model$variance_model, newdata = newdata))))
  z <- (outcome - mean) / pmax(sd, 1e-12)
  score <- if (type == "z") z else if (type == "centile") 100 * stats::pnorm(z) else 2 * stats::pnorm(-abs(z))
  data.frame(newdata, deviation_score = score, score_type = type, stringsAsFactors = FALSE)
}

#' Audit transportability of process norms
#'
#' @param model Process-norm model.
#' @param new_sample External sample.
#' @return An `eye_norm_transportability` object.
#' @export
#' @noRd
audit_norm_transportability <- function(model, new_sample) {
  scored <- score_process_deviation(model, new_sample, type = "z")
  z <- scored$deviation_score
  summary <- data.frame(
    n = sum(is.finite(z)),
    mean_z = .mi_safe_mean(z),
    sd_z = .mi_safe_sd(z),
    within_95_reference = mean(abs(z) <= 1.96, na.rm = TRUE),
    calibration_flag = abs(.mi_safe_mean(z)) > 0.25 | abs(.mi_safe_sd(z) - 1) > 0.25,
    stringsAsFactors = FALSE
  )
  .mi_new("eye_norm_transportability", model = model, scored = scored, summary = summary, status = "Norm transportability audited in an external sample.")
}

#' @export
plot.eye_process_norms <- function(x, type = c("centiles", "fan", "person_profile", "item_deviation", "diagnostics"), newdata = NULL, ...) {
  type <- match.arg(type)
  if (type == "diagnostics") {
    graphics::plot(stats::fitted(x$mean_model), stats::residuals(x$mean_model), xlab = "Fitted", ylab = "Residual", main = "Process norm diagnostics"); graphics::abline(h = 0, lty = 2)
  } else if (length(x$covariates) == 1L && is.numeric(x$data[[x$covariates[[1L]]]])) {
    covariate <- x$covariates[[1L]]
    grid <- data.frame(seq(min(x$data[[covariate]]), max(x$data[[covariate]]), length.out = 100)); names(grid) <- covariate
    centiles <- predict_process_centiles(x, grid, c(2.5, 10, 50, 90, 97.5))
    graphics::plot(x$data[[covariate]], if (x$family == "lognormal") exp(x$data$.outcome) else x$data$.outcome, xlab = covariate, ylab = x$metric, main = "Conditional process centiles")
    columns <- grep("^centile_", names(centiles), value = TRUE)
    for (i in seq_along(columns)) graphics::lines(grid[[covariate]], centiles[[columns[[i]]]], lty = i)
    graphics::legend("topright", legend = columns, lty = seq_along(columns), bty = "n")
  } else {
    reference_data <- x$data
    reference_data[[x$metric]] <- if (x$family == "lognormal") exp(x$data$.outcome) else x$data$.outcome
    scored <- score_process_deviation(x, newdata %||% reference_data, type = "z")
    graphics::barplot(scored$deviation_score, ylab = "Normative z score", main = "Process normative profile")
  }
  invisible(x)
}
#' @export
plot.eye_norm_transportability <- function(x, type = c("item_deviation", "diagnostics"), ...) {
  graphics::hist(x$scored$deviation_score, xlab = "External-sample normative z", main = "Norm transportability")
  graphics::abline(v = c(-1.96, 0, 1.96), lty = c(2, 1, 2))
  invisible(x)
}
#' @export
plot_process_centiles <- function(x, ...) plot(x, type = "centiles", ...)
#' @export
plot_normative_fan <- function(x, ...) plot(x, type = "fan", ...)
#' @export
plot_person_normative_profile <- function(x, ...) plot(x, type = "person_profile", ...)
#' @export
plot_item_normative_deviation <- function(x, ...) plot(x, type = "item_deviation", ...)
