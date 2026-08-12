# eyeprocess 0.9 Milestone #2: base-R plot methods for validation and IRT evidence

.ep09m2_plot_empty <- function(main = "No finite data") {
  graphics::plot.new(); graphics::title(main = main); invisible(NULL)
}

#' @export
plot.eye_irt_information_profile <- function(x, y = NULL, ..., show_sem = FALSE) {
  if (!nrow(x)) return(.ep09m2_plot_empty())
  if (isTRUE(show_sem)) graphics::plot(x$theta, x$conditional_sem, type = "l", xlab = expression(theta), ylab = "Conditional SEM", ...)
  else graphics::plot(x$theta, x$information, type = "l", xlab = expression(theta), ylab = "Test information", ...)
  invisible(x)
}

#' @export
plot.eye_irt_test_characteristic_curve <- function(x, y = NULL, ...) {
  if (!nrow(x)) return(.ep09m2_plot_empty())
  graphics::plot(x$theta, x$expected_score, type = "l", xlab = expression(theta), ylab = "Expected test score", ...)
  invisible(x)
}

#' @export
plot.eye_irt_identification_audit <- function(x, y = NULL, ...) {
  vals <- c(location = x$location_identified, scale = x$scale_identified)
  graphics::barplot(as.numeric(vals), names.arg = names(vals), ylim = c(0, 1), ylab = "Identified (0/1)", ...)
  invisible(x)
}

#' @export
plot.eye_irt_sparse_design_audit <- function(x, y = NULL, ...) {
  pc <- x$person_counts$n_items
  if (!length(pc)) return(.ep09m2_plot_empty())
  graphics::hist(pc, xlab = "Observed items per person", main = "Sparse IRT design", ...)
  invisible(x)
}

#' @export
plot.eye_irt_q3_matrix <- function(x, y = NULL, ...) {
  z <- as.matrix(x); if (!length(z)) return(.ep09m2_plot_empty())
  graphics::image(seq_len(ncol(z)), seq_len(nrow(z)), t(z[nrow(z):1, , drop = FALSE]), xlab = "Item", ylab = "Item", ...)
  invisible(x)
}

#' @export
plot.eye_irt_item_fit <- function(x, y = NULL, statistic = c("infit", "outfit"), ...) {
  statistic <- match.arg(statistic); if (!nrow(x)) return(.ep09m2_plot_empty())
  graphics::plot(seq_len(nrow(x)), x[[statistic]], pch = 19, xaxt = "n", xlab = "Item", ylab = statistic, ...)
  graphics::axis(1, at = seq_len(nrow(x)), labels = x$item_id, las = 2, cex.axis = .7); graphics::abline(h = 1, lty = 2)
  invisible(x)
}

#' @export
plot.eye_irt_person_fit <- function(x, y = NULL, statistic = c("infit", "outfit"), ...) {
  statistic <- match.arg(statistic); v <- x[[statistic]]; v <- v[is.finite(v)]
  if (!length(v)) return(.ep09m2_plot_empty())
  graphics::hist(v, xlab = statistic, main = paste("Person", statistic), ...); graphics::abline(v = 1, lty = 2)
  invisible(x)
}

#' @export
plot.eye_irt_fit_dashboard <- function(x, y = NULL, ...) {
  vals <- c(item_fit = !is.null(x$components$item_fit), person_fit = !is.null(x$components$person_fit), q3 = !is.null(x$components$q3), parameters = !is.null(x$components$parameter_audit), identification = !is.null(x$components$identification))
  graphics::barplot(as.numeric(vals), names.arg = names(vals), ylim = c(0, 1), las = 2, ylab = "Component present", ...)
  invisible(x)
}

#' @export
plot.eye_irt_score_uncertainty <- function(x, y = NULL, ...) {
  vals <- c(mean_SE = x$mean_se, median_SE = x$median_se, p95_SE = x$p95_se)
  if (!any(is.finite(vals))) return(.ep09m2_plot_empty())
  graphics::barplot(vals, ylab = "Conditional SE", ...); invisible(x)
}

#' @export
plot.eye_irt_adaptive_trace <- function(x, y = NULL, ...) {
  if (!nrow(x)) return(.ep09m2_plot_empty())
  graphics::plot(x$step, x$theta_after, type = "b", pch = 19, xlab = "Administered item", ylab = expression(hat(theta)), ...)
  invisible(x)
}

#' @export
plot.eye_irt_link_stability <- function(x, y = NULL, parameter = c("A", "B"), ...) {
  parameter <- match.arg(parameter); tab <- x$table
  if (!nrow(tab)) return(.ep09m2_plot_empty())
  graphics::plot(seq_len(nrow(tab)), tab[[parameter]], pch = 19, xaxt = "n", xlab = "Anchor set", ylab = parameter, ...)
  graphics::axis(1, at = seq_len(nrow(tab)), labels = tab$set, las = 2)
  invisible(x)
}

#' @export
plot.eye_irt_dif_curve <- function(x, y = NULL, ...) {
  if (!nrow(x)) return(.ep09m2_plot_empty())
  graphics::plot(x$theta, x$signed_difference, type = "l", xlab = expression(theta), ylab = "Focal - reference probability", ...); graphics::abline(h = 0, lty = 2)
  invisible(x)
}

#' @export
plot.eye_irt_dtf_curve <- function(x, y = NULL, ...) {
  if (!nrow(x)) return(.ep09m2_plot_empty())
  graphics::plot(x$theta, x$signed_difference, type = "l", xlab = expression(theta), ylab = "Focal - reference expected score", ...); graphics::abline(h = 0, lty = 2)
  invisible(x)
}

#' @export
plot.eye_irt_process_alignment <- function(x, y = NULL, channel = NULL, parameter = c("b", "a"), ...) {
  parameter <- match.arg(parameter); tab <- x$table
  if (is.null(channel)) channel <- x$correlations$channel[1L]
  if (is.na(channel) || !channel %in% names(tab)) return(.ep09m2_plot_empty("No process channel"))
  good <- is.finite(tab[[parameter]]) & is.finite(tab[[channel]])
  if (sum(good) < 2L) return(.ep09m2_plot_empty())
  graphics::plot(tab[[parameter]][good], tab[[channel]][good], xlab = paste("IRT", parameter), ylab = channel, pch = 19, ...)
  invisible(x)
}

#' @export
plot.eye_irt_recovery_result <- function(x, y = NULL, parameter = c("b", "a"), ...) {
  parameter <- match.arg(parameter); d <- x$estimates
  if (!nrow(d)) return(.ep09m2_plot_empty("No successful recovery fits"))
  tx <- d[[paste0(parameter, "_truth")]]; ex <- d[[paste0(parameter, "_estimate")]]; good <- is.finite(tx) & is.finite(ex)
  graphics::plot(tx[good], ex[good], xlab = paste(parameter, "truth"), ylab = paste(parameter, "estimate"), pch = 19, ...); graphics::abline(0, 1, lty = 2)
  invisible(x)
}

#' @export
plot.eye_irt_sbc_evidence <- function(x, y = NULL, ...) {
  plot(x$diagnostics, ...); invisible(x)
}

#' @export
plot.eye_cdm_qmatrix_audit <- function(x, y = NULL, ...) {
  vals <- x$attribute$n_items
  graphics::barplot(vals, names.arg = x$attribute$attribute, ylab = "Items measuring attribute", ...); invisible(x)
}

#' @export
plot.eye_validation_evidence_grade <- function(x, y = NULL, ...) {
  graphics::barplot(x$coverage, ylim = c(0, 1), names.arg = x$grade, ylab = "Evidence component coverage", ...); invisible(x)
}

#' @export
plot.eye_validation_readiness <- function(x, y = NULL, ...) {
  tab <- x$table; graphics::barplot(as.numeric(tab$satisfied), names.arg = tab$requirement, ylim = c(0, 1), las = 2, ylab = "Satisfied", ...); invisible(x)
}

#' @export
plot.eye_validation_evidence_freeze <- function(x, y = NULL, ...) {
  vals <- as.numeric(x$presence); graphics::barplot(vals, names.arg = names(x$presence), ylim = c(0, 1), las = 2, ylab = "Evidence present", ...); invisible(x)
}

#' @export
plot.eye_irt_bank_coverage <- function(x, ...) {
  curve <- x$curve
  graphics::plot(curve$theta, curve$information, type = "l", xlab = "theta", ylab = "test information", ...)
  graphics::abline(h = x$target_information, lty = 2)
  graphics::abline(v = x$target, lty = 3)
  invisible(x)
}

#' @export
plot.eye_irt_targeting_gap <- function(x, ...) {
  tab <- x$table
  ylim <- range(c(tab$person_mass, tab$information_mass), finite = TRUE)
  graphics::plot(tab$theta, tab$person_mass, type = "l", ylim = ylim, xlab = "theta", ylab = "normalized mass", ...)
  graphics::lines(tab$theta, tab$information_mass, lty = 2)
  invisible(x)
}

#' @export
plot.eye_irt_missing_design_audit <- function(x, ...) {
  vals <- c(observed = x$observed_fraction,
            structural_missing = x$structural_missing / max(1, x$n_persons * x$n_items),
            unexpected_missing = x$unexpected_missing / max(1, x$n_persons * x$n_items))
  graphics::barplot(vals, ylab = "fraction", ylim = c(0, max(1, vals, na.rm = TRUE)), ...)
  invisible(x)
}

#' @export
plot.eye_irt_prior_sensitivity <- function(x, ...) {
  tab <- x$table
  if (!"estimate" %in% names(tab)) return(.ep09m2_plot_empty("Prior sensitivity table has no `estimate` column."))
  y <- as.numeric(tab$estimate)
  graphics::plot(seq_along(y), y, type = "b", xlab = "prior specification", ylab = "estimate", ...)
  invisible(x)
}

#' @export
plot.eye_validation_evidence_atlas <- function(x, ...) {
  vals <- as.numeric(x$component_status$present)
  names(vals) <- x$component_status$component
  graphics::barplot(vals, ylim = c(0, 1), ylab = "evidence component present", las = 2, ...)
  invisible(x)
}

#' @export
plot.eye_validation_atlas_freeze <- function(x, ...) {
  plot(x$payload$atlas, ...)
  invisible(x)
}

#' @export
plot.eye_stress_evidence_result <- function(x, y = NULL, metric = NULL, ...) {
  if (!inherits(x, "eye_stress_evidence_result")) stop("x must be an eye_stress_evidence_result.", call. = FALSE)
  d <- x$results
  if (!nrow(d)) stop("stress evidence has no successful result rows to plot.", call. = FALSE)
  if (is.null(metric)) metric <- unique(d$metric)[[1L]]
  metric <- as.character(metric)
  if (length(metric) != 1L || !metric %in% d$metric) stop("metric is not present in the stress result.", call. = FALSE)
  d <- d[d$metric == metric, , drop = FALSE]
  finite <- is.finite(d$severity) & is.finite(d$value)
  if (!any(finite)) stop("selected stress metric has no finite plotted values.", call. = FALSE)
  d <- d[finite, , drop = FALSE]
  families <- unique(d$corruption)
  ltys <- ((seq_along(families) - 1L) %% 6L) + 1L
  pchs <- (seq_along(families) - 1L) %% 25L
  graphics::plot(range(d$severity), range(d$value), type = "n",
                 xlab = "Declared corruption severity", ylab = metric, ...)
  for (i in seq_along(families)) {
    z <- d[d$corruption == families[[i]], , drop = FALSE]
    ord <- order(z$severity)
    graphics::lines(z$severity[ord], z$value[ord], type = "b", lty = ltys[[i]], pch = pchs[[i]])
  }
  graphics::legend("topright", legend = families, lty = ltys, pch = pchs, bty = "n")
  invisible(x)
}
