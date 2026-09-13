# eyeprocess 0.7.0.9000 ------------------------------------------------------
# Lightweight base-R diagnostic plots for process-IRT and validation objects.
# Deliberately dependency-free; publication graphics can build on returned data.

.ep07_plot_new <- function(main = NULL, xlab = "", ylab = "", ...) {
  graphics::plot.new()
  graphics::title(main = main, xlab = xlab, ylab = ylab, ...)
}

.ep07_plot_coef <- function(tab, label, estimate, main, xlab = "Estimate") {
  if (!nrow(tab)) { .ep07_plot_new(main); return(invisible(NULL)) }
  y <- rev(seq_len(nrow(tab)))
  est <- tab[[estimate]]
  graphics::plot(est, y, yaxt = "n", ylab = "", xlab = xlab, main = main,
                 pch = 19)
  graphics::axis(2, at = y, labels = tab[[label]], las = 2, cex.axis = .8)
  graphics::abline(v = 0, lty = 3)
  if (all(c("lower", "upper") %in% names(tab)))
    graphics::segments(tab$lower, y, tab$upper, y)
  invisible(tab)
}

#' Plot a joint gaze-response-time IRT fit
#' @param x Object to print, plot, summarize, or audit.
#' @param type Plot or result type.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot a joint gaze-response-time IRT fit.
#' @export
plot.eye_joint_gaze_rt_irt <- function(x, type = c("latent", "components"), ...) {
  type <- match.arg(type)
  if (type == "latent" && !is.null(x$person_scores)) {
    z <- as.data.frame(x$person_scores)
    nums <- names(z)[vapply(z, is.numeric, logical(1))]
    if (length(nums) >= 2L) {
      graphics::plot(z[[nums[1]]], z[[nums[2]]], xlab = nums[1], ylab = nums[2],
                     main = "Joint process-IRT person dimensions", pch = 19, ...)
      return(invisible(z))
    }
  }
  if (!is.null(x$component_summary)) {
    z <- as.data.frame(x$component_summary)
    if (all(c("component", "estimate") %in% names(z)))
      return(.ep07_plot_coef(z, "component", "estimate", "Joint model components", ...))
  }
  .ep07_plot_new("Joint gaze-RT-response IRT fit")
  graphics::text(.5, .5, "No plottable latent/component summary stored")
  invisible(x)
}

#' Plot a graded response + RT/process fit
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot a graded response + RT/process fit.
#' @export
plot.eye_joint_graded_rt_process_irt <- function(x, ...) {
  if (!is.null(x$person_scores)) {
    z <- as.data.frame(x$person_scores)
    nums <- names(z)[vapply(z, is.numeric, logical(1))]
    if (length(nums) >= 2L) {
      graphics::plot(z[[nums[1]]], z[[nums[2]]], xlab = nums[1], ylab = nums[2],
                     main = "Graded-response process dimensions", pch = 19, ...)
      return(invisible(z))
    }
  }
  .ep07_plot_new("Graded-response + RT/process IRT")
  graphics::text(.5, .5, "Inspect component models for model-specific plots")
  invisible(x)
}

#' Plot nominal-response gaze results
#' @param x Object to print, plot, summarize, or audit.
#' @param type Plot or result type.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot nominal-response gaze results.
#' @export
plot.eye_nominal_gaze_irt <- function(x, type = c("distractor_map", "coefficients"), ...) {
  type <- match.arg(type)
  if (type == "distractor_map") {
    z <- tryCatch(distractor_process_map(x), error = function(e) NULL)
    if (!is.null(z) && nrow(z) && all(c("gaze_contrast", "choice_contrast") %in% names(z))) {
      graphics::plot(z$gaze_contrast, z$choice_contrast, pch = 19,
                     xlab = "Gaze contrast", ylab = "Choice contrast",
                     main = "Distractor process map", ...)
      if ("option" %in% names(z)) graphics::text(z$gaze_contrast, z$choice_contrast,
                                                  labels = z$option, pos = 3, cex = .75)
      graphics::abline(h = 0, v = 0, lty = 3)
      return(invisible(z))
    }
  }
  co <- x$choice_coefficients %||% x$coefficients
  if (is.matrix(co)) {
    graphics::matplot(t(co), type = "h", lty = 1, main = "Nominal gaze-IRT coefficients",
                      xlab = "Coefficient", ylab = "Estimate", ...)
    graphics::abline(h = 0, lty = 3)
    return(invisible(co))
  }
  .ep07_plot_new("Nominal gaze IRT")
  graphics::text(.5, .5, "No plottable distractor summary stored")
  invisible(x)
}

#' Plot omission/not-reached survival IRT diagnostics
#' @param x Object to print, plot, summarize, or audit.
#' @param type Plot or result type.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot omission/not-reached survival IRT diagnostics.
#' @export
plot.eye_omission_survival_irt <- function(x, type = c("survival", "missingness"), ...) {
  type <- match.arg(type)
  if (type == "survival") {
    fit <- x$omission_model %||% x$survival_model
    if (!is.null(fit) && inherits(fit, "coxph")) {
      sf <- survival::survfit(fit)
      graphics::plot(sf, xlab = "Process time", ylab = "Survival probability",
                     main = "Omission survival process", ...)
      return(invisible(sf))
    }
  }
  z <- x$missingness %||% x$classified_missingness
  if (!is.null(z)) {
    tab <- sort(table(z$missingness_class %||% z), decreasing = TRUE)
    graphics::barplot(tab, las = 2, ylab = "Count", main = "Process-informed missingness", ...)
    return(invisible(tab))
  }
  .ep07_plot_new("Omission survival IRT")
  graphics::text(.5, .5, "No survival/missingness summary stored")
  invisible(x)
}

#' Plot many-facet process IRT effects
#' @param x Object to print, plot, summarize, or audit.
#' @param facet Facet to display or extract.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot many-facet process IRT effects.
#' @export
plot.eye_manyfacet_process_irt <- function(x, facet = NULL, ...) {
  z <- tryCatch({
    available_facets <- names(x$facets)
    selected_facet <- if (is.null(facet)) {
      available_facets[1L]
    } else {
      as.character(facet[1L])
    }
    if (!length(selected_facet) ||
        is.na(selected_facet) ||
        !selected_facet %in% available_facets) {
      stop("Requested facet was not fitted.", call. = FALSE)
    }
    channel <- if (!is.null(x$process_model)) "process" else "response"
    fx <- .ep07_named_facet_effects(
      x,
      selected_facet,
      channel = channel
    )
    re <- fx$random_effects
    if (is.null(re) || !nrow(re)) {
      NULL
    } else {
      data.frame(
        facet = selected_facet,
        level = rownames(re),
        effect = as.numeric(re[[1L]]),
        stringsAsFactors = FALSE
      )
    }
  }, error = function(e) NULL)
  if (!is.null(z) && nrow(z)) {
    label <- if (all(c("facet", "level") %in% names(z))) paste(z$facet, z$level, sep = ":") else
      rownames(z)
    estnm <- intersect(c("effect", "estimate", "value"), names(z))[1]
    if (!is.na(estnm)) return(.ep07_plot_coef(transform(z, .label = label), ".label", estnm,
                                               "Many-facet process effects", ...))
  }
  .ep07_plot_new("Many-facet process IRT")
  graphics::text(.5, .5, "No plottable facet effects stored")
  invisible(x)
}

#' Plot detected process changepoints
#' @param x Object to print, plot, summarize, or audit.
#' @param person Person or participant identifier column.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot detected process changepoints.
#' @export
plot.eye_irt_changepoints <- function(x, person = NULL, ...) {
  d <- if (inherits(x, "eye_irt_changepoints")) x else as.data.frame(x)
  if (!is.null(x$data) && is.list(x)) d <- x$data
  d <- as.data.frame(d)
  if (!is.null(person) && "person" %in% names(d)) d <- d[d$person == person, , drop = FALSE]
  posnm <- intersect(c("changepoint", "position", "index"), names(d))[1]
  scnm <- intersect(c("score", "sic", "evidence"), names(d))[1]
  if (!is.na(posnm) && !is.na(scnm) && nrow(d)) {
    graphics::plot(d[[posnm]], d[[scnm]], type = "b", pch = 19,
                   xlab = "Candidate position", ylab = scnm,
                   main = "Process changepoint evidence", ...)
    return(invisible(d))
  }
  if (!is.na(posnm) && nrow(d)) {
    graphics::dotchart(d[[posnm]], main = "Estimated process changepoints",
                       xlab = "Position", ...)
    return(invisible(d))
  }
  .ep07_plot_new("Process changepoints")
  graphics::text(.5, .5, "No plottable changepoint table")
  invisible(x)
}

#' Plot a process-HMM IRT fit
#' @param x Object to print, plot, summarize, or audit.
#' @param type Plot or result type.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot a process-HMM IRT fit.
#' @export
plot.eye_process_hmm_irt <- function(x, type = c("occupancy", "transition"), ...) {
  type <- match.arg(type)
  if (type == "occupancy") {
    z <- tryCatch(process_state_occupancy(x), error = function(e) NULL)
    if (!is.null(z) && nrow(z)) {
      state <- z$state %||% z$process_state
      valnm <- intersect(c("proportion", "occupancy", "value"), names(z))[1]
      if (!is.na(valnm)) {
        tab <- stats::aggregate(z[[valnm]], list(state = state), mean, na.rm = TRUE)
        graphics::barplot(tab$x, names.arg = tab$state, xlab = "State", ylab = "Mean occupancy",
                          main = "Process-state occupancy", ...)
        return(invisible(tab))
      }
    }
  } else {
    z <- tryCatch(process_state_transition_summary(x), error = function(e) NULL)
    if (!is.null(z) && nrow(z) && all(c("from", "to") %in% names(z))) {
      valnm <- intersect(c("probability", "count", "value"), names(z))[1]
      if (!is.na(valnm)) {
        mat <- xtabs(z[[valnm]] ~ z$from + z$to)
        graphics::image(seq_len(nrow(mat)), seq_len(ncol(mat)), mat,
                        xlab = "From state", ylab = "To state",
                        main = "Process-state transition matrix", axes = FALSE, ...)
        graphics::axis(1, at = seq_len(nrow(mat)), labels = rownames(mat))
        graphics::axis(2, at = seq_len(ncol(mat)), labels = colnames(mat))
        return(invisible(mat))
      }
    }
  }
  .ep07_plot_new("Process HMM + IRT")
  graphics::text(.5, .5, "No plottable HMM summary stored")
  invisible(x)
}

#' Plot a latent-space IRT adapter fit
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot a latent-space IRT adapter fit.
#' @export
plot.eye_latent_space_irt <- function(x, ...) {
  z <- tryCatch(process_residual_map(x), error = function(e) NULL)
  if (!is.null(z) && nrow(z)) {
    dims <- grep("^(dim|z|coord|latent)", names(z), value = TRUE, ignore.case = TRUE)
    if (length(dims) < 2L) dims <- names(z)[vapply(z, is.numeric, logical(1))]
    if (length(dims) >= 2L) {
      pch <- if ("entity_type" %in% names(z)) as.integer(factor(z$entity_type)) else 19
      graphics::plot(z[[dims[1]]], z[[dims[2]]], pch = pch,
                     xlab = dims[1], ylab = dims[2], main = "Person-item latent space", ...)
      if ("entity_id" %in% names(z)) graphics::text(z[[dims[1]]], z[[dims[2]]],
                                                      labels = z$entity_id, pos = 3, cex = .6)
      return(invisible(z))
    }
  }
  .ep07_plot_new("Latent-space IRT")
  graphics::text(.5, .5, "No extractable 2-D latent coordinates")
  invisible(x)
}

#' Plot process person-fit discrepancies
#' @param x Object to print, plot, summarize, or audit.
#' @param top Number of highest-ranked cases to display.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process person-fit discrepancies.
#' @export
plot.eye_process_person_fit <- function(x, top = 25L, ...) {
  d <- as.data.frame(x$person_fit %||% x)
  score <- intersect(c("joint_discrepancy", "process_discrepancy", "score", "rms"), names(d))[1]
  id <- intersect(c("person", "person_id", "id"), names(d))[1]
  if (!is.na(score) && nrow(d)) {
    ord <- order(d[[score]], decreasing = TRUE, na.last = NA)
    ord <- head(ord, top)
    lab <- if (!is.na(id)) d[[id]][ord] else ord
    graphics::dotchart(d[[score]][ord], labels = lab, xlab = score,
                       main = "Largest model-process discrepancies", ...)
    return(invisible(d[ord, , drop = FALSE]))
  }
  .ep07_plot_new("Process person fit")
  graphics::text(.5, .5, "No person-fit score found")
  invisible(x)
}

#' Plot an IRT linking/equating transformation
#' @param x Object to print, plot, summarize, or audit.
#' @param theta Latent-trait values.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot an IRT linking/equating transformation.
#' @export
plot.eye_irt_equating <- function(x, theta = seq(-4, 4, length.out = 201), ...) {
  A <- x$A %||% x$slope %||% 1
  B <- x$B %||% x$intercept %||% 0
  linked <- A * theta + B
  graphics::plot(theta, linked, type = "l", xlab = "New-form scale",
                 ylab = "Reference scale", main = "IRT scale linking", ...)
  graphics::abline(0, 1, lty = 3)
  invisible(data.frame(theta = theta, linked = linked))
}

#' Plot flexible IRF shape diagnostics
#' @param x Object to print, plot, summarize, or audit.
#' @param item Item identifier, name, or item column.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot flexible IRF shape diagnostics.
#' @export
plot.eye_gpirt <- function(x, item = NULL, ...) {
  z <- NULL
  if (inherits(x, "eye_gpirt") &&
      identical(x$engine, "spline_reference") &&
      !is.null(x$response_matrix)) {
    z <- tryCatch(
      compare_parametric_nonparametric_irf(
        x$response_matrix,
        gpirt_object = x
      ),
      error = function(e) NULL
    )
    if (!is.null(z) && nrow(z)) {
      available_items <- unique(as.character(z$item))
      if (is.null(item)) {
        selected_item <- available_items[1L]
      } else if (is.numeric(item)) {
        idx <- as.integer(item[1L])
        selected_item <- if (
          is.finite(idx) &&
          idx >= 1L &&
          idx <= length(available_items)
        ) available_items[idx] else NA_character_
      } else {
        selected_item <- as.character(item[1L])
      }
      if (!is.na(selected_item)) {
        z <- z[z$item == selected_item, , drop = FALSE]
      } else {
        z <- z[0, , drop = FALSE]
      }
    }
  }
  if (!is.null(z) && nrow(z)) {
    xnm <- intersect(c("theta", "ability"), names(z))[1]
    ynames <- intersect(c("parametric", "flexible", "nonparametric", "probability"), names(z))
    if (!is.na(xnm) && length(ynames)) {
      mat <- as.matrix(z[ynames])
      graphics::matplot(z[[xnm]], mat, type = "l", lty = seq_len(ncol(mat)),
                        xlab = xnm, ylab = "Response probability",
                        main = "Item-response shape audit", ...)
      graphics::legend("topleft", legend = ynames, lty = seq_len(ncol(mat)), bty = "n")
      return(invisible(z))
    }
  }
  .ep07_plot_new("Flexible item-response function")
  graphics::text(.5, .5, "No plottable IRF grid stored")
  invisible(x)
}

#' Plot CAT simulation information accumulation
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot CAT simulation information accumulation.
#' @export
plot.eye_process_cat_simulation <- function(x, ...) {
  d <- as.data.frame(x$history %||% x)
  step <- intersect(c("step", "item_number"), names(d))[1]
  info <- intersect(c("information", "utility", "cumulative_information"), names(d))[1]
  if (!is.na(step) && !is.na(info) && nrow(d)) {
    graphics::plot(d[[step]], d[[info]], type = "b", pch = 19,
                   xlab = "Administered item", ylab = info,
                   main = "Process-aware adaptive information", ...)
    return(invisible(d))
  }
  .ep07_plot_new("Process-aware CAT")
  graphics::text(.5, .5, "No CAT history found")
  invisible(x)
}

#' Plot parameter-recovery bias or RMSE
#' @param x Object to print, plot, summarize, or audit.
#' @param metric Metric to calculate or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot parameter-recovery bias or RMSE.
#' @export
plot.eye_irt_recovery_summary <- function(x, metric = c("rmse", "absolute_bias", "coverage"), ...) {
  metric <- match.arg(metric)
  d <- as.data.frame(x)
  .ep07_v_require(d, c("parameter", metric), "x")
  lab <- if ("scenario" %in% names(d)) paste(d$parameter, d$scenario, sep = " / ") else d$parameter
  graphics::dotchart(d[[metric]], labels = lab, xlab = metric,
                     main = "IRT validation recovery", ...)
  invisible(d)
}

#' Plot SBC rank histograms by parameter
#' @param x Object to print, plot, summarize, or audit.
#' @param parameter Value supplied to `parameter`; see Details for its model-specific role.
#' @param breaks Histogram or discretization breaks.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot SBC rank histograms by parameter.
#' @export
plot.eye_irt_sbc <- function(x, parameter = NULL, breaks = 10L, ...) {
  d <- x$ranks
  if (is.null(parameter)) parameter <- unique(d$parameter)[1L]
  z <- d[d$parameter == parameter, , drop = FALSE]
  graphics::hist(z$normalized_rank, breaks = breaks, xlim = c(0, 1),
                 xlab = "Normalized rank", main = paste("SBC:", parameter), ...)
  invisible(z)
}

#' Plot SBC audit summaries
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot SBC audit summaries.
#' @export
plot.eye_sbc_audit <- function(x, ...) {
  d <- as.data.frame(x)
  graphics::plot(d$mean_rank, d$rank_variance, pch = 19,
                 xlab = "Mean normalized rank", ylab = "Rank variance",
                 main = "SBC uniformity screen", ...)
  graphics::abline(v = .5, h = 1 / 12, lty = 3)
  graphics::text(d$mean_rank, d$rank_variance, labels = d$parameter, pos = 3, cex = .75)
  invisible(d)
}

#' Plot posterior predictive discrepancy tail probabilities
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot posterior predictive discrepancy tail probabilities.
#' @export
plot.eye_irt_ppc <- function(x, ...) {
  d <- as.data.frame(x)
  graphics::dotchart(d$p_two_sided, labels = d$discrepancy,
                     xlab = "Two-sided posterior predictive tail probability",
                     main = "Posterior predictive discrepancies", ...)
  graphics::abline(v = c(.01, .05), lty = c(2, 3))
  invisible(d)
}

#' Plot incremental process-channel information by fold
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot incremental process-channel information by fold.
#' @export
plot.eye_incremental_information_audit <- function(x, ...) {
  d <- as.data.frame(x)
  graphics::barplot(d$improvement, names.arg = d$fold, las = 2,
                    ylab = "Out-of-sample improvement",
                    main = "Incremental information from process channel", ...)
  graphics::abline(h = 0, lty = 3)
  invisible(d)
}

#' Plot a process-channel negative-control distribution
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot a process-channel negative-control distribution.
#' @export
plot.eye_process_negative_control <- function(x, ...) {
  graphics::hist(x$null, xlab = "Permuted-channel score",
                 main = "Process-channel negative control", ...)
  graphics::abline(v = x$observed, lwd = 2, lty = 2)
  invisible(x)
}
