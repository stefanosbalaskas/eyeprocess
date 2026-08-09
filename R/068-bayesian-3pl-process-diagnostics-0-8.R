# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Bayesian process-model diagnostics and a process-aware 3PL audit.
# These functions deliberately separate model-comparison evidence from
# substantive interpretation and treat gaze/process alignment with the 3PL
# lower asymptote as descriptive response-process evidence, not a behavioral
# diagnosis or a confirmatory guessing detector.

#' Summarize Bayesian process-model diagnostics
#'
#' Collects model availability, optional approximate leave-one-out summaries,
#' posterior diagnostics, and (only when explicitly requested) a Bayes-factor
#' comparison. The function does not treat any one diagnostic as proof of the
#' substantive process interpretation.
#'
#' @param ... Fitted `brmsfit` objects.
#' @param model_names Optional model labels.
#' @param compute_loo Whether to compute approximate leave-one-out diagnostics.
#' @param compute_bayes_factor Whether to attempt a Bayes factor. This requires
#'   exactly two suitable brms models and typically models fitted with
#'   `save_pars = save_pars(all = TRUE)`.
#' @param posterior_summary Whether to collect posterior convergence summaries
#'   when package `posterior` is available.
#' @return An `eye_bayesian_process_dashboard` object.
#' @export
bayesian_process_diagnostics_dashboard <- function(
    ..., model_names = NULL, compute_loo = TRUE,
    compute_bayes_factor = FALSE, posterior_summary = TRUE) {
  models <- list(...)
  if (!length(models)) stop("Supply at least one fitted Bayesian model.", call. = FALSE)
  if (!requireNamespace("brms", quietly = TRUE))
    stop("Package `brms` is required for Bayesian process diagnostics.", call. = FALSE)

  if (is.null(model_names)) {
    model_names <- names(models)
    if (is.null(model_names) || any(!nzchar(model_names)))
      model_names <- paste0("model_", seq_along(models))
  }
  if (length(model_names) != length(models))
    stop("model_names must have one label per supplied model.", call. = FALSE)
  if (!all(vapply(models, inherits, logical(1), what = "brmsfit")))
    stop("All supplied models must inherit from `brmsfit`.", call. = FALSE)
  names(models) <- model_names

  availability <- data.frame(
    model = model_names,
    class = vapply(models, function(z) paste(class(z), collapse = "/"), character(1)),
    brmsfit = vapply(models, inherits, logical(1), what = "brmsfit"),
    stringsAsFactors = FALSE
  )

  loo_results <- list()
  loo_table <- data.frame()
  loo_compare <- NULL
  if (isTRUE(compute_loo)) {
    loo_results <- lapply(models, function(z) {
      tryCatch(brms::loo(z), error = function(e) e)
    })
    ok <- vapply(loo_results, function(z) !inherits(z, "error"), logical(1))
    loo_table <- do.call(rbind, lapply(seq_along(loo_results), function(i) {
      z <- loo_results[[i]]
      if (inherits(z, "error")) {
        data.frame(model = model_names[i], elpd_loo = NA_real_, se_elpd_loo = NA_real_,
                   looic = NA_real_, status = paste0("failed: ", conditionMessage(z)),
                   stringsAsFactors = FALSE)
      } else {
        est <- as.data.frame(z$estimates)
        data.frame(
          model = model_names[i],
          elpd_loo = if ("elpd_loo" %in% rownames(est)) est["elpd_loo", "Estimate"] else NA_real_,
          se_elpd_loo = if ("elpd_loo" %in% rownames(est)) est["elpd_loo", "SE"] else NA_real_,
          looic = if ("looic" %in% rownames(est)) est["looic", "Estimate"] else NA_real_,
          status = "completed", stringsAsFactors = FALSE
        )
      }
    }))
    if (sum(ok) >= 2L && requireNamespace("loo", quietly = TRUE)) {
      loo_compare <- tryCatch(do.call(loo::loo_compare, loo_results[ok]), error = function(e) e)
    }
  }

  posterior_table <- data.frame()
  if (isTRUE(posterior_summary) && requireNamespace("posterior", quietly = TRUE)) {
    post_rows <- lapply(seq_along(models), function(i) {
      dr <- tryCatch(posterior::as_draws_df(models[[i]]), error = function(e) e)
      if (inherits(dr, "error")) {
        return(data.frame(model = model_names[i], variable = NA_character_, mean = NA_real_,
                          sd = NA_real_, rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
                          status = paste0("failed: ", conditionMessage(dr)), stringsAsFactors = FALSE))
      }
      sm <- tryCatch(posterior::summarise_draws(dr), error = function(e) e)
      if (inherits(sm, "error")) {
        return(data.frame(model = model_names[i], variable = NA_character_, mean = NA_real_,
                          sd = NA_real_, rhat = NA_real_, ess_bulk = NA_real_, ess_tail = NA_real_,
                          status = paste0("failed: ", conditionMessage(sm)), stringsAsFactors = FALSE))
      }
      sm <- as.data.frame(sm)
      keep <- intersect(c("variable", "mean", "sd", "rhat", "ess_bulk", "ess_tail"), names(sm))
      sm <- sm[, keep, drop = FALSE]
      for (nm in setdiff(c("variable", "mean", "sd", "rhat", "ess_bulk", "ess_tail"), names(sm))) {
        sm[[nm]] <- if (identical(nm, "variable")) NA_character_ else NA_real_
      }
      sm$model <- model_names[i]
      sm$status <- "completed"
      sm[, c("model", "variable", "mean", "sd", "rhat", "ess_bulk", "ess_tail", "status"), drop = FALSE]
    })
    posterior_table <- do.call(rbind, post_rows)
  }

  bf <- NULL
  bf_status <- "not_requested"
  if (isTRUE(compute_bayes_factor)) {
    if (length(models) != 2L) {
      bf_status <- "requires_exactly_two_models"
    } else {
      bf <- tryCatch(brms::bayes_factor(models[[1L]], models[[2L]]), error = function(e) e)
      bf_status <- if (inherits(bf, "error")) paste0("failed: ", conditionMessage(bf)) else "completed"
    }
  }

  structure(list(
    models = models,
    availability = availability,
    loo = loo_results,
    loo_table = loo_table,
    loo_compare = loo_compare,
    posterior = posterior_table,
    bayes_factor = bf,
    bayes_factor_status = bf_status,
    caveat = paste(
      "LOO, posterior diagnostics, and Bayes factors answer different questions.",
      "Model comparison does not establish that gaze, pupil, or other process channels have a causal or uniquely psychological interpretation."
    )
  ), class = "eye_bayesian_process_dashboard")
}

#' Extract compact Bayesian process-model diagnostic flags
#'
#' @param x An `eye_bayesian_process_dashboard`.
#' @param rhat_threshold Review threshold for R-hat.
#' @param ess_threshold Review threshold for bulk/tail effective sample size.
#' @export
bayesian_process_diagnostic_flags <- function(x, rhat_threshold = 1.01, ess_threshold = 400) {
  if (!inherits(x, "eye_bayesian_process_dashboard"))
    stop("x must be an eye_bayesian_process_dashboard.", call. = FALSE)
  if (!is.finite(rhat_threshold) || rhat_threshold <= 1 || !is.finite(ess_threshold) || ess_threshold <= 0)
    stop("Require rhat_threshold > 1 and ess_threshold > 0.", call. = FALSE)
  p <- x$posterior
  if (!is.data.frame(p) || !nrow(p)) return(data.frame())
  p$rhat_review <- is.finite(p$rhat) & p$rhat > rhat_threshold
  p$ess_bulk_review <- is.finite(p$ess_bulk) & p$ess_bulk < ess_threshold
  p$ess_tail_review <- is.finite(p$ess_tail) & p$ess_tail < ess_threshold
  p$review_required <- p$rhat_review | p$ess_bulk_review | p$ess_tail_review
  p
}

#' Fit a standard 3PL and audit lower-asymptote alignment with process evidence
#'
#' Fits a conventional 3PL response model with `mirt`, then aligns the fitted
#' lower-asymptote parameter with item-level gaze/pupil/RT summaries. The
#' alignment is descriptive and diagnostic. It must not be interpreted as a
#' confirmatory detector of guessing, rapid responding, disengagement, or any
#' other latent behavior without independent validation.
#'
#' @param response_matrix Person x item dichotomous response matrix.
#' @param process_data Optional trial/person-item process table.
#' @param item Item identifier in `process_data`.
#' @param process_features Candidate numeric process features.
#' @param model mirt model specification.
#' @param SE Request standard errors from `mirt`.
#' @return An `eye_gaze_anchored_3pl_audit` object.
#' @export
fit_gaze_anchored_3pl_audit <- function(
    response_matrix, process_data = NULL, item = "item_id",
    process_features = c("ttff_ms", "dwell_ms", "pupil_bc", "pupil_peak", "rt_ms", "accuracy"),
    model = 1, SE = FALSE) {
  if (!requireNamespace("mirt", quietly = TRUE))
    stop("Package `mirt` is required for the 3PL process audit.", call. = FALSE)
  X <- as.data.frame(response_matrix)
  if (is.null(colnames(X)) || any(!nzchar(colnames(X)))) colnames(X) <- paste0("Item", seq_len(ncol(X)))
  if (ncol(X) < 4L) stop("At least four items are required for this 3PL audit.", call. = FALSE)
  if (nrow(X) < 100L) warning("3PL parameters can be unstable with small samples; validate recovery before interpretation.", call. = FALSE)

  fit <- mirt::mirt(X, model = model, itemtype = "3PL", SE = SE, verbose = FALSE)
  co <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)
  pars <- as.data.frame(co$items)
  pars$item_id <- rownames(pars)

  # Standardize common mirt naming without assuming a single package version.
  if (!"a" %in% names(pars) && "a1" %in% names(pars)) pars$a <- .ep08_num(pars$a1)
  if (!"b" %in% names(pars) && all(c("a1", "d") %in% names(pars)))
    pars$b <- -.ep08_num(pars$d) / pmax(.ep08_num(pars$a1), 1e-8)
  lower_col <- intersect(c("g", "guess", "Guessing", "lower", "lower_asymptote"), names(pars))[1L]
  if (is.na(lower_col)) {
    # mirt IRTpars usually exposes g for 3PL; retain a transparent NA if an
    # alternate return structure is encountered rather than inventing it.
    pars$lower_asymptote <- NA_real_
  } else {
    pars$lower_asymptote <- .ep08_num(pars[[lower_col]])
  }

  keep_par <- intersect(c("item_id", "a", "b", "lower_asymptote"), names(pars))
  item_table <- pars[, keep_par, drop = FALSE]
  process_summary <- NULL
  alignment <- data.frame()

  if (!is.null(process_data)) {
    process_data <- .ep08_as_df(process_data, "process_data")
    .ep08_req_cols(process_data, item, "process_data")
    available <- intersect(process_features, names(process_data))
    numeric_available <- available[vapply(process_data[available], function(z) {
      y <- .ep08_num(z); sum(is.finite(y)) >= 3L
    }, logical(1))]
    if (length(numeric_available)) {
      d <- process_data[, c(item, numeric_available), drop = FALSE]
      d[[item]] <- as.character(d[[item]])
      for (v in numeric_available) d[[v]] <- .ep08_num(d[[v]])
      process_summary <- stats::aggregate(
        d[numeric_available], by = list(item_id = d[[item]]), FUN = .ep08_mean
      )
      item_table <- merge(item_table, process_summary, by = "item_id", all.x = TRUE, sort = FALSE)
      if ("lower_asymptote" %in% names(item_table)) {
        alignment <- do.call(rbind, lapply(numeric_available, function(v) {
          ok <- is.finite(item_table$lower_asymptote) & is.finite(item_table[[v]])
          data.frame(
            feature = v,
            n_items = sum(ok),
            correlation = if (sum(ok) >= 3L && stats::sd(item_table$lower_asymptote[ok]) > 0 &&
                             stats::sd(item_table[[v]][ok]) > 0)
              stats::cor(item_table$lower_asymptote[ok], item_table[[v]][ok]) else NA_real_,
            stringsAsFactors = FALSE
          )
        }))
      }
    }
  }

  structure(list(
    model = fit,
    item_parameters = item_table,
    process_summary = process_summary,
    alignment = alignment,
    process_features = if (is.null(process_summary)) character() else setdiff(names(process_summary), "item_id"),
    status = "standard_3pl_with_descriptive_process_alignment",
    caveat = paste(
      "The 3PL lower asymptote is a psychometric item parameter, not a participant-level guessing label.",
      "Correlations with TTFF, dwell, pupil, RT, or accuracy are descriptive response-process evidence only and require independent validation."
    )
  ), class = "eye_gaze_anchored_3pl_audit")
}

#' Return the process-alignment table from a gaze-anchored 3PL audit
#' @param x An `eye_gaze_anchored_3pl_audit`.
#' @export
gaze_anchored_3pl_alignment <- function(x) {
  if (!inherits(x, "eye_gaze_anchored_3pl_audit"))
    stop("x must be an eye_gaze_anchored_3pl_audit.", call. = FALSE)
  x$alignment
}

#' Identify items for descriptive 3PL/process review
#'
#' @param x An `eye_gaze_anchored_3pl_audit`.
#' @param lower_asymptote_quantile Quantile used to flag relatively large lower asymptotes.
#' @param fast_rt_quantile Optional lower quantile for RT review.
#' @param fast_ttff_quantile Optional lower quantile for TTFF review.
#' @return Item-level review table. These flags are not behavioral classifications.
#' @export
audit_3pl_process_signatures <- function(
    x, lower_asymptote_quantile = 0.80, fast_rt_quantile = 0.20,
    fast_ttff_quantile = 0.20) {
  if (!inherits(x, "eye_gaze_anchored_3pl_audit"))
    stop("x must be an eye_gaze_anchored_3pl_audit.", call. = FALSE)
  d <- x$item_parameters
  qs <- c(lower_asymptote_quantile, fast_rt_quantile, fast_ttff_quantile)
  if (any(!is.finite(qs)) || any(qs <= 0 | qs >= 1)) stop("All review quantiles must lie in (0,1).", call. = FALSE)
  if (!"lower_asymptote" %in% names(d) || !any(is.finite(d$lower_asymptote))) return(d)
  qg <- stats::quantile(d$lower_asymptote, lower_asymptote_quantile, na.rm = TRUE, names = FALSE)
  d$high_lower_asymptote_review <- is.finite(d$lower_asymptote) & d$lower_asymptote >= qg
  d$fast_rt_review <- FALSE
  if ("rt_ms" %in% names(d) && any(is.finite(d$rt_ms))) {
    qr <- stats::quantile(d$rt_ms, fast_rt_quantile, na.rm = TRUE, names = FALSE)
    d$fast_rt_review <- is.finite(d$rt_ms) & d$rt_ms <= qr
  }
  d$fast_ttff_review <- FALSE
  if ("ttff_ms" %in% names(d) && any(is.finite(d$ttff_ms))) {
    qt <- stats::quantile(d$ttff_ms, fast_ttff_quantile, na.rm = TRUE, names = FALSE)
    d$fast_ttff_review <- is.finite(d$ttff_ms) & d$ttff_ms <= qt
  }
  d$process_review_count <- rowSums(d[, c("high_lower_asymptote_review", "fast_rt_review", "fast_ttff_review"), drop = FALSE], na.rm = TRUE)
  d$review_label <- ifelse(d$process_review_count >= 2L,
                           "review_item_response_process_alignment",
                           "no_combined_review_flag")
  d
}

#' Plot bayesian process dashboard diagnostics
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_bayesian_process_dashboard <- function(x, type = c("loo", "rhat", "ess"), ...) {
  type <- match.arg(type)
  if (type == "loo") {
    d <- x$loo_table
    if (!is.data.frame(d) || !nrow(d) || !any(is.finite(d$elpd_loo)))
      return(.ep08_plot_empty("Bayesian process diagnostics", "LOO summary unavailable"))
    graphics::barplot(d$elpd_loo, names.arg = d$model, las = 2,
                      ylab = "ELPD-LOO", main = "Bayesian process-model LOO comparison", ...)
    return(invisible(d))
  }
  d <- x$posterior
  if (!is.data.frame(d) || !nrow(d))
    return(.ep08_plot_empty("Bayesian process diagnostics", "Posterior summary unavailable"))
  if (type == "rhat") {
    z <- d[is.finite(d$rhat), , drop = FALSE]
    if (!nrow(z)) return(.ep08_plot_empty("R-hat diagnostics", "R-hat unavailable"))
    graphics::boxplot(rhat ~ model, data = z, las = 2, ylab = "R-hat",
                      main = "Posterior R-hat by model", ...)
    graphics::abline(h = 1.01, lty = 2)
    return(invisible(z))
  }
  z <- d[is.finite(d$ess_bulk), , drop = FALSE]
  if (!nrow(z)) return(.ep08_plot_empty("ESS diagnostics", "Bulk ESS unavailable"))
  graphics::boxplot(ess_bulk ~ model, data = z, las = 2, ylab = "Bulk ESS",
                    main = "Posterior bulk ESS by model", ...)
  graphics::abline(h = 400, lty = 2)
  invisible(z)
}

#' Plot gaze anchored 3pl audit diagnostics
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param type Type of summary or visual representation to produce.
#' @param feature Process feature to evaluate or display.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_gaze_anchored_3pl_audit <- function(
    x, type = c("lower_asymptote", "process_alignment", "difficulty_discrimination"),
    feature = NULL, ...) {
  type <- match.arg(type)
  d <- x$item_parameters
  if (type == "lower_asymptote") {
    if (!"lower_asymptote" %in% names(d) || !any(is.finite(d$lower_asymptote)))
      return(.ep08_plot_empty("3PL process audit", "Lower-asymptote parameter unavailable"))
    graphics::barplot(d$lower_asymptote, names.arg = d$item_id, las = 2,
                      ylab = "3PL lower asymptote", main = "Item lower-asymptote parameters", ...)
    return(invisible(d))
  }
  if (type == "difficulty_discrimination") {
    if (!all(c("a", "b") %in% names(d)))
      return(.ep08_plot_empty("3PL item parameters", "Difficulty/discrimination unavailable"))
    graphics::plot(d$b, d$a, xlab = "Difficulty b", ylab = "Discrimination a",
                   main = "3PL item parameter map", ...)
    graphics::text(d$b, d$a, labels = d$item_id, pos = 3, cex = .7)
    return(invisible(d))
  }
  available <- intersect(x$process_features, names(d))
  if (!length(available) || !"lower_asymptote" %in% names(d))
    return(.ep08_plot_empty("3PL/process alignment", "No process features available"))
  if (is.null(feature)) feature <- available[1L]
  if (!feature %in% available) stop("Unknown process feature.", call. = FALSE)
  graphics::plot(d[[feature]], d$lower_asymptote,
                 xlab = feature, ylab = "3PL lower asymptote",
                 main = "Descriptive 3PL/process alignment", ...)
  ok <- is.finite(d[[feature]]) & is.finite(d$lower_asymptote)
  if (sum(ok) >= 3L && stats::sd(d[[feature]][ok]) > 0 && stats::sd(d$lower_asymptote[ok]) > 0)
    graphics::abline(stats::lm(y ~ x, data = data.frame(y = d$lower_asymptote[ok], x = d[[feature]][ok])), lty = 2)
  graphics::text(d[[feature]], d$lower_asymptote, labels = d$item_id, pos = 3, cex = .7)
  invisible(d)
}
