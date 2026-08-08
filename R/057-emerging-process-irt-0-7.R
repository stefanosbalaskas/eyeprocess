# eyeprocess 0.7.0.9000 ------------------------------------------------------
# Emerging process-IRT directions identified in the 2025-2026 literature.
# These are explicitly reference/diagnostic APIs, not reproductions of the
# published estimators unless an external validated engine is supplied.

#' Encode multiple-response item response combinations
#'
#' @param data Long person-item-option table.
#' @param person,item,option,selected Column names.
#' @param sort_options Sort selected option labels before combining.
#' @param empty_code Code for no selected options.
#' @export
encode_response_combinations <- function(data,
                                         person = "participant_id",
                                         item = "item_id",
                                         option = "option_id",
                                         selected = "selected",
                                         sort_options = TRUE,
                                         empty_code = "<none>") {
  d <- .ep07_model_frame(data, c(person, item, option, selected))
  key <- interaction(d[[person]], d[[item]], drop = TRUE, lex.order = TRUE)
  sp <- split(seq_len(nrow(d)), key)
  rows <- lapply(sp, function(ix) {
    z <- d[ix, , drop = FALSE]
    keep <- as.logical(z[[selected]])
    opts <- as.character(z[[option]][!is.na(keep) & keep])
    if (sort_options) opts <- sort(opts, method = "radix")
    code <- if (length(opts)) paste(opts, collapse = "|") else empty_code
    data.frame(participant_id = as.character(z[[person]][1L]),
               item_id = as.character(z[[item]][1L]),
               response_combination = code,
               n_selected = length(opts), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  names(out)[1:2] <- c(person, item)
  out
}

#' Audit inter-option/process local dependence
#'
#' Computes pairwise residual correlations (a Q3-style diagnostic) across item
#' or option columns and, when supplied, analogous process-residual
#' correlations. This is a diagnostic for local dependence, not a formal test
#' with universal cutoffs.
#'
#' @param response_residuals Person-by-item/option residual matrix.
#' @param process_residuals Optional aligned process-residual matrix.
#' @param threshold Absolute correlation threshold used only for flagging.
#' @export
audit_process_local_dependence <- function(response_residuals,
                                           process_residuals = NULL,
                                           threshold = 0.20) {
  R <- as.matrix(response_residuals)
  storage.mode(R) <- "double"
  if (ncol(R) < 2L) stop("At least two residual columns are required.", call. = FALSE)
  rc <- stats::cor(R, use = "pairwise.complete.obs")
  nm <- colnames(R) %||% paste0("V", seq_len(ncol(R)))
  ix <- which(upper.tri(rc), arr.ind = TRUE)
  out <- data.frame(
    first = nm[ix[, 1L]], second = nm[ix[, 2L]],
    response_residual_correlation = rc[ix], stringsAsFactors = FALSE)
  out$response_flag <- is.finite(out$response_residual_correlation) &
    abs(out$response_residual_correlation) >= threshold

  if (!is.null(process_residuals)) {
    P <- as.matrix(process_residuals); storage.mode(P) <- "double"
    if (!identical(dim(P), dim(R)))
      stop("`process_residuals` must have the same dimensions as `response_residuals`.", call. = FALSE)
    pc <- stats::cor(P, use = "pairwise.complete.obs")
    out$process_residual_correlation <- pc[ix]
    out$process_flag <- is.finite(out$process_residual_correlation) &
      abs(out$process_residual_correlation) >= threshold
    out$concordant_direction <- sign(out$response_residual_correlation) == sign(out$process_residual_correlation)
  }
  structure(list(pairs = out, threshold = threshold,
                 max_absolute_response = max(abs(out$response_residual_correlation), na.rm = TRUE),
                 note = "Q3-style residual correlations diagnose local dependence; threshold flags are descriptive and require model/design context."),
            class = "eye_process_local_dependence_audit")
}

#' Fit a multiple-response process-IRT reference model
#'
#' The bundled reference treats option selections as repeated binary outcomes
#' with option-specific random difficulty/discrimination and optional gaze.
#' This preserves option-level observations but does **not** reproduce the 2026
#' MRM/MRM-LD likelihood. Use `engine = "external"` for a validated exact
#' implementation of a multiple-response model with inter-option dependence.
#'
#' @param data Long person-item-option table.
#' @param selected Binary option-selection indicator.
#' @param theta Supplied latent-trait estimate/score.
#' @param person,item,option Identifiers.
#' @param gaze Optional option-level process measure.
#' @param engine `reference` or `external`.
#' @param external_engine Validated external fitter.
#' @param ... Arguments passed to the external engine.
#' @export
fit_multiple_response_process_irt <- function(
    data, selected = "selected", theta = "theta",
    person = "participant_id", item = "item_id", option = "option_id",
    gaze = NULL, engine = c("reference", "external"),
    external_engine = NULL, ...) {
  engine <- match.arg(engine)
  if (engine == "external") {
    if (!is.function(external_engine))
      stop("Supply a validated multiple-response IRT fitter through `external_engine`.", call. = FALSE)
    return(structure(list(model = external_engine(data = data, ...), engine = "external",
                          exact_multiple_response = TRUE,
                          status = "experimental-external"),
                     class = "eye_multiple_response_process_irt"))
  }
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("Install optional package `lme4` for the reference option-selection model.", call. = FALSE)
  cols <- unique(c(selected, theta, person, item, option, gaze))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  d <- .ep07_model_frame(data, cols)
  d$.selected <- as.integer(as.logical(d[[selected]]))
  d$.theta <- as.numeric(d[[theta]])
  d$.person <- factor(d[[person]])
  d$.item_option <- interaction(d[[item]], d[[option]], drop = TRUE)
  if (!is.null(gaze)) {
    d$.gaze <- as.numeric(scale(log1p(pmax(as.numeric(d[[gaze]]), 0))))
    d$.gaze[!is.finite(d$.gaze)] <- 0
    f <- .selected ~ .theta * .gaze + (1 + .theta | .item_option) + (1 | .person)
  } else {
    f <- .selected ~ .theta + (1 + .theta | .item_option) + (1 | .person)
  }
  fit <- lme4::glmer(f, data = d, family = stats::binomial(), ...)
  structure(list(model = fit, data = d, gaze = gaze, engine = "reference",
                 exact_multiple_response = FALSE,
                 status = "experimental-reference",
                 note = paste(
                   "Option-level crossed logistic reference; it does not model the",
                   "full response-combination likelihood or MRM-LD inter-option dependence.")),
            class = "eye_multiple_response_process_irt")
}

#' Fit a revisiting-aware cognitive-diagnosis process workflow
#'
#' Convenience adapter motivated by current work combining response time and
#' item revisiting with cognitive diagnosis. The response layer is delegated to
#' `fit_cognitive_diagnosis_process()`; revisiting/RT/gaze remain process
#' evidence and do not redefine attributes or the Q-matrix.
#'
#' @param response_matrix Person-by-item responses.
#' @param q_matrix Item-by-attribute Q-matrix.
#' @param process_data Long process data.
#' @param person_id Person identifier in `process_data`.
#' @param revisited Revisit indicator/count column.
#' @param rt Response-time column.
#' @param gaze Optional gaze process columns.
#' @param ... Passed to `fit_cognitive_diagnosis_process()`.
#' @export
fit_revisit_process_cdm <- function(response_matrix, q_matrix, process_data,
                                    person_id = "participant_id",
                                    revisited = "revisited", rt = "response_time",
                                    gaze = NULL, ...) {
  pd <- .ep07_model_frame(process_data, unique(c(person_id, revisited, rt, gaze)))
  pd[[revisited]] <- as.numeric(pd[[revisited]])
  pd[[rt]] <- log1p(pmax(as.numeric(pd[[rt]]), 0))
  features <- unique(c(revisited, rt, gaze))
  fit <- fit_cognitive_diagnosis_process(
    response_matrix = response_matrix,
    q_matrix = q_matrix,
    process_data = pd,
    process_features = features,
    person_id = person_id,
    ...)
  fit$revisit_process_features <- features
  fit$note <- paste(
    "Revisiting and response time are retained as collateral process evidence;",
    "mastery labels remain defined by the supplied Q-matrix and response model.")
  class(fit) <- unique(c("eye_revisit_process_cdm", class(fit)))
  fit
}

#' Plot process/local-dependence diagnostics
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
plot.eye_process_local_dependence_audit <- function(x, ...) {
  z <- x$pairs
  if (!nrow(z)) {
    graphics::plot.new(); graphics::title(main = "Process local dependence")
    return(invisible(x))
  }
  labs <- paste(z$first, z$second, sep = " / ")
  graphics::dotchart(z$response_residual_correlation, labels = labs,
                     xlab = "Response residual correlation",
                     main = "Q3-style local-dependence audit", ...)
  graphics::abline(v = c(-x$threshold, x$threshold), lty = 3)
  invisible(z)
}
