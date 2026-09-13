# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Evidence-gated research-frontier interfaces. These functions intentionally do
# not fake exact estimators that have not been independently implemented and
# recovery-validated inside eyeprocess.

.ep08_gated_model <- function(id, purpose, required_evidence, engine = NULL,
                              fit = NULL, status = NULL, notes = NULL) {
  if (is.null(status)) status <- if (is.null(engine)) "gated" else "experimental_external_engine"
  structure(list(
    id = id, purpose = purpose, required_evidence = required_evidence,
    engine = engine, fit = fit, status = status, notes = notes,
    caveat = paste(
      "This research-frontier interface is evidence-gated.",
      "A named estimator is not treated as implemented merely because a simpler surrogate can be fit."
    )
  ), class = "eye_gated_process_model")
}

#' Print a gated process model object
#' @return Invisibly returns the input object after printing its summary; the object's class and contents are unchanged.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
print.eye_gated_process_model <- function(x, ...) {
  cat("<eye_gated_process_model>", x$id, "\n")
  cat(" status:", x$status, "\n")
  cat(" purpose:", x$purpose, "\n")
  cat(" required evidence:", paste(x$required_evidence, collapse = "; "), "\n")
  invisible(x)
}

#' Plot gated process model diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot gated process model diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_gated_process_model <- function(x, ...) {
  if (!is.null(x$fit) && !is.null(x$engine) && is.function(x$engine$plot)) {
    return(x$engine$plot(x$fit, ...))
  }
  graphics::plot.new(); graphics::title(main = paste("Gated frontier model:", x$id))
  graphics::text(.5, .58, paste("status:", x$status))
  graphics::text(.5, .46, "No validated internal plot is claimed for this gated estimator.")
  invisible(x)
}

#' Create a gated KDE latent-distribution IRT interface
#'
#' @param response_matrix Response data supplied to an optional external engine.
#' @param engine Optional function implementing the exact/nonparametric marginal
#'   likelihood estimator. If omitted, a gated specification is returned.
#' @param ... Passed to `engine` when supplied.
#' @return An object of class "eye_gated_process_model", stored as a named list, with components "id", "purpose", "required_evidence", "engine", "fit", "status", "notes", "caveat". It contains a gated KDE latent-distribution IRT interface and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_kde_latent_distribution_irt <- function(response_matrix, engine = NULL, ...) {
  req <- c("nonparametric latent-density integration inside the IRT marginal likelihood",
           "bandwidth-selection specification", "parameter-recovery simulation",
           "comparison against misspecified Gaussian latent distributions")
  if (is.null(engine)) return(.ep08_gated_model(
    "kde_latent_distribution_irt",
    "IRT calibration with a nonparametric KDE latent distribution rather than post-hoc density estimation.", req))
  if (!is.function(engine)) stop("engine must be NULL or a function.", call. = FALSE)
  fit <- engine(response_matrix, ...)
  .ep08_gated_model("kde_latent_distribution_irt",
                    "External exact/nonparametric latent-density IRT engine.", req,
                    engine = list(name = deparse(substitute(engine))), fit = fit)
}

#' Create a gated persistence-augmented gaze-diffusion IRT interface
#'
#' @param data Response/RT/process data.
#' @param engine Optional externally validated estimator function.
#' @param ... Passed to `engine`.
#' @return An object of class "eye_gated_process_model", stored as a named list, with components "id", "purpose", "required_evidence", "engine", "fit", "status", "notes", "caveat". It contains a gated persistence-augmented gaze-diffusion IRT interface and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_persistence_gaze_diffusion_irt <- function(data, engine = NULL, ...) {
  req <- c("separate capability, caution, and persistence identification",
           "competing censoring/persistence likelihood", "parameter recovery",
           "behavior-label guardrails")
  if (is.null(engine)) return(.ep08_gated_model(
    "persistence_gaze_diffusion_irt",
    "Extend gaze-diffusion measurement with a competing maximum-time/persistence process.", req))
  if (!is.function(engine)) stop("engine must be NULL or a function.", call. = FALSE)
  fit <- engine(data, ...)
  .ep08_gated_model("persistence_gaze_diffusion_irt",
                    "External persistence-augmented gaze-diffusion estimator.", req,
                    engine = list(name = deparse(substitute(engine))), fit = fit)
}

#' Create a gated Bayesian nonignorable-missing IRT interface
#'
#' @param data Response/missingness data.
#' @param engine Optional externally validated Bayesian estimator.
#' @param ... Passed to `engine`.
#' @return An object of class "eye_gated_process_model", stored as a named list, with components "id", "purpose", "required_evidence", "engine", "fit", "status", "notes", "caveat". It contains a gated Bayesian nonignorable-missing IRT interface and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_nonignorable_missing_irt <- function(data, engine = NULL, ...) {
  req <- c("explicit nonignorable missingness mechanism", "prior specification",
           "identifiability checks", "missingness-mechanism recovery simulations",
           "sensitivity to prior/missingness misspecification")
  if (is.null(engine)) return(.ep08_gated_model(
    "nonignorable_missing_irt",
    "Bayesian IRT with a jointly modeled nonignorable missingness mechanism.", req))
  if (!is.function(engine)) stop("engine must be NULL or a function.", call. = FALSE)
  fit <- engine(data, ...)
  .ep08_gated_model("nonignorable_missing_irt",
                    "External Bayesian nonignorable-missing IRT estimator.", req,
                    engine = list(name = deparse(substitute(engine))), fit = fit)
}

#' Prepare leakage-safe structured/unstructured process representations
#'
#' @param structured Structured person/item/window features.
#' @param unstructured Optional sequence/sample object.
#' @param fold Fold identifier. If supplied, representation builders are applied
#'   fold-locally using `builder`.
#' @param builder Optional function `(train_structured, train_unstructured,
#'   test_structured, test_unstructured, fold_value, ...)` returning a fold result.
#' @param id Optional identifier columns retained in the representation contract.
#' @param ... Passed to `builder`.
#' @return An object of class "eye_structured_unstructured_process_features", stored as a named list, with components "contract", "folds", "status". It contains leakage-safe structured/unstructured process representations and associated metadata or diagnostics needed to interpret the result.
#' @export
prepare_structured_unstructured_process_features <- function(
    structured, unstructured = NULL, fold = NULL, builder = NULL,
    id = c("person_id", "item_id"), ...) {
  structured <- .ep08_as_df(structured, "structured")
  id <- intersect(id, names(structured))
  contract <- list(
    structured_columns = names(structured), id_columns = id,
    has_unstructured = !is.null(unstructured), fold_column = fold,
    leakage_rule = "Any learned representation, scaling, vocabulary, embedding, or feature selection must be fitted inside the training fold only."
  )
  if (is.null(fold)) {
    return(structure(list(contract = contract, structured = structured,
                          unstructured = unstructured, status = "representation_contract_only"),
                     class = "eye_structured_unstructured_process_features"))
  }
  .ep08_req_cols(structured, fold)
  if (is.null(builder)) {
    return(structure(list(contract = contract, structured = structured,
                          unstructured = unstructured,
                          fold_registry = unique(structured[[fold]]),
                          status = "fold_registry_requires_builder"),
                     class = "eye_structured_unstructured_process_features"))
  }
  if (!is.function(builder)) stop("builder must be NULL or a function.", call. = FALSE)
  vals <- unique(structured[[fold]][!is.na(structured[[fold]])])
  if (length(vals) < 2L) stop("At least two non-missing folds are required for fold-local representation building.", call. = FALSE)
  results <- lapply(vals, function(v) {
    test_idx <- !is.na(structured[[fold]]) & structured[[fold]] == v
    train_s <- structured[!test_idx, , drop = FALSE]
    test_s <- structured[test_idx, , drop = FALSE]
    train_u <- unstructured; test_u <- unstructured
    if (is.data.frame(unstructured) && fold %in% names(unstructured)) {
      train_u <- unstructured[!is.na(unstructured[[fold]]) & unstructured[[fold]] != v, , drop = FALSE]
      test_u <- unstructured[!is.na(unstructured[[fold]]) & unstructured[[fold]] == v, , drop = FALSE]
    }
    builder(train_s, train_u, test_s, test_u, v, ...)
  })
  names(results) <- as.character(vals)
  structure(list(contract = contract, folds = results,
                 status = "fold_local_representation_built"),
            class = "eye_structured_unstructured_process_features")
}

#' Create a gated scalable cross-classified MH-RM process IRT interface
#'
#' @param data Data supplied to an optional external engine.
#' @param engine Optional estimator implementing the intended scalable MH-RM model.
#' @param ... Passed to engine.
#' @return An object of class "eye_gated_process_model", stored as a named list, with components "id", "purpose", "required_evidence", "engine", "fit", "status", "notes", "caveat". It contains a gated scalable cross-classified MH-RM process IRT interface and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_crossclassified_process_irt_mhrm <- function(data, engine = NULL, ...) {
  req <- c("cross-classified outcome/process likelihood", "study-design nesting",
           "scalable MH-RM estimation", "parameter recovery across cluster sizes",
           "runtime/memory scaling benchmark")
  if (is.null(engine)) return(.ep08_gated_model(
    "crossclassified_process_irt_mhrm",
    "Scalable MH-RM estimation for cross-classified outcome/process IRT.", req))
  if (!is.function(engine)) stop("engine must be NULL or a function.", call. = FALSE)
  fit <- engine(data, ...)
  .ep08_gated_model("crossclassified_process_irt_mhrm",
                    "External scalable cross-classified process-IRT estimator.", req,
                    engine = list(name = deparse(substitute(engine))), fit = fit)
}

#' Audit whether a gated frontier model has a minimum evidence contract
#' @param x Gated model object.
#' @param evidence Named evidence objects.
#' @return A data frame containing whether a gated frontier model has a minimum evidence contract. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
audit_frontier_model_contract <- function(x, evidence = list()) {
  if (!inherits(x, "eye_gated_process_model")) stop("x must be eye_gated_process_model.", call. = FALSE)
  if (!is.list(evidence)) stop("evidence must be a list.", call. = FALSE)
  supplied <- names(evidence)
  data.frame(
    model = x$id,
    engine_supplied = !is.null(x$fit),
    n_required_evidence_elements = length(x$required_evidence),
    n_named_evidence_objects = length(supplied),
    status = if (!is.null(x$fit) && length(supplied) >= 3L) "candidate_for_validation_review" else "remains_gated",
    stringsAsFactors = FALSE
  )
}
