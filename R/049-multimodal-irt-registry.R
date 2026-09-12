# eyeprocess 0.7.0.9000 -------------------------------------------------------
# Multimodal IRT registry and channel architecture.

.eyeprocess_irt_registry <- new.env(parent = emptyenv())

.ep07_channel <- function(type, family, role, link = NULL, variables = NULL,
                          latent = NULL, options = list()) {
  type <- .ep07_scalar_chr(type, "type")
  family <- .ep07_scalar_chr(family, "family")
  role <- .ep07_scalar_chr(role, "role")
  structure(list(type = type, family = family, role = role, link = link,
                 variables = variables, latent = latent, options = options),
            class = c(paste0("eye_irt_", type, "_channel"), "eye_irt_channel"))
}

#' Binary/ordinal response channel for multimodal IRT
#' @param family Statistical family used by the channel or model.
#' @param response Response variable or response-column name.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains binary/ordinal response channel for multimodal IRT and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_response_channel <- function(family = c("2pl", "rasch", "graded", "partial_credit"),
                                 response = "response", latent = "ability", options = list()) {
  family <- match.arg(family)
  .ep07_channel("response", family, "measurement", variables = response, latent = latent, options = options)
}

#' Response-time channel for multimodal IRT
#' @param family Statistical family used by the channel or model.
#' @param rt Response-time variable or column name.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains response-time channel for multimodal IRT and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_rt_channel <- function(family = c("lognormal", "gaussian_log", "shifted_lognormal"),
                           rt = "rt", latent = "speed", options = list()) {
  family <- match.arg(family)
  .ep07_channel("rt", family, "process", variables = rt, latent = latent, options = options)
}

#' Count-valued process channel for multimodal IRT
#' @param family Statistical family used by the channel or model.
#' @param value Process-value column or values.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains count-valued process channel for multimodal IRT and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_count_channel <- function(family = c("negative_binomial", "poisson"),
                              value = "fixation_count", latent = "engagement", options = list()) {
  family <- match.arg(family)
  .ep07_channel("count", family, "process", variables = value, latent = latent, options = options)
}

#' Survival/event-time channel for multimodal IRT
#' @param family Statistical family used by the channel or model.
#' @param time Time values.
#' @param event Event indicator or event column.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains survival/event-time channel for multimodal IRT and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_survival_channel <- function(family = c("cox", "weibull", "exponential"),
                                 time = "time", event = "event", latent = NULL, options = list()) {
  family <- match.arg(family)
  .ep07_channel("survival", family, "process", variables = c(time = time, event = event), latent = latent, options = options)
}

#' Nominal response/process channel
#' @param choice Observed nominal choice.
#' @param categories Nominal response categories.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains nominal response/process channel and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_nominal_channel <- function(choice = "response_option", categories = NULL,
                                latent = "ability", options = list()) {
  .ep07_channel("nominal", "nominal", "measurement", variables = choice,
                latent = latent, options = c(options, list(categories = categories)))
}

#' Compositional AOI channel
#' @param parts Compositional parts.
#' @param family Statistical family used by the channel or model.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains compositional AOI channel and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_compositional_channel <- function(parts, family = c("logratio_gaussian", "dirichlet"),
                                      latent = "process", options = list()) {
  family <- match.arg(family)
  if (missing(parts) || length(parts) < 2L) stop("`parts` must name at least two compositional variables.", call. = FALSE)
  .ep07_channel("compositional", family, "process", variables = as.character(parts), latent = latent, options = options)
}

#' Sequence/process-state channel
#' @param sequence Sequence input.
#' @param family Statistical family used by the channel or model.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains sequence/process-state channel and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_sequence_channel <- function(sequence = "sequence", family = c("ngram", "hmm", "embedding"),
                                 latent = "strategy", options = list()) {
  family <- match.arg(family)
  .ep07_channel("sequence", family, "process", variables = sequence, latent = latent, options = options)
}

#' Functional trajectory channel
#' @param value Process-value column or values.
#' @param time Time values.
#' @param family Statistical family used by the channel or model.
#' @param latent Latent variable or latent-variable labels.
#' @param options Additional channel/model options.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains functional trajectory channel and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_functional_channel <- function(value = "pupil", time = "time",
                                   family = c("basis_gaussian", "functional_factor"),
                                   latent = "process", options = list()) {
  family <- match.arg(family)
  .ep07_channel("functional", family, "process", variables = c(value = value, time = time), latent = latent, options = options)
}

#' Define a multimodal IRT model specification
#'
#' @param id Stable model identifier.
#' @param latent Named or unnamed latent dimensions.
#' @param channels Named list of channel objects.
#' @param status One of reference, experimental, or gated.
#' @param fit_fun Optional fitting function.
#' @param simulate_fun Optional simulator.
#' @param validate_fun Optional model-specific validation function.
#' @param citation Character vector of citations/DOIs.
#' @param description Human-readable model description.
#' @param requirements Optional packages/engines.
#' @param metadata Additional metadata.
#' @return An `eye_irt_model_spec`.
#' @export
irt_model_spec <- function(id, latent, channels,
                           status = c("experimental", "reference", "gated"),
                           fit_fun = NULL, simulate_fun = NULL, validate_fun = NULL,
                           citation = character(), description = NULL,
                           requirements = character(), metadata = list()) {
  id <- .ep07_scalar_chr(id, "id")
  status <- match.arg(status)
  if (!is.list(channels) || !length(channels) ||
      !all(vapply(channels, inherits, logical(1), what = "eye_irt_channel"))) {
    stop("`channels` must be a non-empty list of irt_*_channel() objects.", call. = FALSE)
  }
  latent <- unique(as.character(latent))
  if (!length(latent) || anyNA(latent) || any(!nzchar(latent))) stop("`latent` must contain named latent dimensions.", call. = FALSE)
  for (fn in list(fit_fun, simulate_fun, validate_fun)) {
    if (!is.null(fn) && !is.function(fn)) stop("Model callbacks must be functions or NULL.", call. = FALSE)
  }
  structure(list(
    id = id, latent = latent, channels = channels, status = status,
    fit_fun = fit_fun, simulate_fun = simulate_fun, validate_fun = validate_fun,
    citation = as.character(citation), description = description,
    requirements = as.character(requirements), metadata = metadata
  ), class = "eye_irt_model_spec")
}

#' Register a multimodal IRT model
#' @param spec An `irt_model_spec()`.
#' @param overwrite Whether to replace an existing model with the same id.
#' @return An R object containing a multimodal IRT model. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
register_irt_model <- function(spec, overwrite = FALSE) {
  if (!inherits(spec, "eye_irt_model_spec")) stop("`spec` must come from irt_model_spec().", call. = FALSE)
  id <- spec$id
  if (exists(id, envir = .eyeprocess_irt_registry, inherits = FALSE) && !isTRUE(overwrite)) {
    stop(sprintf("IRT model `%s` is already registered. Use `overwrite = TRUE` deliberately.", id), call. = FALSE)
  }
  assign(id, spec, envir = .eyeprocess_irt_registry)
  invisible(spec)
}

.ep07_register_builtin_irt <- function() {
  builtin <- list(
    irt_model_spec(
      "joint_gaze_rt",
      latent = c("ability", "speed", "engagement"),
      channels = list(
        response = irt_response_channel("2pl"),
        rt = irt_rt_channel("lognormal"),
        gaze = irt_count_channel("negative_binomial")
      ),
      status = "reference", fit_fun = fit_joint_gaze_rt_irt,
      citation = "10.1177/01466216221089344",
      description = "Response + RT + gaze-count joint measurement architecture."
    ),
    irt_model_spec(
      "nominal_gaze",
      latent = c("ability", "option_process"),
      channels = list(
        response = irt_nominal_channel(),
        gaze = irt_compositional_channel(c("option_A", "option_B"))
      ),
      status = "experimental", fit_fun = fit_nominal_gaze_irt,
      description = "Nominal response choices integrated with option-level gaze evidence."
    ),
    irt_model_spec(
      "omission_survival",
      latent = c("ability", "speed", "omission_process"),
      channels = list(
        response = irt_response_channel("2pl"),
        time = irt_survival_channel()
      ),
      status = "experimental", fit_fun = fit_omission_survival_irt,
      description = "Separates observed responses, omissions, and not-reached observations."
    ),
    irt_model_spec(
      "manyfacet_process",
      latent = c("person", "item", "process"),
      channels = list(response = irt_response_channel("rasch"), gaze = irt_count_channel()),
      status = "reference", fit_fun = fit_manyfacet_process_irt,
      description = "Crossed person/item/device/session/algorithm facet model."
    ),
    irt_model_spec(
      "process_hmm",
      latent = c("ability", "process_state"),
      channels = list(response = irt_response_channel("2pl"), process = irt_sequence_channel(family = "hmm")),
      status = "experimental", fit_fun = fit_process_hmm_irt,
      description = "Two-stage process-state HMM plus response measurement reference engine."
    ),
    irt_model_spec(
      "graded_rt_process",
      latent = c("ability", "speed", "process"),
      channels = list(
        response = irt_response_channel("graded"),
        rt = irt_rt_channel("lognormal"),
        process = irt_count_channel("negative_binomial")
      ),
      status = "experimental", fit_fun = fit_joint_graded_rt_process_irt,
      description = "Mixed/graded response extension with response-time and process channels."
    ),
    irt_model_spec(
      "latent_space_process",
      latent = c("ability", "interaction_space"),
      channels = list(response = irt_response_channel("graded")),
      status = "experimental", fit_fun = fit_latent_space_irt,
      requirements = "LSMjml",
      description = "Person-item latent-space adapter for residual interaction structure."
    ),
    irt_model_spec(
      "gpirt_shape_audit",
      latent = c("ability"),
      channels = list(response = irt_response_channel("2pl")),
      status = "gated", fit_fun = fit_gpirt,
      description = "Flexible item-response-curve audit; exact GP engine requires an external callback."
    ),
    irt_model_spec(
      "flow_mirt",
      latent = c("ability_1", "ability_2"),
      channels = list(response = irt_response_channel("2pl")),
      status = "gated", fit_fun = fit_flow_mirt,
      description = "Normalizing-flow MIRT research gate; no production claim without external engine and recovery evidence."
    ),
    irt_model_spec(
      "multiple_response_process",
      latent = c("ability", "option_process"),
      channels = list(
        response = irt_nominal_channel(),
        process = irt_compositional_channel(c("selected", "not_selected"))
      ),
      status = "experimental", fit_fun = fit_multiple_response_process_irt,
      citation = "10.1017/psy.2025.10073",
      description = "Multiple-response option/process reference model; exact MRM/MRM-LD requires a validated external engine."
    ),
    irt_model_spec(
      "bounded_continuous_process",
      latent = c("process_trait"),
      channels = list(process = irt_continuous_channel("censored_normal")),
      status = "experimental", fit_fun = fit_censored_normal_process_irt,
      citation = "10.1007/s41237-026-00292-x",
      description = "Conditional censored-normal calibration for bounded process measurements."
    )
  )
  for (sp in builtin) {
    if (!exists(sp$id, envir = .eyeprocess_irt_registry, inherits = FALSE)) assign(sp$id, sp, envir = .eyeprocess_irt_registry)
  }
  invisible(TRUE)
}

#' List registered multimodal IRT models
#' @return A tabular R object containing list registered multimodal IRT models; rows represent analysis units and columns contain the returned quantities.
#' @export
list_irt_models <- function() {
  .ep07_register_builtin_irt()
  ids <- sort(ls(envir = .eyeprocess_irt_registry, all.names = TRUE))
  if (!length(ids)) return(data.frame())
  do.call(rbind, lapply(ids, function(id) {
    z <- get(id, envir = .eyeprocess_irt_registry, inherits = FALSE)
    data.frame(
      id = z$id,
      status = z$status,
      latent = paste(z$latent, collapse = ", "),
      channels = paste(names(z$channels), collapse = ", "),
      requirements = paste(z$requirements, collapse = ", "),
      description = if (is.null(z$description)) "" else z$description,
      stringsAsFactors = FALSE
    )
  }))
}

#' Retrieve a registered multimodal IRT model
#' @param id Stable identifier.
#' @return An R object containing retrieve a registered multimodal IRT model. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
get_irt_model <- function(id) {
  .ep07_register_builtin_irt()
  id <- .ep07_scalar_chr(id, "id")
  if (!exists(id, envir = .eyeprocess_irt_registry, inherits = FALSE)) {
    stop(sprintf("Unknown IRT model `%s`. See list_irt_models().", id), call. = FALSE)
  }
  get(id, envir = .eyeprocess_irt_registry, inherits = FALSE)
}

#' Fit a registered multimodal IRT model
#' @param spec IRT model or validation specification.
#' @param data Input data frame or compatible tabular object.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @param allow_experimental Whether experimental models are permitted.
#' @return An R object containing a registered multimodal IRT model. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
fit_irt_model <- function(spec, data, ..., allow_experimental = FALSE) {
  if (is.character(spec)) spec <- get_irt_model(spec)
  if (!inherits(spec, "eye_irt_model_spec")) stop("`spec` must be a model id or eye_irt_model_spec.", call. = FALSE)
  if (spec$status %in% c("experimental", "gated") && !isTRUE(allow_experimental)) {
    stop(sprintf("Model `%s` is %s. Set `allow_experimental = TRUE` only for validation/research use.",
                 spec$id, spec$status), call. = FALSE)
  }
  if (is.null(spec$fit_fun)) stop(sprintf("Model `%s` has no bundled fitter.", spec$id), call. = FALSE)
  fit <- spec$fit_fun(data = data, ...)
  attr(fit, "eye_irt_model_spec") <- spec
  fit
}

#' Simulate from a registered multimodal IRT model
#' @param spec IRT model or validation specification.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @param allow_experimental Whether experimental models are permitted.
#' @return An R object containing from a registered multimodal IRT model. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
simulate_irt_model <- function(spec, ..., allow_experimental = TRUE) {
  if (is.character(spec)) spec <- get_irt_model(spec)
  if (!inherits(spec, "eye_irt_model_spec")) stop("`spec` must be a model id or eye_irt_model_spec.", call. = FALSE)
  if (is.null(spec$simulate_fun)) {
    stop(sprintf("Model `%s` does not register a simulator. Supply one through irt_model_spec().", spec$id), call. = FALSE)
  }
  if (!allow_experimental && spec$status != "reference") stop("Experimental model simulation is disabled.", call. = FALSE)
  spec$simulate_fun(...)
}

#' Validate a registered multimodal IRT model
#' @param spec IRT model or validation specification.
#' @param validation Validation results or validation specification.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return An object of class "eye_irt_evidence_grade", stored as a named list, with components "model_id", "grade", "checks", "recovery", "contract", "warning". It contains a registered multimodal IRT model and associated metadata or diagnostics needed to interpret the result.
#' @export
validate_irt_model <- function(spec, validation = NULL, ...) {
  if (is.character(spec)) spec <- get_irt_model(spec)
  if (!inherits(spec, "eye_irt_model_spec")) stop("`spec` must be a model id or eye_irt_model_spec.", call. = FALSE)
  if (!is.null(spec$validate_fun)) return(spec$validate_fun(validation = validation, ...))
  if (is.null(validation)) stop("Supply a validation object or register a model-specific validator.", call. = FALSE)
  grade_model_evidence(validation, ...)
}

#' Compare multimodal IRT model objects
#'
#' Uses supplied scoring functions when models expose different engines. The
#' default extracts AIC/BIC/logLik when available and never treats in-sample fit
#' as sufficient evidence for model promotion.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @param names Value supplied to `names`; see Details for its model-specific role.
#' @return An object of class "eye_irt_model_comparison", stored as an R object, containing multimodal IRT model objects and associated metadata needed to interpret the result.
#' @export
compare_irt_models <- function(..., names = NULL) {
  fits <- list(...)
  if (!length(fits)) stop("Supply at least one fitted model.", call. = FALSE)
  if (is.null(names)) names <- names(fits)
  if (is.null(names) || any(!nzchar(names))) names <- paste0("model_", seq_along(fits))
  extract <- function(fit) {
    ll <- try(stats::logLik(fit), silent = TRUE)
    aic <- try(stats::AIC(fit), silent = TRUE)
    bic <- try(stats::BIC(fit), silent = TRUE)
    data.frame(
      logLik = if (inherits(ll, "try-error")) NA_real_ else as.numeric(ll),
      AIC = if (inherits(aic, "try-error")) NA_real_ else as.numeric(aic),
      BIC = if (inherits(bic, "try-error")) NA_real_ else as.numeric(bic),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, Map(function(f, n) cbind(model = n, extract(f)), fits, names))
  rownames(out) <- NULL
  structure(out, class = c("eye_irt_model_comparison", class(out)))
}

#' Promote an IRT model after evidence gates are met
#'
#' Promotion is an evidence record, not a mutable global status change unless
#' `update_registry = TRUE` is requested.
#' @param spec IRT model or validation specification.
#' @param evidence Validation evidence used for promotion.
#' @param target Target evidence/status level.
#' @param update_registry Whether the in-memory registry is updated.
#' @return An object of class "eye_irt_promotion", stored as a named list, with components "model", "from", "to", "evidence_grade", "evidence_pass", "timestamp". It contains promote an IRT model after evidence gates are met and associated metadata or diagnostics needed to interpret the result.
#' @export
promote_irt_model <- function(spec, evidence,
                              target = c("experimental", "reference"),
                              update_registry = FALSE) {
  target <- match.arg(target)
  if (is.character(spec)) spec <- get_irt_model(spec)
  if (!inherits(spec, "eye_irt_model_spec")) stop("`spec` must be a registered model or model spec.", call. = FALSE)
  grade <- grade_model_evidence(evidence)
  if (target == "reference" && !isTRUE(grade$pass)) {
    stop(sprintf("Model `%s` cannot be promoted to reference: evidence grade is %s.", spec$id, grade$grade), call. = FALSE)
  }
  promoted <- spec
  promoted$status <- target
  record <- structure(list(model = spec$id, from = spec$status, to = target,
                           evidence_grade = grade$grade, evidence_pass = grade$pass,
                           timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE)),
                      class = "eye_irt_promotion")
  if (isTRUE(update_registry)) register_irt_model(promoted, overwrite = TRUE)
  record
}

#' Print a multimodal IRT model specification
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the input object after printing its summary; the object's class and contents are unchanged.
#' @export
print.eye_irt_model_spec <- function(x, ...) {
  cat("<eye_irt_model_spec>", x$id, "\n")
  cat(" status:", x$status, "\n")
  cat(" latent:", paste(x$latent, collapse = ", "), "\n")
  cat(" channels:", paste(names(x$channels), collapse = ", "), "\n")
  if (length(x$citation)) cat(" citation:", paste(x$citation, collapse = "; "), "\n")
  invisible(x)
}
