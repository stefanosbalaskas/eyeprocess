# eyeprocess 0.9 Milestone #2: explicit adapters to mature IRT engines

.ep09m2_engine_registry <- function() {
  data.frame(
    engine = c("mirt", "TAM", "GDINA", "LNIRT", "eRm", "equateIRT", "catR", "mirtCAT"),
    capability = c(
      "multidimensional/polytomous/testlet/multiple-group/mixed IRT",
      "Rasch/2PL/3PL/GPCM/latent regression/plausible values",
      "cognitive diagnosis and Q-matrix workflows",
      "joint response and lognormal response-time IRT",
      "conditional Rasch/PCM/LLTM diagnostics",
      "IRT linking/equating and transformation stability",
      "unidimensional adaptive-testing simulation",
      "multidimensional CAT and constrained/shadow testing"
    ),
    package = c("mirt", "TAM", "GDINA", "LNIRT", "eRm", "equateIRT", "catR", "mirtCAT"),
    stringsAsFactors = FALSE
  )
}

#' Registry of specialized external IRT engines
#' @return An object of class "eye_irt_engine_registry", "data.frame", stored as a data frame, containing registry of specialized external IRT engines and associated metadata needed to interpret the result.
#' @export
eyeprocess_irt_engine_registry <- function() {
  x <- .ep09m2_engine_registry()
  x$available <- vapply(x$package, requireNamespace, logical(1), quietly = TRUE)
  structure(x, class = c("eye_irt_engine_registry", "data.frame"))
}

#' Query an external IRT engine
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_irt_engine_registry", "data.frame", stored as a data frame, containing query an external IRT engine and associated metadata needed to interpret the result.
#' @export
eyeprocess_irt_engine_status <- function(engine) {
  reg <- eyeprocess_irt_engine_registry(); engine <- as.character(engine)
  if (length(engine) != 1L || !engine %in% reg$engine) stop("Unknown engine. Available: ", paste(reg$engine, collapse = ", "), call. = FALSE)
  reg[reg$engine == engine, , drop = FALSE]
}

.ep09m2_gated_engine <- function(engine, call, reason = NULL) {
  structure(list(status = "gated", engine = engine, call = call, fit = NULL,
                 reason = if (is.null(reason)) paste0("Optional engine '", engine, "' is not installed.") else reason), class = "eye_gated_irt_engine")
}

#' Fit a model with mirt without substituting another estimator
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param model Model specification passed to the selected external engine.
#' @param itemtype Item type specification for mirt.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call". It contains a model with mirt without substituting another estimator and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_eyeprocess_mirt <- function(data, model = 1, itemtype = "2PL", ..., engine = "mirt") {
  if (!identical(engine, "mirt")) stop("fit_eyeprocess_mirt() only accepts engine='mirt'.", call. = FALSE)
  if (!requireNamespace("mirt", quietly = TRUE)) return(.ep09m2_gated_engine("mirt", match.call()))
  fit <- mirt::mirt(data = data, model = model, itemtype = itemtype, ...)
  structure(list(status = "fitted", engine = "mirt", fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Fit a TAM model without substituting another estimator
#' @param resp Response matrix supplied to TAM.
#' @param model Model specification passed to the selected external engine.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call". It contains a TAM model without substituting another estimator and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_eyeprocess_tam <- function(resp, model = c("rasch", "2pl", "gpcm"), ..., engine = "TAM") {
  if (!identical(engine, "TAM")) stop("fit_eyeprocess_tam() only accepts engine='TAM'.", call. = FALSE)
  model <- match.arg(model)
  if (!requireNamespace("TAM", quietly = TRUE)) return(.ep09m2_gated_engine("TAM", match.call()))
  fit <- switch(model,
    rasch = TAM::tam.mml(resp = resp, ...),
    `2pl` = TAM::tam.mml.2pl(resp = resp, irtmodel = "2PL", ...),
    gpcm = TAM::tam.mml.2pl(resp = resp, irtmodel = "GPCM", ...)
  )
  structure(list(status = "fitted", engine = "TAM", fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Fit a G-DINA cognitive-diagnosis model without fallback substitution
#' @param dat Response data supplied to the GDINA engine.
#' @param Q Binary item-by-attribute Q-matrix.
#' @param model Model specification passed to the selected external engine.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call". It contains a G-DINA cognitive-diagnosis model without fallback substitution and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_eyeprocess_gdina <- function(dat, Q, model = "GDINA", ..., engine = "GDINA") {
  if (!identical(engine, "GDINA")) stop("fit_eyeprocess_gdina() only accepts engine='GDINA'.", call. = FALSE)
  if (!requireNamespace("GDINA", quietly = TRUE)) return(.ep09m2_gated_engine("GDINA", match.call()))
  fit <- GDINA::GDINA(dat = dat, Q = Q, model = model, ...)
  structure(list(status = "fitted", engine = "GDINA", fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Fit a joint response/response-time LNIRT model without fallback substitution
#' @param Y Item-response matrix supplied to LNIRT.
#' @param RT Response-time matrix supplied to LNIRT.
#' @param quadratic Whether the LNIRT quadratic option is requested.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call", "rt_scale". It contains a joint response/response-time LNIRT model without fallback substitution and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_eyeprocess_lnirt <- function(Y, RT, quadratic = FALSE, ..., engine = "LNIRT") {
  if (!identical(engine, "LNIRT")) stop("fit_eyeprocess_lnirt() only accepts engine='LNIRT'.", call. = FALSE)
  if (!requireNamespace("LNIRT", quietly = TRUE)) return(.ep09m2_gated_engine("LNIRT", match.call()))
  RT <- as.matrix(RT); if (any(is.finite(RT) & RT <= 0)) stop("RT must be positive before log transformation.", call. = FALSE)
  logRT <- log(RT)
  fit <- if (isTRUE(quadratic)) LNIRT::LNIRTQ(Y = Y, RT = logRT, ...) else LNIRT::LNIRT(Y = Y, RT = logRT, ...)
  structure(list(status = "fitted", engine = "LNIRT", fit = fit, call = match.call(), rt_scale = "log"), class = "eye_external_irt_fit")
}

#' Fit an eRm Rasch-family model without fallback substitution
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param model Model specification passed to the selected external engine.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call". It contains an eRm Rasch-family model without fallback substitution and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_eyeprocess_erm <- function(data, model = c("RM", "PCM"), ..., engine = "eRm") {
  if (!identical(engine, "eRm")) stop("fit_eyeprocess_erm() only accepts engine='eRm'.", call. = FALSE)
  model <- match.arg(model)
  if (!requireNamespace("eRm", quietly = TRUE)) return(.ep09m2_gated_engine("eRm", match.call()))
  fit <- if (model == "RM") eRm::RM(data, ...) else eRm::PCM(data, ...)
  structure(list(status = "fitted", engine = "eRm", fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Run a catR adaptive-testing simulation without fallback substitution
#' @param itemBank Item bank supplied to catR.
#' @param trueTheta Known true latent-trait value used for CAT simulation.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call". It contains a catR adaptive-testing simulation without fallback substitution and associated metadata or diagnostics needed to interpret the result.
#' @export
simulate_eyeprocess_catr <- function(itemBank, trueTheta = 0, ..., engine = "catR") {
  if (!identical(engine, "catR")) stop("simulate_eyeprocess_catr() only accepts engine='catR'.", call. = FALSE)
  if (!requireNamespace("catR", quietly = TRUE)) return(.ep09m2_gated_engine("catR", match.call()))
  fit <- catR::randomCAT(trueTheta = trueTheta, itemBank = itemBank, ...)
  structure(list(status = "fitted", engine = "catR", fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Run a named equateIRT linking/equating function without fallback substitution
#'
#' The caller supplies an exported equateIRT function name and its arguments.
#' eyeprocess does not replace the requested equating estimator when the engine
#' is unavailable.
#' @param function_name Name of the external equateIRT function to call.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "function_name", "fit", "call". It contains a named equateIRT linking/equating function without fallback substitution and associated metadata or diagnostics needed to interpret the result.
#' @export
run_eyeprocess_equateirt <- function(function_name, ..., engine = "equateIRT") {
  if (!identical(engine, "equateIRT")) stop("run_eyeprocess_equateirt() only accepts engine='equateIRT'.", call. = FALSE)
  function_name <- as.character(function_name)
  if (length(function_name) != 1L || is.na(function_name) || !nzchar(function_name)) stop("function_name must be a non-empty scalar.", call. = FALSE)
  if (!requireNamespace("equateIRT", quietly = TRUE)) return(.ep09m2_gated_engine("equateIRT", match.call()))
  exports <- getNamespaceExports("equateIRT")
  if (!function_name %in% exports) stop("function_name is not an exported equateIRT function.", call. = FALSE)
  fun <- getExportedValue("equateIRT", function_name)
  if (!is.function(fun)) stop("Requested equateIRT export is not a function.", call. = FALSE)
  fit <- fun(...)
  structure(list(status = "fitted", engine = "equateIRT", function_name = function_name, fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Run a mirtCAT adaptive-testing workflow without fallback substitution
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param engine Requested estimation or analysis engine.
#' @return An object of class "eye_external_irt_fit", stored as a named list, with components "status", "engine", "fit", "call". It contains a mirtCAT adaptive-testing workflow without fallback substitution and associated metadata or diagnostics needed to interpret the result.
#' @export
run_eyeprocess_mirtcat <- function(..., engine = "mirtCAT") {
  if (!identical(engine, "mirtCAT")) stop("run_eyeprocess_mirtcat() only accepts engine='mirtCAT'.", call. = FALSE)
  if (!requireNamespace("mirtCAT", quietly = TRUE)) return(.ep09m2_gated_engine("mirtCAT", match.call()))
  fun <- getExportedValue("mirtCAT", "mirtCAT")
  fit <- fun(...)
  structure(list(status = "fitted", engine = "mirtCAT", fit = fit, call = match.call()), class = "eye_external_irt_fit")
}

#' Validate that an external IRT fit used the requested engine
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param engine Requested estimation or analysis engine.
#' @return A logical value or vector indicating that an external IRT fit used the requested engine.
#' @export
validate_eyeprocess_external_irt_fit <- function(x, engine = NULL) {
  if (!(inherits(x, "eye_external_irt_fit") || inherits(x, "eye_gated_irt_engine"))) stop("x is not an eyeprocess external IRT result.", call. = FALSE)
  if (!is.null(engine) && !identical(x$engine, engine)) stop("Engine mismatch.", call. = FALSE)
  if (inherits(x, "eye_external_irt_fit") && is.null(x$fit)) stop("Fitted external IRT result has NULL fit.", call. = FALSE)
  if (inherits(x, "eye_gated_irt_engine") && !is.null(x$fit)) stop("Gated external IRT result must have NULL fit.", call. = FALSE)
  invisible(TRUE)
}

#' @export
print.eye_gated_irt_engine <- function(x, ...) {
  cat("eyeprocess gated IRT engine\n")
  cat("  engine:", x$engine, "\n")
  cat("  status: gated\n")
  cat("  reason:", x$reason, "\n")
  invisible(x)
}

#' @export
print.eye_external_irt_fit <- function(x, ...) {
  cat("eyeprocess external IRT fit\n")
  cat("  engine:", x$engine, "\n")
  cat("  status:", x$status, "\n")
  invisible(x)
}
