# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Additional sensitivity/diagnostic adapters derived from the advanced template:
# true mirt mixture IRT, nonparametric Rasch checks, item-reduction sensitivity,
# biometric missingness imputation, and response-process Rasch trees.

#' Fit a true mirt mixture-IRT response model
#'
#' This is distinct from the existing two-stage process-class reference model:
#' the response distribution itself is calibrated using mirt's mixture density.
#' Process features may then be compared with the fitted response classes only
#' when a defensible class-membership extraction is available.
#'
#' @param response_matrix Person x item response matrix.
#' @param n_classes Number of mixture classes. The currently verified internal
#'   route supports two classes (`mixture-2`).
#' @param model mirt model specification, default one dimension.
#' @param itemtype Item type.
#' @param SE Request standard errors.
#' @return An object of class "eye_mixture_irt_process", stored as a named list, with components "model", "coefficients", "n_classes", "itemtype", "status", "caveat". It contains a true mirt mixture-IRT response model and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_mixture_irt_process_classes <- function(
    response_matrix, n_classes = 2L, model = 1, itemtype = "2PL", SE = FALSE) {
  if (!requireNamespace("mirt", quietly = TRUE))
    stop("Package `mirt` is required for mixture IRT.", call. = FALSE)
  n_classes <- as.integer(n_classes)
  if (n_classes != 2L)
    stop("The internally verified route currently supports n_classes = 2 only.", call. = FALSE)
  X <- as.data.frame(response_matrix)
  if (nrow(X) < 50L || ncol(X) < 4L)
    warning("Mixture IRT can be unstable with small person/item counts; validate recovery before interpretation.", call. = FALSE)
  fit <- mirt::multipleGroup(
    data = X, model = model, itemtype = itemtype,
    dentype = "mixture-2", SE = SE, verbose = FALSE
  )
  co <- tryCatch(mirt::coef(fit, IRTpars = TRUE, simplify = FALSE), error = function(e) NULL)
  structure(list(
    model = fit, coefficients = co, n_classes = n_classes,
    itemtype = itemtype, status = "experimental_response_mixture_irt",
    caveat = paste(
      "Mixture classes are latent response-distribution components.",
      "They are not automatically cognitive strategies or psychological types; process evidence and recovery validation are required."
    )
  ), class = "eye_mixture_irt_process")
}

#' Map supplied latent-class memberships to process summaries
#'
#' @param class_membership Data containing person/class assignments or probabilities.
#' @param process_data Person-level or trial-level process data.
#' @param person Person identifier present in both objects.
#' @param class_col Class-assignment column.
#' @param process_features Numeric process features.
#' @return An object of class "eye_latent_process_alignment", stored as a named list, with components "data", "summary", "process_features", "class_col", "caveat". It contains supplied latent-class memberships to process summaries and associated metadata or diagnostics needed to interpret the result.
#' @export
map_latent_classes_to_process_profiles <- function(
    class_membership, process_data, person = "person_id", class_col = "class",
    process_features) {
  class_membership <- .ep08_as_df(class_membership, "class_membership")
  process_data <- .ep08_as_df(process_data, "process_data")
  .ep08_req_cols(class_membership, c(person, class_col), "class_membership")
  if (!length(process_features)) stop("Supply at least one process feature.", call. = FALSE)
  .ep08_req_cols(process_data, c(person, process_features), "process_data")
  if (anyDuplicated(as.character(class_membership[[person]])))
    stop("class_membership must contain one assignment row per person for class-profile mapping.", call. = FALSE)
  p <- process_data[, c(person, process_features), drop = FALSE]
  for (v in process_features) p[[v]] <- .ep08_num(p[[v]])
  p <- stats::aggregate(p[process_features], by = list(person_id = as.character(p[[person]])), FUN = .ep08_mean)
  names(p)[1L] <- person
  cm <- class_membership[, c(person, class_col), drop = FALSE]
  cm[[person]] <- as.character(cm[[person]])
  d <- merge(cm, p, by = person, all.x = TRUE)
  summary <- stats::aggregate(d[process_features], by = list(class = d[[class_col]]), FUN = .ep08_mean)
  structure(list(data = d, summary = summary, process_features = process_features,
                 class_col = class_col,
                 caveat = "Process summaries can aid interpretation of latent response classes but cannot prove substantive strategy labels."),
            class = "eye_latent_process_alignment")
}

#' Run nonparametric Rasch diagnostics with eRm
#'
#' @param response_matrix Dichotomous response matrix.
#' @param methods eRm NPtest methods, e.g. T1 and T10.
#' @param n Number of sampled matrices.
#' @param splitcr Split criterion for tests that use one.
#' @param seed Seed.
#' @return An object of class "eye_nonparametric_rasch_audit", stored as a named list, with components "tests", "status", "n", "splitcr", "caveat". It contains nonparametric Rasch diagnostics with eRm and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_nonparametric_rasch <- function(
    response_matrix, methods = c("T1", "T10"), n = 100L,
    splitcr = "median", seed = 321) {
  if (!requireNamespace("eRm", quietly = TRUE))
    stop("Package `eRm` is required for nonparametric Rasch diagnostics.", call. = FALSE)
  methods <- unique(as.character(methods))
  methods <- methods[nzchar(methods)]
  if (!length(methods)) stop("Supply at least one NPtest method.", call. = FALSE)
  n <- as.integer(n)
  if (n < 1L) stop("n must be positive.", call. = FALSE)
  X <- as.matrix(response_matrix)
  tests <- lapply(seq_along(methods), function(i) {
    method <- methods[i]
    args <- list(X, n = as.integer(n), method = method, seed = as.integer(seed) + i - 1L)
    if (identical(method, "T10")) args$splitcr <- splitcr
    tryCatch(do.call(eRm::NPtest, args), error = function(e) e)
  })
  names(tests) <- methods
  status <- data.frame(
    method = methods,
    status = vapply(tests, function(z) if (inherits(z, "error")) "failed" else "completed", character(1)),
    interpretation = ifelse(methods == "T1",
                            "Potential local item dependence if the relevant test evidence is small/atypical.",
                            "Potential subgroup/item-difficulty instability if the relevant test evidence is small/atypical."),
    stringsAsFactors = FALSE
  )
  structure(list(
    tests = tests, status = status, n = as.integer(n), splitcr = splitcr,
    caveat = "Nonparametric Rasch tests are diagnostic evidence and should be interpreted with item content, DIF, local dependence, and process data."
  ), class = "eye_nonparametric_rasch_audit")
}

#' Run stepwise Rasch item-reduction as a sensitivity analysis
#'
#' @param erm_model Fitted eRm model.
#' @param criterion Criterion list passed to `eRm::stepwiseIt()`.
#' @param alpha Significance threshold.
#' @param maxstep Maximum elimination steps.
#' @return An object of class "eye_item_reduction_sensitivity", stored as a named list, with components "model", "eliminated_items", "alpha", "maxstep", "status", "caveat". It contains stepwise Rasch item-reduction as a sensitivity analysis and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_item_reduction_sensitivity <- function(
    erm_model, criterion = list("itemfit"), alpha = 0.05, maxstep = 5L) {
  if (!requireNamespace("eRm", quietly = TRUE))
    stop("Package `eRm` is required for item-reduction sensitivity.", call. = FALSE)
  if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1).", call. = FALSE)
  maxstep <- as.integer(maxstep)
  if (maxstep < 1L) stop("maxstep must be positive.", call. = FALSE)
  fit <- eRm::stepwiseIt(erm_model, criterion = criterion, alpha = alpha,
                         verbose = FALSE, maxstep = as.integer(maxstep))
  eliminated <- if (!is.null(fit$it.elim)) as.character(fit$it.elim) else character()
  structure(list(
    model = fit, eliminated_items = eliminated, alpha = alpha, maxstep = as.integer(maxstep),
    status = "item_reduction_sensitivity_only",
    caveat = paste(
      "Automated item elimination must not determine the operational scale by itself.",
      "Content validity, theoretical coverage, DIF, local dependence, and response-process evidence remain required."
    )
  ), class = "eye_item_reduction_sensitivity")
}

#' Run biometric-feature imputation as a sensitivity analysis
#'
#' @param data Data containing biometric/process features.
#' @param variables Variables to impute.
#' @param methods Any of `mice` and `missForest`.
#' @param m Number of MICE imputations.
#' @param maxit Iteration count.
#' @param seed Seed.
#' @return An `eye_biometric_imputation_sensitivity` object. Complete-case
#'   analysis is not replaced automatically.
#' @export
biometric_imputation_sensitivity <- function(
    data, variables, methods = c("mice", "missForest"), m = 3L, maxit = 3L, seed = 521) {
  data <- .ep08_as_df(data)
  if (!length(variables)) stop("Supply at least one variable to impute.", call. = FALSE)
  .ep08_req_cols(data, variables)
  methods <- intersect(unique(as.character(methods)), c("mice", "missForest"))
  m <- as.integer(m); maxit <- as.integer(maxit)
  if (m < 1L || maxit < 1L) stop("m and maxit must be positive.", call. = FALSE)
  x <- data[, variables, drop = FALSE]
  missingness <- data.frame(
    variable = variables,
    missing_prop = vapply(x, function(z) mean(is.na(z)), numeric(1)),
    stringsAsFactors = FALSE
  )
  results <- list()
  status <- data.frame(method = methods, status = rep("not_run", length(methods)), stringsAsFactors = FALSE)
  if ("mice" %in% methods) {
    if (requireNamespace("mice", quietly = TRUE)) {
      fit <- tryCatch(mice::mice(x, m = as.integer(m), maxit = as.integer(maxit),
                                 printFlag = FALSE, seed = seed), error = function(e) e)
      if (!inherits(fit, "error")) {
        results$mice <- list(model = fit, completed = mice::complete(fit, action = 1L))
        status$status[status$method == "mice"] <- "completed"
      } else {
        results$mice <- fit; status$status[status$method == "mice"] <- "failed"
      }
    } else status$status[status$method == "mice"] <- "package_unavailable"
  }
  if ("missForest" %in% methods) {
    if (requireNamespace("missForest", quietly = TRUE)) {
      fit <- tryCatch(missForest::missForest(x, maxiter = as.integer(maxit), ntree = 100,
                                             verbose = FALSE), error = function(e) e)
      if (!inherits(fit, "error")) {
        results$missForest <- list(model = fit, completed = fit$ximp, oob_error = fit$OOBerror)
        status$status[status$method == "missForest"] <- "completed"
      } else {
        results$missForest <- fit; status$status[status$method == "missForest"] <- "failed"
      }
    } else status$status[status$method == "missForest"] <- "package_unavailable"
  }
  structure(list(
    missingness = missingness, results = results, status = status, variables = variables,
    caveat = paste(
      "Imputed biometric datasets are sensitivity analyses by default.",
      "Do not silently replace complete-case or explicitly modeled missingness analyses."
    )
  ), class = "eye_biometric_imputation_sensitivity")
}

#' Alias emphasizing sensitivity rather than automatic replacement
#' @return An R object containing alias emphasizing sensitivity rather than automatic replacement. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
audit_biometric_imputation <- function(...) biometric_imputation_sensitivity(...)

#' Fit a process-informed Rasch tree
#'
#' @param response_matrix Person x item dichotomous response matrix.
#' @param covariates Person-level response-process covariates.
#' @param formula Optional splitting formula. If omitted, all covariates are used.
#' @param maxit Maximum model iterations.
#' @return An object of class "eye_process_rasch_tree", stored as a named list, with components "model", "covariates", "formula", "status", "caveat". It contains a process-informed Rasch tree and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_process_rasch_tree <- function(response_matrix, covariates, formula = NULL, maxit = 60L) {
  if (!requireNamespace("psychotree", quietly = TRUE))
    stop("Package `psychotree` is required for process-informed Rasch trees.", call. = FALSE)
  X <- as.matrix(response_matrix)
  covariates <- .ep08_as_df(covariates, "covariates")
  if (nrow(X) != nrow(covariates)) stop("response_matrix and covariates must have the same number of persons.", call. = FALSE)
  d <- covariates
  d$resp <- I(X)
  if (is.null(formula)) {
    rhs <- names(covariates)
    if (!length(rhs)) stop("At least one splitting covariate is required.", call. = FALSE)
    formula <- stats::reformulate(rhs, response = "resp")
  }
  fit <- psychotree::raschtree(formula, data = d, maxit = as.integer(maxit))
  structure(list(
    model = fit, covariates = covariates, formula = formula,
    status = "rasch_tree_heterogeneity_diagnostic",
    caveat = paste(
      "Tree splits indicate item-parameter heterogeneity/DIF structure conditional on process covariates.",
      "They are not automatically psychological strategy classes."
    )
  ), class = "eye_process_rasch_tree")
}

#' Compare Bayesian process models by LOO or Bayes factor
#'
#' @param ... Fitted brms models.
#' @param method `loo` or `bayes_factor`.
#' @return An R object containing bayesian process models by LOO or Bayes factor. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
compare_bayesian_process_models <- function(..., method = c("loo", "bayes_factor")) {
  method <- match.arg(method)
  models <- list(...)
  if (length(models) < 2L) stop("Supply at least two fitted Bayesian models.", call. = FALSE)
  if (!requireNamespace("brms", quietly = TRUE)) stop("Package `brms` is required.", call. = FALSE)
  if (!all(vapply(models, inherits, logical(1), what = "brmsfit")))
    stop("All supplied models must inherit from `brmsfit`.", call. = FALSE)
  if (method == "loo") {
    return(do.call(brms::loo, c(models, list(compare = TRUE))))
  }
  if (length(models) != 2L) stop("Bayes-factor comparison requires exactly two brmsfit models.", call. = FALSE)
  brms::bayes_factor(models[[1L]], models[[2L]])
}

#' Plot mixture irt process diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot mixture irt process diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_mixture_irt_process <- function(x, ...) {
  co <- x$coefficients
  if (is.null(co)) return(.ep08_plot_empty("Mixture IRT", "Coefficients unavailable"))
  graphics::plot.new(); graphics::title(main = "Mixture IRT fitted response classes")
  graphics::text(.5, .55, paste("latent response classes:", x$n_classes))
  graphics::text(.5, .45, "Inspect class-specific item parameters before process interpretation.")
  invisible(x)
}

#' Plot latent process alignment diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot latent process alignment diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_latent_process_alignment <- function(x, ...) {
  s <- x$summary
  vars <- intersect(x$process_features, names(s))
  if (!nrow(s) || !length(vars)) return(.ep08_plot_empty("Latent-class/process alignment"))
  M <- as.matrix(s[vars]); rownames(M) <- s$class
  graphics::matplot(seq_along(vars), t(M), type = "b", lty = seq_len(nrow(M)), pch = seq_len(nrow(M)),
                    axes = FALSE, xlab = "Process feature", ylab = "Class mean",
                    main = "Latent response classes vs process summaries", ...)
  graphics::axis(1, at = seq_along(vars), labels = vars, las = 2)
  graphics::legend("topright", rownames(M), lty = seq_len(nrow(M)), pch = seq_len(nrow(M)), bty = "n")
  invisible(s)
}

#' Plot nonparametric rasch audit diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot nonparametric rasch audit diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param method Method used for the requested diagnostic or summary.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_nonparametric_rasch_audit <- function(x, method = names(x$tests)[1L], ...) {
  if (!method %in% names(x$tests)) stop("Unknown NPtest method.", call. = FALSE)
  z <- x$tests[[method]]
  if (inherits(z, "error")) return(.ep08_plot_empty("Nonparametric Rasch diagnostic", conditionMessage(z)))
  graphics::plot(z, ...)
  invisible(z)
}

#' Plot item reduction sensitivity diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot item reduction sensitivity diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_item_reduction_sensitivity <- function(x, ...) {
  if (!length(x$eliminated_items)) return(.ep08_plot_empty("Item-reduction sensitivity", "No items were eliminated"))
  graphics::plot(seq_along(x$eliminated_items), rep(1, length(x$eliminated_items)),
                 type = "n", yaxt = "n", xlab = "Elimination step", ylab = "",
                 main = "Stepwise item-reduction sensitivity", ...)
  graphics::text(seq_along(x$eliminated_items), 1, labels = x$eliminated_items, srt = 45)
  invisible(x$eliminated_items)
}

#' Plot biometric imputation sensitivity diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot biometric imputation sensitivity diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_biometric_imputation_sensitivity <- function(x, ...) {
  d <- x$missingness
  graphics::barplot(d$missing_prop, names.arg = d$variable, las = 2, ylim = c(0, 1),
                    ylab = "Missing proportion", main = "Biometric-feature missingness before imputation", ...)
  invisible(d)
}

#' Plot process rasch tree diagnostics
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process rasch tree diagnostics.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_process_rasch_tree <- function(x, ...) {
  graphics::plot(x$model, ...)
  invisible(x$model)
}
