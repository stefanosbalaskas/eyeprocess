# eyeprocess 0.7.0.9000 ------------------------------------------------------
# Completion layer for the explicit public API proposed in the 0.7 roadmap.
#
# These functions intentionally reuse the substantive implementations in
# R/048--R/054.  Aliases are retained where the roadmap used a clearer public
# name, while genuinely new helpers add missing semantic/validation contracts.

#' Plot option-level distractor information
#'
#' @param object An `eye_nominal_gaze_irt` object or a data frame returned by
#'   `distractor_process_map()`.
#' @param ... Graphical arguments passed to base graphics.
#' @return The plotted distractor map, invisibly.
#' @export
plot_distractor_information <- function(object, ...) {
  z <- if (inherits(object, "eye_nominal_gaze_irt")) distractor_process_map(object) else
    .ep07_as_data_frame(object, "object")
  if (!nrow(z)) {
    graphics::plot.new(); graphics::title(main = "Distractor process information")
    graphics::text(.5, .5, "No distractor coefficients available")
    return(invisible(z))
  }
  if (all(c("gaze_contrast", "choice_contrast") %in% names(z))) {
    graphics::plot(z$gaze_contrast, z$choice_contrast, pch = 19,
                   xlab = "Gaze contrast", ylab = "Choice contrast",
                   main = "Distractor process information", ...)
    graphics::abline(h = 0, v = 0, lty = 3)
    if ("option" %in% names(z))
      graphics::text(z$gaze_contrast, z$choice_contrast, labels = z$option,
                     pos = 3, cex = .75)
  } else if (all(c("response_category", "gaze_channel", "coefficient") %in% names(z))) {
    labs <- paste(z$response_category, z$gaze_channel, sep = " / ")
    graphics::dotchart(z$coefficient, labels = labs,
                       xlab = "Coefficient", main = "Option-level gaze coefficients", ...)
    graphics::abline(v = 0, lty = 3)
  } else {
    num <- names(z)[vapply(z, is.numeric, logical(1))]
    if (!length(num)) stop("No numeric distractor-information field is available to plot.", call. = FALSE)
    graphics::dotchart(z[[num[1L]]], labels = seq_len(nrow(z)),
                       xlab = num[1L], main = "Distractor process information", ...)
  }
  invisible(z)
}

#' Fit a gaze-informed missingness IRT diagnostic
#'
#' Fits a transparent two-part reference model: (1) whether an item response is
#' missing and (2) the observed response, both conditional on a supplied latent
#' trait (or a clearly labelled person-score proxy), item, and visual exposure.
#' This is a diagnostic bridge to joint MNAR/process IRT, not a substitute for a
#' fully joint latent missingness model.
#'
#' @param data Long person-item data.
#' @param response Response column; missing values identify omissions.
#' @param person,item Person and item identifiers.
#' @param gaze_exposure Non-negative visual-exposure measure.
#' @param theta Optional latent-trait column. If `NULL`, a smoothed person
#'   proportion-correct logit is used as an explicit proxy.
#' @param reached Optional reached/not-reached indicator.
#' @return An `eye_gaze_informed_missingness_irt` object.
#' @export
fit_gaze_informed_missingness_irt <- function(
    data, response = "response", person = "participant_id", item = "item_id",
    gaze_exposure = "gaze_exposure", theta = NULL, reached = NULL) {
  cols <- unique(c(response, person, item, gaze_exposure, theta, reached))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  d <- .ep07_model_frame(data, cols)
  if (any(d[[gaze_exposure]] < 0, na.rm = TRUE))
    stop("`gaze_exposure` must be non-negative.", call. = FALSE)
  d$.missing <- as.integer(is.na(d[[response]]))
  d$.log_exposure <- log1p(as.numeric(d[[gaze_exposure]]))
  d$.item <- factor(d[[item]])
  d$.person <- factor(d[[person]])

  theta_source <- "supplied"
  if (is.null(theta)) {
    theta_source <- "smoothed-person-score-proxy"
    obs <- !is.na(d[[response]])
    y <- suppressWarnings(as.numeric(d[[response]]))
    ok_binary <- all(stats::na.omit(y) %in% c(0, 1))
    if (!ok_binary)
      stop("Without `theta`, the response must be binary so a transparent score proxy can be formed.", call. = FALSE)
    sums <- tapply(y[obs], d$.person[obs], sum, na.rm = TRUE)
    ns <- tapply(y[obs], d$.person[obs], length)
    p <- (sums + 0.5) / (ns + 1)
    theta_map <- stats::qlogis(pmin(pmax(p, 1e-5), 1 - 1e-5))
    d$.theta <- unname(theta_map[as.character(d$.person)])
  } else {
    d$.theta <- as.numeric(d[[theta]])
  }

  missing_formula <- stats::as.formula(".missing ~ .theta + .log_exposure + .item")
  missing_model <- stats::glm(missing_formula, data = d, family = stats::binomial())

  observed <- d[!is.na(d[[response]]), , drop = FALSE]
  response_model <- NULL
  if (nrow(observed)) {
    y <- suppressWarnings(as.numeric(observed[[response]]))
    observed$.response_numeric <- y
    if (all(stats::na.omit(y) %in% c(0, 1))) {
      response_model <- stats::glm(
        .response_numeric ~ .theta + .log_exposure + .item,
        data = observed, family = stats::binomial())
    } else {
      response_model <- stats::lm(
        .response_numeric ~ .theta + .log_exposure + .item,
        data = observed)
    }
  }

  reached_summary <- NULL
  if (!is.null(reached)) {
    r <- as.logical(d[[reached]])
    reached_summary <- data.frame(
      reached = c(FALSE, TRUE),
      n = c(sum(!r, na.rm = TRUE), sum(r, na.rm = TRUE)),
      missing_rate = c(mean(d$.missing[!r], na.rm = TRUE),
                       mean(d$.missing[r], na.rm = TRUE)))
  }

  structure(list(
    missingness_model = missing_model,
    response_model = response_model,
    theta_source = theta_source,
    reached_summary = reached_summary,
    data = d,
    status = "reference-diagnostic",
    note = paste(
      "Two-part conditional diagnostic; it does not establish MAR/MNAR status",
      "and is not a full joint latent missingness IRT estimator.")),
    class = "eye_gaze_informed_missingness_irt")
}

.ep07_named_facet_effects <- function(object, facet, channel = c("response", "process")) {
  channel <- match.arg(channel)
  if (!inherits(object, "eye_manyfacet_process_irt"))
    stop("`object` must come from fit_manyfacet_process_irt().", call. = FALSE)
  if (!facet %in% names(object$facets))
    stop(sprintf("Facet `%s` was not included in the fitted model.", facet), call. = FALSE)
  ef <- facet_effects(object, channel = channel)
  col <- unname(object$facets[[facet]])
  re <- ef$random_effects[[col]]
  vc <- ef$variance_components
  vc <- vc[vc$grp == col, , drop = FALSE]
  structure(list(facet = facet, column = col, channel = channel,
                 random_effects = re, variance_component = vc),
            class = "eye_process_facet_effects")
}

#' Extract device facet effects
#' @param object A fitted eyeprocess model or audit object.
#' @param channel Measurement channel to inspect.
#' @export
device_facet_effects <- function(object, channel = c("response", "process")) {
  .ep07_named_facet_effects(object, "device", channel)
}

#' Extract session facet effects
#' @param object A fitted eyeprocess model or audit object.
#' @param channel Measurement channel to inspect.
#' @export
session_facet_effects <- function(object, channel = c("response", "process")) {
  .ep07_named_facet_effects(object, "session", channel)
}

#' Extract algorithm facet effects
#' @param object A fitted eyeprocess model or audit object.
#' @param channel Measurement channel to inspect.
#' @export
algorithm_facet_effects <- function(object, channel = c("response", "process")) {
  .ep07_named_facet_effects(object, "algorithm", channel)
}

#' Detect a response-process change point
#'
#' Public roadmap alias for `detect_irt_changepoints()`.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
detect_process_changepoint <- function(...) detect_irt_changepoints(...)

#' Plot detected process change points
#'
#' @param object An `eye_irt_changepoints`, `eye_changepoint_rt_irt`, or
#'   `eye_changepoint_multimodal_irt` object.
#' @param ... Graphical arguments.
#' @export
plot_process_changepoint <- function(object, ...) {
  cp <- if (inherits(object, "eye_irt_changepoints")) object else object$changepoints
  if (!inherits(cp, "eye_irt_changepoints"))
    stop("Supply a process/change-point object returned by eyeprocess.", call. = FALSE)
  z <- cp$results
  if (!nrow(z)) {
    graphics::plot.new(); graphics::title(main = "Process change points")
    return(invisible(z))
  }
  y <- z$changepoint_order
  graphics::plot(seq_len(nrow(z)), y, pch = ifelse(z$detected, 19, 1),
                 xlab = "Participant", ylab = "Estimated change-point order",
                 main = "Detected process change points", ...)
  invisible(z)
}

#' Plot person/item latent-space coordinates
#'
#' @param object Fitted `eye_latent_space_irt` object.
#' @param dimensions Two coordinate dimensions to display.
#' @param labels Whether to add entity labels.
#' @param ... Graphical arguments.
#' @export
plot_person_item_space <- function(object, dimensions = c(1L, 2L), labels = FALSE, ...) {
  if (!inherits(object, "eye_latent_space_irt"))
    stop("`object` must come from fit_latent_space_irt().", call. = FALSE)
  dimensions <- as.integer(dimensions)
  if (length(dimensions) != 2L || any(dimensions < 1L))
    stop("`dimensions` must contain two positive indices.", call. = FALSE)
  p <- as.matrix(object$person_coordinates); i <- as.matrix(object$item_coordinates)
  if (max(dimensions) > ncol(p) || max(dimensions) > ncol(i))
    stop("Requested latent-space dimension is unavailable.", call. = FALSE)
  xr <- range(c(p[, dimensions[1L]], i[, dimensions[1L]]), finite = TRUE)
  yr <- range(c(p[, dimensions[2L]], i[, dimensions[2L]]), finite = TRUE)
  graphics::plot(p[, dimensions[1L]], p[, dimensions[2L]], pch = 1,
                 xlim = xr, ylim = yr,
                 xlab = paste0("Latent-space dimension ", dimensions[1L]),
                 ylab = paste0("Latent-space dimension ", dimensions[2L]),
                 main = "Person-item latent space", ...)
  graphics::points(i[, dimensions[1L]], i[, dimensions[2L]], pch = 4)
  if (isTRUE(labels)) {
    graphics::text(p[, dimensions[1L]], p[, dimensions[2L]],
                   labels = rownames(p) %||% seq_len(nrow(p)), pos = 3, cex = .6)
    graphics::text(i[, dimensions[1L]], i[, dimensions[2L]],
                   labels = rownames(i) %||% paste0("I", seq_len(nrow(i))), pos = 3, cex = .7)
  }
  invisible(list(person = p, item = i, dimensions = dimensions))
}

#' Explain local person-item latent-space interactions
#'
#' Returns the closest person-item pairs in the fitted residual latent space.
#' Closeness is descriptive residual structure, not a causal explanation.
#'
#' @param object Fitted `eye_latent_space_irt` object.
#' @param person Optional person row/index/name to restrict.
#' @param item Optional item row/index/name to restrict.
#' @param top Number of closest pairs to return.
#' @export
explain_latent_interaction <- function(object, person = NULL, item = NULL, top = 10L) {
  if (!inherits(object, "eye_latent_space_irt"))
    stop("`object` must come from fit_latent_space_irt().", call. = FALSE)
  p <- as.matrix(object$person_coordinates); it <- as.matrix(object$item_coordinates)
  if (ncol(p) != ncol(it)) stop("Person/item coordinates have incompatible dimensions.", call. = FALSE)
  pnames <- rownames(p) %||% as.character(seq_len(nrow(p)))
  inames <- rownames(it) %||% as.character(seq_len(nrow(it)))
  pick <- function(x, nm, n) {
    if (is.null(x)) return(seq_len(n))
    if (is.numeric(x)) return(intersect(as.integer(x), seq_len(n)))
    match(x, nm, nomatch = 0L)[match(x, nm, nomatch = 0L) > 0L]
  }
  pi <- pick(person, pnames, nrow(p)); ii <- pick(item, inames, nrow(it))
  if (!length(pi) || !length(ii)) stop("No matching person/item coordinates.", call. = FALSE)
  rows <- lapply(pi, function(a) {
    delta <- sweep(it[ii, , drop = FALSE], 2L, p[a, ], "-")
    data.frame(person = pnames[a], item = inames[ii],
               distance = sqrt(rowSums(delta^2)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$distance), , drop = FALSE]
  utils::head(out, max(1L, as.integer(top)))
}

#' Plot uncertainty for flexible item response functions
#'
#' For the bundled spline-reference engine, standard errors are derived on the
#' logit scale from the fitted GLM and transformed to response probabilities.
#' Exact GPIRT engines should supply their own posterior uncertainty summaries.
#'
#' @param object An `eye_gpirt` object.
#' @param item Item name or index.
#' @param theta_grid Trait grid.
#' @param level Pointwise confidence level for the spline-reference diagnostic.
#' @param ... Graphical arguments.
#' @export
plot_irf_uncertainty <- function(object, item = 1L,
                                 theta_grid = seq(-4, 4, length.out = 101),
                                 level = 0.95, ...) {
  if (!inherits(object, "eye_gpirt")) stop("`object` must come from fit_gpirt().", call. = FALSE)
  if (!identical(object$engine, "spline_reference"))
    stop("The generic external GPIRT gate does not expose a common posterior prediction contract. Plot uncertainty with the external engine's native method.", call. = FALSE)
  idx <- if (is.character(item)) match(item, names(object$models)) else as.integer(item)[1L]
  if (!is.finite(idx) || idx < 1L || idx > length(object$models)) stop("Unknown item.", call. = FALSE)
  pr <- stats::predict(object$models[[idx]], newdata = data.frame(theta = theta_grid),
                       type = "link", se.fit = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  tab <- data.frame(theta = theta_grid,
                    estimate = stats::plogis(pr$fit),
                    lower = stats::plogis(pr$fit - z * pr$se.fit),
                    upper = stats::plogis(pr$fit + z * pr$se.fit))
  graphics::plot(tab$theta, tab$estimate, type = "l", ylim = c(0, 1),
                 xlab = expression(theta), ylab = "Response probability",
                 main = paste("Flexible IRF:", names(object$models)[idx]), ...)
  graphics::lines(tab$theta, tab$lower, lty = 2)
  graphics::lines(tab$theta, tab$upper, lty = 2)
  invisible(tab)
}

#' Audit the empirical latent-trait distribution
#'
#' @param theta Numeric latent-trait draws/estimates.
#' @param tail_z Absolute standardized threshold used for tail-rate diagnostics.
#' @export
audit_latent_distribution <- function(theta, tail_z = 3) {
  x <- as.numeric(theta); x <- x[is.finite(x)]
  if (length(x) < 8L) stop("At least eight finite latent-trait values are required.", call. = FALSE)
  mu <- mean(x); s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) stop("Latent-trait variance must be positive.", call. = FALSE)
  z <- (x - mu) / s
  skew <- mean(z^3)
  exkurt <- mean(z^4) - 3
  bc <- (skew^2 + 1) / pmax(exkurt + 3, .Machine$double.eps)
  theo <- stats::qnorm(stats::ppoints(length(x)))
  qq_cor <- suppressWarnings(stats::cor(sort(z), theo))
  out <- data.frame(
    n = length(x), mean = mu, sd = s, skewness = skew,
    excess_kurtosis = exkurt, tail_rate = mean(abs(z) > tail_z),
    tail_z = tail_z, bimodality_coefficient = bc,
    normal_qq_correlation = qq_cor, stringsAsFactors = FALSE)
  class(out) <- c("eye_latent_distribution_audit", "data.frame")
  out
}

.ep07_loglik_normal <- function(x) {
  mu <- mean(x); s <- sqrt(mean((x - mu)^2)); s <- pmax(s, sqrt(.Machine$double.eps))
  sum(stats::dnorm(x, mu, s, log = TRUE))
}

.ep07_fit_student_t <- function(x) {
  # Location/scale t likelihood; bounded df prevents the Gaussian limit from
  # creating numerical instability while still exposing heavy-tail preference.
  init <- c(mean(x), log(stats::sd(x)), log(8 - 2))
  fn <- function(par) {
    mu <- par[1L]; sig <- exp(par[2L]); df <- 2 + exp(par[3L])
    -sum(stats::dt((x - mu) / sig, df = df, log = TRUE) - log(sig))
  }
  fit <- stats::optim(init, fn, method = "BFGS")
  list(logLik = -fit$value, parameters = c(location = fit$par[1L],
       scale = exp(fit$par[2L]), df = 2 + exp(fit$par[3L])), converged = fit$convergence == 0L)
}

.ep07_fit_two_normal <- function(x, max_iter = 250L, tol = 1e-8) {
  q <- stats::quantile(x, c(.3, .7), names = FALSE)
  mu <- q; sig <- rep(stats::sd(x), 2L); w <- c(.5, .5)
  ll_old <- -Inf
  for (iter in seq_len(max_iter)) {
    dens <- cbind(w[1L] * stats::dnorm(x, mu[1L], pmax(sig[1L], 1e-8)),
                  w[2L] * stats::dnorm(x, mu[2L], pmax(sig[2L], 1e-8)))
    den <- rowSums(dens); den <- pmax(den, .Machine$double.xmin)
    r <- dens / den
    nk <- colSums(r); w <- nk / length(x)
    mu <- colSums(r * x) / pmax(nk, 1e-8)
    sig <- sqrt(colSums(r * sweep(matrix(x, nrow = length(x), ncol = 2L), 2L, mu, "-")^2) / pmax(nk, 1e-8))
    sig <- pmax(sig, 1e-6)
    ll <- sum(log(den))
    if (is.finite(ll_old) && abs(ll - ll_old) < tol) break
    ll_old <- ll
  }
  list(logLik = ll, parameters = c(weight1 = w[1L], mean1 = mu[1L], sd1 = sig[1L],
                                    weight2 = w[2L], mean2 = mu[2L], sd2 = sig[2L]),
       converged = is.finite(ll))
}

#' Compare simple latent-distribution reference models
#'
#' Compares Gaussian, location/scale Student-t, and two-normal-mixture
#' reference densities by AIC/BIC. This is a stress-test diagnostic; it does not
#' change the latent distribution inside an already fitted IRT model.
#'
#' @param theta Numeric latent-trait draws/estimates.
#' @export
compare_latent_distribution_models <- function(theta) {
  x <- as.numeric(theta); x <- x[is.finite(x)]
  if (length(x) < 20L) stop("At least 20 finite values are recommended for distribution comparison.", call. = FALSE)
  n <- length(x)
  ll_n <- .ep07_loglik_normal(x)
  ft <- .ep07_fit_student_t(x)
  fm <- .ep07_fit_two_normal(x)
  tab <- data.frame(
    model = c("normal", "student_t", "two_normal_mixture"),
    logLik = c(ll_n, ft$logLik, fm$logLik),
    k = c(2L, 3L, 5L),
    converged = c(TRUE, ft$converged, fm$converged),
    stringsAsFactors = FALSE)
  tab$AIC <- -2 * tab$logLik + 2 * tab$k
  tab$BIC <- -2 * tab$logLik + log(n) * tab$k
  tab$delta_AIC <- tab$AIC - min(tab$AIC, na.rm = TRUE)
  tab$delta_BIC <- tab$BIC - min(tab$BIC, na.rm = TRUE)
  tab <- tab[order(tab$BIC), , drop = FALSE]
  rownames(tab) <- NULL
  structure(list(comparison = tab, audit = audit_latent_distribution(x),
                 student_t = ft$parameters, mixture = fm$parameters,
                 status = "distribution-stress-test"),
            class = "eye_latent_distribution_comparison")
}

#' Stress-test IRT estimators across latent distributions
#'
#' Public roadmap alias for `stress_test_latent_distribution()`.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
latent_distribution_stress_test <- function(...) stress_test_latent_distribution(...)

#' Fit an event-time IRT reference workflow
#'
#' The built-in Cox reference conditions on an already supplied theta and is
#' therefore an event-time *measurement diagnostic*, not a full continuous-time
#' latent-trait estimator. A validated exact implementation can be supplied via
#' `external_engine`.
#'
#' @param data Long event/item data.
#' @param event_time Time-to-event column.
#' @param event Event indicator (1 event, 0 censored).
#' @param theta Supplied latent-trait column for the reference engine.
#' @param person,item Person and item identifiers.
#' @param engine `cox_reference` or `external`.
#' @param external_engine Function implementing a study-specific event-time IRT.
#' @param ... Additional arguments passed to the external engine.
#' @export
fit_event_time_irt <- function(
    data, event_time = "event_time", event = "event", theta = "theta",
    person = "participant_id", item = "item_id",
    engine = c("cox_reference", "external"), external_engine = NULL, ...) {
  engine <- match.arg(engine)
  if (engine == "external") {
    if (!is.function(external_engine))
      stop("Supply a validated event-time IRT fitter through `external_engine`.", call. = FALSE)
    return(structure(list(model = external_engine(data = data, ...), engine = "external",
                          status = "experimental-gated"), class = "eye_event_time_irt"))
  }
  if (!requireNamespace("survival", quietly = TRUE))
    stop("Install optional package `survival` for the Cox reference engine.", call. = FALSE)
  d <- .ep07_model_frame(data, c(event_time, event, theta, person, item))
  d$.time <- as.numeric(d[[event_time]])
  d$.event <- as.integer(as.logical(d[[event]]))
  d$.theta <- as.numeric(d[[theta]])
  d$.item <- factor(d[[item]])
  d$.person <- factor(d[[person]])
  if (any(d$.time < 0, na.rm = TRUE)) stop("Event times must be non-negative.", call. = FALSE)
  f <- stats::as.formula("survival::Surv(.time, .event) ~ .theta + .item")
  fit <- survival::coxph(f, data = d, model = TRUE, x = TRUE,
                         robust = TRUE, cluster = d$.person)
  structure(list(model = fit, engine = "cox_reference", theta_conditioned = TRUE,
                 status = "experimental-reference",
                 note = "Conditioned-on-theta Cox diagnostic; not a full joint continuous-time IRT estimator."),
            class = "eye_event_time_irt")
}

#' Simulate data from a model or registered model specification
#'
#' @param model Registered model id/specification, simulation function, or an
#'   object exposing a `simulate_fun` function.
#' @param ... Arguments passed to the simulator.
#' @export
simulate_from_model <- function(model, ...) {
  if (is.function(model)) return(model(...))
  if (is.character(model) || inherits(model, "eye_irt_model_spec"))
    return(simulate_irt_model(model, ...))
  if (is.list(model) && is.function(model$simulate_fun)) return(model$simulate_fun(...))
  stop("Supply a registered IRT model/specification or a simulation function.", call. = FALSE)
}

#' Extract canonical parameter truth from simulated data
#'
#' @param simulation Simulation object containing `truth`, `parameters`, or a
#'   `truth` attribute.
#' @return Data frame with `parameter` and `truth`.
#' @export
extract_parameter_truth <- function(simulation) {
  x <- NULL
  if (is.list(simulation) && !is.null(simulation$truth)) x <- simulation$truth
  if (is.null(x) && is.list(simulation) && !is.null(simulation$parameters)) x <- simulation$parameters
  if (is.null(x)) x <- attr(simulation, "truth", exact = TRUE)
  if (is.null(x)) stop("Simulation object does not expose parameter truth.", call. = FALSE)
  if (is.data.frame(x)) {
    if (!all(c("parameter", "truth") %in% names(x)))
      stop("Truth data frame must contain `parameter` and `truth`.", call. = FALSE)
    return(x[, c("parameter", "truth"), drop = FALSE])
  }
  # Preserve names before coercion because as.numeric() drops them.
  src <- if (is.list(simulation) && !is.null(simulation$truth)) simulation$truth else
    if (is.list(simulation) && !is.null(simulation$parameters)) simulation$parameters else
      attr(simulation, "truth", exact = TRUE)
  src_u <- unlist(src, recursive = TRUE, use.names = TRUE)
  vals <- as.numeric(src_u)
  nm <- names(src_u)
  if (is.null(nm) || any(!nzchar(nm))) nm <- paste0("parameter_", seq_along(vals))
  data.frame(parameter = nm, truth = vals, stringsAsFactors = FALSE)
}

#' Fit one model-validation replicate
#'
#' @param replicate Replicate id.
#' @param generator Function `(replicate, scenario)` returning simulated data;
#'   the simulation should expose truth via `extract_parameter_truth()`.
#' @param fitter Function accepting the simulated data (or its `$data` member).
#' @param extractor Function `(fit, simulation)` returning estimates with at
#'   least `parameter` and `estimate`; optional `lower`/`upper` are retained.
#' @param scenario Scenario label/object passed to the generator.
#' @param engine Engine label.
#' @export
fit_validation_replicate <- function(replicate, generator, fitter, extractor,
                                     scenario = "baseline", engine = "unspecified") {
  if (!all(vapply(list(generator, fitter, extractor), is.function, logical(1))))
    stop("`generator`, `fitter`, and `extractor` must be functions.", call. = FALSE)
  tryCatch({
    sim <- generator(replicate = replicate, scenario = scenario)
    truth <- extract_parameter_truth(sim)
    dat <- if (is.list(sim) && !is.null(sim$data)) sim$data else sim
    fit <- fitter(dat)
    est <- .ep07_v_as_df(extractor(fit, sim))
    .ep07_v_require(est, c("parameter", "estimate"), "extractor result")
    out <- merge(truth, est, by = "parameter", all = TRUE, sort = FALSE)
    out$replicate <- replicate
    out$scenario <- if (length(scenario) == 1L) as.character(scenario) else "custom"
    out$engine <- engine
    out$converged <- TRUE
    as_irt_recovery_results(out)
  }, error = function(e) {
    fail <- validation_failure_taxonomy(e)
    data.frame(replicate = replicate, parameter = NA_character_, truth = NA_real_,
               estimate = NA_real_, scenario = if (length(scenario) == 1L) as.character(scenario) else "custom",
               engine = engine, converged = FALSE,
               failure_type = fail$failure_type[1L], failure_message = fail$message[1L],
               stringsAsFactors = FALSE)
  })
}

#' Declare a vendor semantic schema contract
#'
#' @param vendor Vendor/ecosystem label.
#' @param version Optional format/software version.
#' @param required_fields Fields that must survive import.
#' @param optional_fields Fields that may be present.
#' @param aliases Named list mapping canonical fields to accepted vendor names.
#' @param timestamp Named list describing device/system/media time columns.
#' @param coordinate Named list describing x/y columns and coordinate semantics.
#' @param units Named list of expected units for canonical fields.
#' @param eye_streams Expected eye streams (`left`, `right`, `cyclopean`, etc.).
#' @param event_fields Event/annotation fields expected to survive.
#' @export
vendor_schema_contract <- function(
    vendor, version = NA_character_, required_fields = character(),
    optional_fields = character(), aliases = list(), timestamp = list(),
    coordinate = list(), units = list(), eye_streams = character(),
    event_fields = character()) {
  vendor <- .ep07_scalar_chr(vendor, "vendor")
  structure(list(vendor = vendor, version = as.character(version)[1L],
                 required_fields = unique(required_fields),
                 optional_fields = unique(optional_fields), aliases = aliases,
                 timestamp = timestamp, coordinate = coordinate, units = units,
                 eye_streams = unique(eye_streams), event_fields = unique(event_fields),
                 contract_version = "0.7.0"), class = "eye_vendor_schema_contract")
}

#' Validate imported data against a vendor semantic contract
#'
#' @param data Imported/canonical table.
#' @param contract `eye_vendor_schema_contract`.
#' @param metadata Optional named metadata list.
#' @export
validate_vendor_semantics <- function(data, contract, metadata = list()) {
  d <- .ep07_as_data_frame(data, "data")
  if (!inherits(contract, "eye_vendor_schema_contract"))
    stop("`contract` must come from vendor_schema_contract().", call. = FALSE)
  present <- names(d)
  required <- data.frame(field = contract$required_fields,
                         present = contract$required_fields %in% present,
                         required = TRUE, stringsAsFactors = FALSE)
  optional <- data.frame(field = contract$optional_fields,
                         present = contract$optional_fields %in% present,
                         required = FALSE, stringsAsFactors = FALSE)
  fields <- rbind(required, optional)

  alias_rows <- list()
  if (length(contract$aliases)) {
    alias_rows <- lapply(names(contract$aliases), function(canonical) {
      accepted <- unique(c(canonical, as.character(contract$aliases[[canonical]])))
      data.frame(canonical = canonical, matched = any(accepted %in% present),
                 matched_field = paste(intersect(accepted, present), collapse = ", "),
                 stringsAsFactors = FALSE)
    })
  }
  aliases <- if (length(alias_rows)) do.call(rbind, alias_rows) else data.frame()

  time_audit <- NULL
  ts <- contract$timestamp
  if (length(ts)) {
    time_audit <- validate_vendor_timestamp_semantics(
      d, vendor = contract$vendor,
      device_time = ts$device_time %||% ts$native_time %||% NULL,
      system_time = ts$system_time %||% NULL,
      media_time = ts$media_time %||% NULL)
  }

  unit_rows <- lapply(names(contract$units), function(nm) {
    expected <- as.character(contract$units[[nm]])[1L]
    meta_field <- metadata[[nm]]
    observed <- if (is.list(meta_field)) meta_field$Units else NULL
    observed <- observed %||% metadata[[paste0(nm, "_units")]] %||% NA_character_
    data.frame(field = nm, expected_unit = expected, observed_unit = as.character(observed)[1L],
               pass = !is.na(observed) && identical(tolower(as.character(observed)[1L]), tolower(expected)),
               stringsAsFactors = FALSE)
  })
  units <- if (length(unit_rows)) do.call(rbind, unit_rows) else data.frame()

  pass <- all(fields$present[fields$required]) &&
    (!nrow(aliases) || all(aliases$matched)) &&
    (is.null(time_audit) || isTRUE(time_audit$pass)) &&
    (!nrow(units) || all(units$pass))
  structure(list(pass = pass, vendor = contract$vendor, version = contract$version,
                 fields = fields, aliases = aliases, timestamp = time_audit,
                 units = units, contract = contract), class = "eye_vendor_semantic_validation")
}

#' Audit event survival across an interchange round trip
#'
#' @param source_events,roundtrip_events Event tables.
#' @param hed_column Optional HED annotation column to compare structurally.
#' @param ... Arguments forwarded to `event_semantics_audit()`.
#' @export
event_roundtrip_audit <- function(source_events, roundtrip_events,
                                  hed_column = NULL, ...) {
  core <- event_semantics_audit(source_events, roundtrip_events, ...)
  hed <- NULL
  if (!is.null(hed_column) && hed_column %in% names(source_events) && hed_column %in% names(roundtrip_events)) {
    a <- validate_hed_event_semantics(source_events, hed_column = hed_column)
    b <- validate_hed_event_semantics(roundtrip_events, hed_column = hed_column)
    hed <- list(source = a, roundtrip = b,
                valid_fraction_source = mean(a$structurally_valid),
                valid_fraction_roundtrip = mean(b$structurally_valid))
  }
  structure(list(status = core$status, event_semantics = core, hed = hed),
            class = "eye_event_roundtrip_audit")
}

.ep07_roundtrip_extract_samples <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x) && !is.null(x$samples)) {
    return(x$samples)
  }
  stop("Define `extract_samples` for this object class.", call. = FALSE)
}

.ep07_adapter_extract_samples <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x) && !is.null(x$samples)) {
    return(x$samples)
  }
  stop("Define `extract_samples` for this adapter output.", call. = FALSE)
}

#' Execute and audit an Eye-Tracking-BIDS round trip
#'
#' This is a callback harness so it remains stable even if the package's BIDS
#' writer/reader signatures evolve. `exporter` receives the source object plus
#' `export_args`; `importer` receives the exporter result plus `import_args`.
#'
#' @param source Source eyeprocess object/table.
#' @param exporter Function that writes/exports BIDS and returns a locator or
#'   object consumable by `importer`.
#' @param importer Function that reconstructs an eyeprocess object/table.
#' @param export_args,import_args Named argument lists.
#' @param extract_samples Function extracting the canonical sample table from
#'   source and reconstructed objects.
#' @param audit_args Arguments forwarded to `semantic_roundtrip_audit()`.
#' @export
roundtrip_eye_bids <- function(source, exporter, importer,
                               export_args = list(), import_args = list(),
                               extract_samples = .ep07_roundtrip_extract_samples,
                               audit_args = list()) {
  if (!is.function(exporter) || !is.function(importer) || !is.function(extract_samples))
    stop("`exporter`, `importer`, and `extract_samples` must be functions.", call. = FALSE)
  exported <- do.call(exporter, c(list(source), export_args))
  reconstructed <- do.call(importer, c(list(exported), import_args))
  a <- extract_samples(source); b <- extract_samples(reconstructed)
  audit <- do.call(semantic_roundtrip_audit, c(list(source = a, roundtrip = b), audit_args))
  structure(list(exported = exported, reconstructed = reconstructed, audit = audit,
                 status = audit$overall), class = "eye_bids_roundtrip")
}

#' Compare adapter output across software/format versions
#'
#' @param input Shared raw fixture/input.
#' @param baseline_adapter,candidate_adapter Functions that parse `input`.
#' @param baseline_version,candidate_version Version labels.
#' @param extract_samples Function extracting comparable sample tables.
#' @param audit_args Arguments passed to `field_fidelity_report()`.
#' @export
cross_version_adapter_regression <- function(
    input, baseline_adapter, candidate_adapter,
    baseline_version = "baseline", candidate_version = "candidate",
    extract_samples = .ep07_adapter_extract_samples, audit_args = list()) {
  if (!is.function(baseline_adapter) || !is.function(candidate_adapter) || !is.function(extract_samples))
    stop("Adapter and extractor arguments must be functions.", call. = FALSE)
  old <- baseline_adapter(input); new <- candidate_adapter(input)
  a <- extract_samples(old); b <- extract_samples(new)
  fidelity <- do.call(field_fidelity_report, c(list(source = a, roundtrip = b), audit_args))
  status <- if (all(fidelity$fields$status == "LOSSLESS")) "LOSSLESS" else
    if (all(fidelity$fields$status %in% c("LOSSLESS", "UNIT_TRANSFORMED", "COORDINATE_TRANSFORMED", "SEMANTICALLY_EQUIVALENT")))
      "SEMANTICALLY_EQUIVALENT" else "REGRESSION_OR_AMBIGUOUS"
  structure(list(status = status, baseline_version = baseline_version,
                 candidate_version = candidate_version, fidelity = fidelity,
                 baseline = old, candidate = new),
            class = "eye_adapter_regression_audit")
}
