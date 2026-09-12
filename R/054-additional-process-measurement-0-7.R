# eyeprocess 0.7.0.9000 ------------------------------------------------------
# Additional process-measurement directions motivated by 2026 work:
# bounded continuous IRT, process-dependent discrimination, channel ablation,
# and multimodal trait wrappers. These are deliberately evidence-gated.

#' Continuous/bounded process channel for multimodal IRT
#' @param family Continuous response family.
#' @param value Variable name.
#' @param lower,upper Bounds where applicable.
#' @param latent Latent dimension.
#' @param options Additional channel metadata.
#' @return An object of class "eye_irt_", "_channel", stored as a named list, with components "type", "family", "role", "link", "variables", "latent", "options". It contains continuous/bounded process channel for multimodal IRT and associated metadata or diagnostics needed to interpret the result.
#' @export
irt_continuous_channel <- function(
    family = c("censored_normal", "beta", "gaussian"),
    value = "process_value", lower = 0, upper = 1,
    latent = "process", options = list()) {
  family <- match.arg(family)
  if (!is.finite(lower) || !is.finite(upper) || lower >= upper)
    stop("lower and upper must be finite with lower < upper.", call. = FALSE)
  .ep07_channel("continuous", family, "process", variables = value,
                latent = latent,
                options = c(options, list(lower = lower, upper = upper)))
}

.ep07_cn_loglik_item <- function(par, y, theta, lower_bound, upper_bound) {
  alpha <- par[1L]
  beta <- par[2L]
  sigma <- exp(par[3L])
  mu <- alpha * theta + beta
  eps <- sqrt(.Machine$double.eps)
  left <- y <= lower_bound + eps
  right <- y >= upper_bound - eps
  mid <- !(left | right)
  ll <- numeric(length(y))
  ll[left] <- stats::pnorm(lower_bound, mean = mu[left], sd = sigma, log.p = TRUE)
  ll[right] <- stats::pnorm(upper_bound, mean = mu[right], sd = sigma,
                            lower.tail = FALSE, log.p = TRUE)
  ll[mid] <- stats::dnorm(y[mid], mean = mu[mid], sd = sigma, log = TRUE)
  -sum(ll[is.finite(ll)])
}

#' Conditional censored-normal calibration for bounded process measurements
#'
#' Fits the 2026 censored-normal response form item-by-item conditional on a
#' supplied latent score. This is a useful calibration/diagnostic engine for
#' bounded continuous process variables such as AOI proportions. It is NOT the
#' paper's full marginal EM estimator and should therefore remain experimental.
#'
#' @param response_matrix Person x item bounded continuous matrix.
#' @param theta Supplied person latent scores on the calibration scale.
#' @param lower,upper Observable bounds.
#' @param control `optim()` control list.
#' @return An object of class "eye_censored_normal_process_irt", stored as a named list, with components "coefficients", "fits", "theta", "lower", "upper", "engine", "status", "citation", "caveat". It contains conditional censored-normal calibration for bounded process measurements and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_censored_normal_process_irt <- function(
    response_matrix, theta, lower = 0, upper = 1,
    control = list(maxit = 1000)) {
  X <- as.matrix(response_matrix)
  storage.mode(X) <- "double"
  theta <- as.numeric(theta)
  if (nrow(X) != length(theta)) stop("length(theta) must equal nrow(response_matrix).", call. = FALSE)
  if (lower >= upper) stop("lower must be < upper.", call. = FALSE)
  if (any(X < lower | X > upper, na.rm = TRUE))
    stop("All observed values must lie within [lower, upper].", call. = FALSE)
  items <- colnames(X) %||% paste0("item", seq_len(ncol(X)))
  fits <- vector("list", ncol(X)); rows <- vector("list", ncol(X))
  for (j in seq_len(ncol(X))) {
    y <- X[, j]
    ok <- is.finite(y) & is.finite(theta)
    yy <- y[ok]; tt <- theta[ok]
    if (length(yy) < 10L || length(unique(yy)) < 2L) {
      fits[[j]] <- NULL
      rows[[j]] <- data.frame(item = items[j], discrimination = NA_real_,
                              intercept = NA_real_, sigma = NA_real_,
                              n = length(yy), convergence = 99L)
      next
    }
    init <- stats::coef(stats::lm(yy ~ tt))
    alpha0 <- unname(init[2L]); beta0 <- unname(init[1L])
    if (!is.finite(alpha0)) alpha0 <- 1
    if (!is.finite(beta0)) beta0 <- mean(yy, na.rm = TRUE)
    sigma0 <- stats::sd(yy, na.rm = TRUE)
    if (!is.finite(sigma0) || sigma0 <= 0) sigma0 <- 0.1 * (upper - lower)
    start <- c(alpha = alpha0, beta = beta0, log_sigma = log(max(sigma0, 1e-3)))
    fit <- stats::optim(start, .ep07_cn_loglik_item, y = yy, theta = tt,
                        lower_bound = lower, upper_bound = upper, method = "BFGS",
                        hessian = TRUE, control = control)
    fits[[j]] <- fit
    rows[[j]] <- data.frame(item = items[j], discrimination = fit$par[1L],
                            intercept = fit$par[2L], sigma = exp(fit$par[3L]),
                            n = length(yy), convergence = fit$convergence,
                            logLik = -fit$value, stringsAsFactors = FALSE)
  }
  structure(list(
    coefficients = do.call(rbind, rows), fits = fits, theta = theta,
    lower = lower, upper = upper,
    engine = "conditional_censored_normal_mle",
    status = "experimental",
    citation = "10.1007/s41237-026-00292-x",
    caveat = paste(
      "Conditional item calibration given supplied theta; not the full marginal",
      "EM estimator in Minamimoto, Wakai & Okada (2026). Validate recovery before use."
    )
  ), class = "eye_censored_normal_process_irt")
}

#' Predict expected bounded response from a censored-normal process IRT fit
#' @param object A fitted eyeprocess model or audit object.
#' @param theta Latent-trait values.
#' @param items Items to include.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return A vector or matrix containing expected bounded response from a censored-normal process IRT fit, with shape determined by the supplied analysis units.
#' @export
predict.eye_censored_normal_process_irt <- function(object, theta = object$theta,
                                                    items = NULL, ...) {
  co <- object$coefficients
  if (!is.null(items)) co <- co[co$item %in% items, , drop = FALSE]
  theta <- as.numeric(theta)
  out <- sapply(seq_len(nrow(co)), function(j) {
    mu <- co$discrimination[j] * theta + co$intercept[j]
    sigma <- co$sigma[j]
    a <- (object$lower - mu) / sigma
    b <- (object$upper - mu) / sigma
    # E[min(U, max(L, Y*))] for Y* ~ Normal(mu, sigma).
    object$lower * stats::pnorm(a) +
      mu * (stats::pnorm(b) - stats::pnorm(a)) +
      sigma * (stats::dnorm(a) - stats::dnorm(b)) +
      object$upper * (1 - stats::pnorm(b))
  })
  if (is.null(dim(out))) out <- matrix(out, ncol = 1L)
  colnames(out) <- co$item
  out
}

#' Audit process-dependent item discrimination
#'
#' Residualises a process variable against person/item baselines, then tests
#' whether the theta-response slope changes with that residual process value.
#' This is a transparent diagnostic inspired by 2026 evidence on conditional
#' response-time/discrimination dependencies, not an exact reproduction of the
#' published meta-analytic model.
#'
#' @param data Long-format response data.
#' @param response Binary response column.
#' @param theta Person latent-score column.
#' @param process RT/gaze/process measure.
#' @param person,item Identifier columns.
#' @param nonlinear If TRUE and mgcv is installed, additionally estimate a
#'   smooth theta-by-process diagnostic surface.
#' @return An object of class "eye_process_dependent_discrimination", stored as a named list, with components "process_model", "response_model", "interaction", "smooth_model", "residual_process", "status", "caveat". It contains process-dependent item discrimination and associated metadata or diagnostics needed to interpret the result.
#' @export
process_dependent_discrimination_audit <- function(
    data, response, theta, process, person, item, nonlinear = TRUE) {
  d <- as.data.frame(data)
  .ep07_v_require(d, c(response, theta, process, person, item), "data")
  y <- d[[response]]
  if (!all(stats::na.omit(y) %in% c(0, 1))) stop("response must be binary 0/1.", call. = FALSE)
  z <- log(pmax(as.numeric(d[[process]]), .Machine$double.eps))
  dd <- data.frame(y = as.numeric(y), theta = as.numeric(d[[theta]]),
                   process = z, person = factor(d[[person]]), item = factor(d[[item]]))
  dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("Package `lme4` is required for process_dependent_discrimination_audit().", call. = FALSE)

  process_model <- lme4::lmer(process ~ 1 + (1 | person) + (1 | item), data = dd,
                              REML = TRUE)
  dd$process_residual <- stats::residuals(process_model)
  response_model <- lme4::glmer(
    y ~ theta * process_residual + (1 + theta | item) + (1 | person),
    data = dd, family = stats::binomial(),
    control = lme4::glmerControl(optimizer = "bobyqa")
  )
  co <- summary(response_model)$coefficients
  interaction <- co["theta:process_residual", , drop = FALSE]
  smooth <- NULL
  if (isTRUE(nonlinear) && requireNamespace("mgcv", quietly = TRUE)) {
    smooth <- tryCatch(
      mgcv::gam(y ~ s(theta, process_residual) + s(item, bs = "re") + s(person, bs = "re"),
                family = stats::binomial(), data = dd, method = "REML"),
      error = function(e) e)
  }
  structure(list(
    process_model = process_model,
    response_model = response_model,
    interaction = interaction,
    smooth_model = smooth,
    residual_process = dd[, c("person", "item", "theta", "process_residual", "y")],
    status = "diagnostic",
    caveat = paste(
      "The interaction indicates process-dependent effective discrimination in",
      "this diagnostic parameterization; it does not by itself identify a causal mechanism."
    )
  ), class = "eye_process_dependent_discrimination")
}

#' Plot process-dependent discrimination
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process-dependent discrimination.
#' @export
plot.eye_process_dependent_discrimination <- function(x, ...) {
  d <- x$residual_process
  q <- stats::quantile(d$process_residual, c(.1, .5, .9), na.rm = TRUE)
  grid <- expand.grid(theta = seq(min(d$theta), max(d$theta), length.out = 101),
                      process_residual = as.numeric(q))
  b <- lme4::fixef(x$response_model)
  lp <- b[1] + b["theta"] * grid$theta + b["process_residual"] * grid$process_residual +
    b["theta:process_residual"] * grid$theta * grid$process_residual
  grid$probability <- stats::plogis(lp)
  grid$process_band <- factor(grid$process_residual, levels = as.numeric(q),
                              labels = c("low", "median", "high"))
  mat <- reshape(grid[, c("theta", "process_band", "probability")],
                 idvar = "theta", timevar = "process_band", direction = "wide")
  yn <- grep("^probability", names(mat), value = TRUE)
  graphics::matplot(mat$theta, as.matrix(mat[yn]), type = "l", lty = seq_along(yn),
                    xlab = "Theta", ylab = "Response probability",
                    main = "Process-dependent effective discrimination", ...)
  graphics::legend("topleft", legend = sub("probability\\.", "", yn),
                   lty = seq_along(yn), bty = "n")
  invisible(grid)
}

#' Ablate process channels under a common out-of-sample evaluator
#'
#' @param data Input data.
#' @param channels Named list whose elements are character vectors of columns.
#' @param evaluator Function `(data, active_columns, channel_name)` returning a
#'   scalar out-of-sample score. The evaluator owns all fitting/splitting logic.
#' @param baseline Character vector of always-active columns.
#' @param higher_is_better Direction of the score.
#' @return An object of class "eye_process_channel_ablation", "data.frame", stored as a data frame, containing ablate process channels under a common out-of-sample evaluator and associated metadata needed to interpret the result.
#' @export
process_channel_ablation <- function(data, channels, evaluator, baseline = character(),
                                     higher_is_better = TRUE) {
  if (!is.list(channels) || is.null(names(channels)))
    stop("channels must be a named list of column vectors.", call. = FALSE)
  if (!is.function(evaluator)) stop("evaluator must be a function.", call. = FALSE)
  full <- unique(c(baseline, unlist(channels, use.names = FALSE)))
  .ep07_v_require(as.data.frame(data), full, "data")
  full_score <- as.numeric(evaluator(data, full, "full"))[1L]
  rows <- lapply(names(channels), function(nm) {
    active <- setdiff(full, channels[[nm]])
    score <- as.numeric(evaluator(data, active, paste0("minus_", nm)))[1L]
    loss <- if (higher_is_better) full_score - score else score - full_score
    data.frame(channel = nm, full_score = full_score, ablated_score = score,
               information_loss = loss, columns_removed = paste(channels[[nm]], collapse = ", "),
               stringsAsFactors = FALSE)
  })
  structure(do.call(rbind, rows), class = c("eye_process_channel_ablation", "data.frame"))
}

#' Plot process-channel ablation
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process-channel ablation.
#' @export
plot.eye_process_channel_ablation <- function(x, ...) {
  d <- as.data.frame(x)
  graphics::barplot(d$information_loss, names.arg = d$channel, las = 2,
                    ylab = "Out-of-sample information loss",
                    main = "Process-channel ablation", ...)
  graphics::abline(h = 0, lty = 3)
  invisible(d)
}

#' Multimodal trait-model convenience wrapper
#'
#' Extends the gaze/RT architecture to noncognitive or other latent traits by
#' allowing caller-defined semantic labels. The statistical engine is delegated
#' to `fit_joint_gaze_rt_irt()`; interpretation remains the researcher's job.
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param rt Response-time variable or column name.
#' @param gaze Gaze/process variable or column name.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param trait_label Value supplied to `trait_label`; see Details for its model-specific role.
#' @param process_label Value supplied to `process_label`; see Details for its model-specific role.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return An object of class "eye_joint_gaze_rt_irt", stored as a named list, with components "engine", "response_model", "rt_model", "gaze_model", "person_scores", "item_scores", "person_covariance", "item_covariance", "data_n", "gaze_family", "columns", "status", and additional components. It contains multimodal trait-model convenience wrapper and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_multimodal_trait_irt <- function(data, response, rt, gaze, person, item,
                                     trait_label = "trait",
                                     process_label = "process",
                                     ...) {
  fit <- fit_joint_gaze_rt_irt(data = data, response = response, rt = rt,
                               gaze = gaze, person = person, item = item, ...)
  fit$trait_label <- trait_label
  fit$process_label <- process_label
  fit$status <- "experimental"
  fit$citation_additional <- "10.1177/10944281261457337"
  fit$caveat_multimodal_trait <- paste(
    "Using process channels for noncognitive/personality traits requires construct-",
    "specific validation and measurement-invariance evidence; improved precision",
    "alone does not establish construct validity."
  )
  class(fit) <- unique(c("eye_multimodal_trait_irt", class(fit)))
  fit
}

#' Generalizability-style variance decomposition for a process measure
#'
#' Useful before many-facet IRT: quantifies how much variance comes from person,
#' item, device, session, algorithm, and residual sources.
#' @param data Input data frame or compatible tabular object.
#' @param outcome Outcome variable.
#' @param facets Facet variables included in the analysis.
#' @param REML Whether restricted maximum likelihood is used.
#' @return An object of class "eye_process_g_study", stored as a named list, with components "model", "variance_components", "facets", "outcome". It contains generalizability-style variance decomposition for a process measure and associated metadata or diagnostics needed to interpret the result.
#' @export
generalizability_process_study <- function(data, outcome, facets,
                                           REML = TRUE) {
  d <- as.data.frame(data)
  .ep07_v_require(d, c(outcome, facets), "data")
  if (!requireNamespace("lme4", quietly = TRUE))
    stop("Package `lme4` is required.", call. = FALSE)
  facets <- unique(as.character(facets))
  for (f in facets) d[[f]] <- factor(d[[f]])
  rhs <- paste(sprintf("(1 | %s)", facets), collapse = " + ")
  form <- stats::as.formula(paste(outcome, "~ 1 +", rhs))
  fit <- lme4::lmer(form, data = d, REML = REML)
  vc <- as.data.frame(lme4::VarCorr(fit))
  out <- data.frame(facet = vc$grp, variance = vc$vcov, sd = vc$sdcor,
                    stringsAsFactors = FALSE)
  out$proportion <- out$variance / sum(out$variance)
  structure(list(model = fit, variance_components = out, facets = facets,
                 outcome = outcome), class = "eye_process_g_study")
}

#' Plot process-measure variance components
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot process-measure variance components.
#' @export
plot.eye_process_g_study <- function(x, ...) {
  d <- x$variance_components
  graphics::barplot(d$proportion, names.arg = d$facet, las = 2,
                    ylab = "Variance proportion",
                    main = "Process-measure generalizability decomposition", ...)
  invisible(d)
}

#' Cross-device process-scale equating audit
#'
#' Fits a simple affine linking map on anchor observations and reports residual
#' bias/RMSE by device. It complements, rather than replaces, IRT anchor linking.
#' @param data Input data frame or compatible tabular object.
#' @param value Process-value column or values.
#' @param reference_value Value supplied to `reference_value`; see Details for its model-specific role.
#' @param device Device identifier or device facet.
#' @param anchor Anchor or reference group used for linking.
#' @return An object of class "eye_cross_device_equating_audit", "data.frame", stored as a data frame, containing cross-device process-scale equating audit and associated metadata needed to interpret the result.
#' @export
cross_device_process_equating_audit <- function(data, value, reference_value,
                                                device, anchor = NULL) {
  d <- as.data.frame(data)
  .ep07_v_require(d, c(value, reference_value, device), "data")
  if (!is.null(anchor)) .ep07_v_require(d, anchor, "data")
  sp <- split(d, d[[device]])
  rows <- lapply(names(sp), function(dev) {
    z <- sp[[dev]]
    if (!is.null(anchor)) z <- z[!is.na(z[[anchor]]), , drop = FALSE]
    x <- as.numeric(z[[value]]); y <- as.numeric(z[[reference_value]])
    ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
    if (length(x) < 3L) return(data.frame(device = dev, n = length(x), A = NA, B = NA,
                                          bias = NA, rmse = NA))
    fit <- stats::lm(y ~ x)
    pr <- stats::predict(fit)
    data.frame(device = dev, n = length(x), A = unname(stats::coef(fit)[2]),
               B = unname(stats::coef(fit)[1]), bias = mean(pr - y),
               rmse = sqrt(mean((pr - y)^2)), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  structure(out, class = c("eye_cross_device_equating_audit", "data.frame"))
}
