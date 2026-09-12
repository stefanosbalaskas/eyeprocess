# eyeprocess 0.9 Milestone #2: IRT scoring and adaptive-design utilities

.ep09m2_response_loglik <- function(theta, response, items, D = 1) {
  items <- .ep09m2_item_pars(items)
  response <- as.numeric(response)
  if (length(response) != nrow(items) || any(!is.na(response) & !response %in% c(0, 1))) stop("response must contain 0/1/NA and match items.", call. = FALSE)
  keep <- !is.na(response)
  if (!any(keep)) return(0)
  p <- vapply(which(keep), function(j) eyeprocess_irt_4pl_probability(theta, items$a[j], items$b[j], items$c[j], items$d[j], D), numeric(1))
  y <- response[keep]
  sum(y * log(pmax(p, .Machine$double.eps)) + (1 - y) * log(pmax(1 - p, .Machine$double.eps)))
}

#' EAP score for dichotomous IRT item parameters
#' @param response Observed item response or response variable.
#' @param items Item-parameter data frame or item collection.
#' @param theta_grid Grid of latent-trait values used for numerical scoring or integration.
#' @param prior_mean Mean of the normal latent-trait prior.
#' @param prior_sd Standard deviation of the normal latent-trait prior.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_eap_score <- function(response, items, theta_grid = seq(-4, 4, length.out = 81), prior_mean = 0, prior_sd = 1, D = 1) {
  theta_grid <- .ep09m2_theta(theta_grid)
  if (length(theta_grid) < 5L) stop("theta_grid must contain at least five points.", call. = FALSE)
  prior_mean <- as.numeric(prior_mean); prior_sd <- as.numeric(prior_sd)
  if (length(prior_mean) != 1L || !is.finite(prior_mean) || length(prior_sd) != 1L || !is.finite(prior_sd) || prior_sd <= 0) stop("prior_mean must be finite scalar and prior_sd positive finite scalar.", call. = FALSE)
  logw <- vapply(theta_grid, .ep09m2_response_loglik, numeric(1), response = response, items = items, D = D) + stats::dnorm(theta_grid, prior_mean, prior_sd, log = TRUE)
  w <- exp(logw - max(logw)); w <- w / sum(w)
  estimate <- sum(theta_grid * w)
  se <- sqrt(sum((theta_grid - estimate)^2 * w))
  structure(list(estimate = estimate, se = se, theta = theta_grid, posterior = w, method = "EAP"), class = "eye_irt_score")
}

#' MAP score for dichotomous IRT item parameters
#' @param response Observed item response or response variable.
#' @param items Item-parameter data frame or item collection.
#' @param bounds Numerical lower and upper optimization bounds.
#' @param prior_mean Mean of the normal latent-trait prior.
#' @param prior_sd Standard deviation of the normal latent-trait prior.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_map_score <- function(response, items, bounds = c(-6, 6), prior_mean = 0, prior_sd = 1, D = 1) {
  bounds <- as.numeric(bounds); prior_mean <- as.numeric(prior_mean); prior_sd <- as.numeric(prior_sd)
  if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[1] >= bounds[2] || length(prior_mean) != 1L || !is.finite(prior_mean) || length(prior_sd) != 1L || !is.finite(prior_sd) || prior_sd <= 0) stop("invalid bounds or normal prior.", call. = FALSE)
  objective <- function(th) -(.ep09m2_response_loglik(th, response, items, D = D) + stats::dnorm(th, prior_mean, prior_sd, log = TRUE))
  fit <- stats::optimize(objective, interval = bounds)
  structure(list(estimate = fit$minimum, objective = fit$objective, method = "MAP", bounds = bounds), class = "eye_irt_score")
}

#' Bounded ML score for dichotomous IRT item parameters
#' @param response Observed item response or response variable.
#' @param items Item-parameter data frame or item collection.
#' @param bounds Numerical lower and upper optimization bounds.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_mle_score <- function(response, items, bounds = c(-6, 6), D = 1) {
  bounds <- as.numeric(bounds)
  if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[1] >= bounds[2]) stop("bounds must be increasing and finite.", call. = FALSE)
  fit <- stats::optimize(function(th) -.ep09m2_response_loglik(th, response, items, D = D), interval = bounds)
  boundary <- isTRUE(all.equal(fit$minimum, bounds[1], tolerance = 1e-4)) || isTRUE(all.equal(fit$minimum, bounds[2], tolerance = 1e-4))
  structure(list(estimate = fit$minimum, objective = fit$objective, method = "ML", bounds = bounds, boundary = boundary), class = "eye_irt_score")
}

#' Score a response matrix with EAP, MAP, or ML
#' @param responses Response matrix or response data.
#' @param items Item-parameter data frame or item collection.
#' @param method Scoring, linking, or analysis method.
#' @param person_ids Optional person identifiers.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @export
eyeprocess_irt_score_table <- function(responses, items, method = c("EAP", "MAP", "ML"), person_ids = rownames(responses), ...) {
  y <- .ep09m2_binary_matrix(responses); method <- match.arg(method)
  if (is.null(person_ids)) person_ids <- paste0("person_", seq_len(nrow(y)))
  if (length(person_ids) != nrow(y)) stop("person_ids length mismatch.", call. = FALSE)
  fun <- switch(method, EAP = eyeprocess_irt_eap_score, MAP = eyeprocess_irt_map_score, ML = eyeprocess_irt_mle_score)
  rows <- lapply(seq_len(nrow(y)), function(i) {
    z <- fun(y[i, ], items, ...)
    data.frame(person_id = person_ids[i], estimate = z$estimate, se = if (!is.null(z$se)) z$se else NA_real_, method = method, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Draw plausible values from a discrete posterior grid
#' @param score Score object containing posterior or uncertainty information.
#' @param n Number of values, draws, or plausible values to generate.
#' @param seed Random-number seed for reproducible execution.
#' @export
eyeprocess_irt_plausible_values <- function(score, n = 5L, seed = 1L) {
  if (!inherits(score, "eye_irt_score") || is.null(score$posterior) || is.null(score$theta)) stop("score must be an EAP eye_irt_score with posterior grid weights.", call. = FALSE)
  n <- as.integer(n); seed <- as.integer(seed)
  if (length(n) != 1L || is.na(n) || n < 1L || length(seed) != 1L || is.na(seed) || seed < 1L) stop("n and seed must be positive scalar integers.", call. = FALSE)
  .eye_local_seed(seed)
  sample(score$theta, size = n, replace = TRUE, prob = score$posterior)
}

#' Marginal reliability from latent-score variance and conditional error variance
#' @param theta_estimate Estimated latent-trait values.
#' @param se Standard-error values.
#' @export
eyeprocess_irt_marginal_reliability <- function(theta_estimate, se) {
  theta_estimate <- as.numeric(theta_estimate); se <- as.numeric(se)
  keep <- is.finite(theta_estimate) & is.finite(se) & se >= 0
  if (sum(keep) < 2L) return(NA_real_)
  v <- stats::var(theta_estimate[keep])
  if (!is.finite(v) || v <= 0) return(NA_real_)
  max(0, min(1, 1 - mean(se[keep]^2) / v))
}

#' Summarise score uncertainty
#' @param scores Score object or score table.
#' @export
eyeprocess_irt_score_uncertainty <- function(scores) {
  scores <- .ep09m2_as_df(scores, "scores"); .ep09m2_req_cols(scores, c("estimate", "se"), "scores")
  se <- as.numeric(scores$se); est <- as.numeric(scores$estimate)
  structure(list(n = sum(is.finite(est)), mean_se = .ep09m2_finite_mean(se), median_se = .ep09m2_safe_quantile(se, .5),
                 p95_se = .ep09m2_safe_quantile(se, .95), marginal_reliability = eyeprocess_irt_marginal_reliability(est, se)), class = "eye_irt_score_uncertainty")
}

#' Audit how well item information targets a theta distribution
#' @param items Item-parameter data frame or item collection.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param weights Optional numerical weights.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_information_targeting <- function(items, theta, weights = NULL, D = 1) {
  theta <- .ep09m2_theta(theta)
  if (is.null(weights)) weights <- rep(1 / length(theta), length(theta))
  weights <- as.numeric(weights)
  if (length(weights) != length(theta) || any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) stop("weights must be non-negative and match theta.", call. = FALSE)
  weights <- weights / sum(weights)
  info <- eyeprocess_irt_test_information(theta, items, D = D)
  structure(list(weighted_information = sum(info$information * weights), weighted_sem = sum(info$conditional_sem * weights), curve = info, weights = weights), class = "eye_irt_information_targeting")
}

#' Item bank object for adaptive design
#' @param items Item-parameter data frame or item collection.
#' @param content Item content/category metadata.
#' @param exposure_limit Maximum permitted item exposure.
#' @export
eyeprocess_irt_item_bank <- function(items, content = NULL, exposure_limit = 1) {
  items <- .ep09m2_item_pars(items)
  exposure_limit <- as.numeric(exposure_limit)
  if (length(exposure_limit) != 1L || !is.finite(exposure_limit) || exposure_limit <= 0 || exposure_limit > 1) stop("exposure_limit must lie in (0,1].", call. = FALSE)
  if (!is.null(content)) {
    content <- as.character(content)
    if (length(content) != nrow(items) || anyNA(content)) stop("content must match item rows and be non-missing.", call. = FALSE)
    items$content <- content
  }
  structure(list(items = items, exposure_limit = exposure_limit), class = "eye_irt_item_bank")
}

#' Validate an adaptive IRT item bank
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
validate_eyeprocess_irt_item_bank <- function(x) {
  if (!inherits(x, "eye_irt_item_bank")) stop("x must be an eye_irt_item_bank.", call. = FALSE)
  .ep09m2_item_pars(x$items)
  invisible(TRUE)
}

#' Select the most informative eligible item at a theta estimate
#' @param bank Validated item-bank object.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param administered Identifiers or records for administered items.
#' @param exposure Item exposure information used by the adaptive-selection rule.
#' @param content_required Content constraints required for item selection.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_item_selection <- function(bank, theta, administered = character(), exposure = NULL, content_required = NULL, D = 1) {
  validate_eyeprocess_irt_item_bank(bank); theta <- as.numeric(theta)
  if (length(theta) != 1L || !is.finite(theta)) stop("theta must be finite scalar.", call. = FALSE)
  items <- bank$items; eligible <- !items$item_id %in% as.character(administered)
  if (!is.null(exposure)) {
    exposure <- .ep09m2_as_df(exposure, "exposure"); .ep09m2_req_cols(exposure, c("item_id", "rate"), "exposure")
    rate <- setNames(exposure$rate, exposure$item_id); er <- rate[items$item_id]; er[is.na(er)] <- 0
    eligible <- eligible & er < bank$exposure_limit
  }
  if (!is.null(content_required)) {
    if (!"content" %in% names(items)) stop("bank has no content labels.", call. = FALSE)
    eligible <- eligible & items$content %in% as.character(content_required)
  }
  if (!any(eligible)) return(structure(list(selected = NA_character_, reason = "no_eligible_item", information = NA_real_), class = "eye_irt_item_selection"))
  idx <- which(eligible)
  info <- vapply(idx, function(j) eyeprocess_irt_item_information(theta, "4pl", a = items$a[j], b = items$b[j], c = items$c[j], d = items$d[j], D = D), numeric(1))
  j <- idx[which.max(info)]
  structure(list(selected = items$item_id[j], information = max(info), theta = theta, reason = "maximum_information", candidate_count = length(idx)), class = "eye_irt_item_selection")
}

#' Evaluate a simple adaptive stopping rule
#' @param n_administered Number of items already administered.
#' @param se Standard-error values.
#' @param min_items Minimum number of items required.
#' @param max_items Maximum permitted test length.
#' @param target_se Target conditional standard error for stopping.
#' @export
eyeprocess_irt_stopping_rule <- function(n_administered, se = NA_real_, min_items = 5L, max_items = 30L, target_se = 0.30) {
  n_administered <- as.integer(n_administered); min_items <- as.integer(min_items); max_items <- as.integer(max_items); target_se <- as.numeric(target_se); se <- as.numeric(se)
  if (length(n_administered) != 1L || length(min_items) != 1L || length(max_items) != 1L || anyNA(c(n_administered, min_items, max_items)) || min_items < 1L || max_items < min_items || n_administered < 0L || length(target_se) != 1L || !is.finite(target_se) || target_se <= 0 || length(se) != 1L) stop("invalid stopping-rule arguments.", call. = FALSE)
  precision_met <- is.finite(se) && se <= target_se && n_administered >= min_items
  max_met <- n_administered >= max_items
  list(stop = precision_met || max_met, reason = if (precision_met) "target_precision" else if (max_met) "maximum_items" else "continue", n_administered = n_administered, se = se)
}

#' Summarise item exposure rates
#' @param administered Identifiers or records for administered items.
#' @param item_bank_ids Complete set of item identifiers in the bank.
#' @export
eyeprocess_irt_exposure_summary <- function(administered, item_bank_ids = unique(administered)) {
  administered <- as.character(administered); item_bank_ids <- unique(as.character(item_bank_ids))
  tab <- table(factor(administered, levels = item_bank_ids)); total <- sum(tab)
  data.frame(item_id = item_bank_ids, count = as.integer(tab), rate = if (total) as.integer(tab) / total else 0, stringsAsFactors = FALSE)
}

#' Audit content balance in an administered adaptive form
#' @param administered Identifiers or records for administered items.
#' @param item_bank Item bank or item-bank data frame.
#' @param target Target level, distribution, or criterion.
#' @export
eyeprocess_irt_content_balance_audit <- function(administered, item_bank, target = NULL) {
  validate_eyeprocess_irt_item_bank(item_bank)
  if (!"content" %in% names(item_bank$items)) stop("item bank has no content labels.", call. = FALSE)
  map <- setNames(item_bank$items$content, item_bank$items$item_id); content <- map[as.character(administered)]
  if (anyNA(content)) stop("administered contains item IDs absent from bank.", call. = FALSE)
  if (!length(content)) stop("administered must contain at least one item.", call. = FALSE)
  obs <- prop.table(table(content))
  if (is.null(target)) { target <- rep(1 / length(obs), length(obs)); names(target) <- names(obs) }
  target_names <- names(target); target <- as.numeric(target)
  if (is.null(target_names) || length(target_names) != length(target) || anyNA(target_names) || any(!nzchar(target_names)) || anyDuplicated(target_names) || any(!is.finite(target)) || any(target < 0) || sum(target) <= 0) stop("target must be a uniquely named, finite, non-negative numeric vector with positive sum.", call. = FALSE)
  cats <- union(names(obs), target_names); o <- setNames(rep(0, length(cats)), cats); t <- o
  o[names(obs)] <- as.numeric(obs); t[target_names] <- target; t <- t / sum(t)
  data.frame(content = cats, observed = unname(o), target = unname(t), deviation = unname(o - t), stringsAsFactors = FALSE)
}

#' Create an auditable adaptive-testing trace
#' @param item_id Item identifier or vector of item identifiers.
#' @param theta_before Latent-trait estimate before item administration.
#' @param theta_after Latent-trait estimate after item administration.
#' @param se_after Conditional standard error after item administration.
#' @param information Item or test information value or vector.
#' @param response Observed item response or response variable.
#' @export
eyeprocess_irt_adaptive_trace <- function(item_id, theta_before, theta_after, se_after, information, response = NA_real_) {
  n <- length(item_id)
  if (length(response) == 1L && n != 1L && is.na(response)) response <- rep(NA_real_, n)
  vals <- list(theta_before, theta_after, se_after, information, response)
  if (any(vapply(vals, length, integer(1)) != n)) stop("all trace vectors must have equal length.", call. = FALSE)
  structure(data.frame(step = seq_len(n), item_id = as.character(item_id), theta_before = as.numeric(theta_before), theta_after = as.numeric(theta_after),
                       se_after = as.numeric(se_after), information = as.numeric(information), response = as.numeric(response), stringsAsFactors = FALSE),
            class = c("eye_irt_adaptive_trace", "data.frame"))
}

#' Information gain between two conditional standard errors
#' @param se_before Conditional standard error before an item is administered.
#' @param se_after Conditional standard error after item administration.
#' @export
eyeprocess_irt_information_gain <- function(se_before, se_after) {
  se_before <- as.numeric(se_before); se_after <- as.numeric(se_after)
  if (length(se_before) != length(se_after) || any(!is.finite(se_before)) || any(!is.finite(se_after)) || any(se_before <= 0 | se_after <= 0)) stop("SE values must be positive finite equal-length vectors.", call. = FALSE)
  1 / se_after^2 - 1 / se_before^2
}

#' Process-aware selection penalty without mental-state inference
#' @param information Item or test information value or vector.
#' @param burden Item-level process burden or cost measure.
#' @param burden_weight Weight applied to the burden penalty.
#' @param quality_risk Item-level measurement-quality risk.
#' @param quality_weight Weight applied to the quality-risk penalty.
#' @export
eyeprocess_irt_process_aware_selection_penalty <- function(information, burden, burden_weight = 0, quality_risk = 0, quality_weight = 0) {
  information <- as.numeric(information); burden <- as.numeric(burden); quality_risk <- as.numeric(quality_risk)
  if (length(burden) == 1L && length(information) > 1L) burden <- rep(burden, length(information))
  if (length(quality_risk) == 1L && length(information) > 1L) quality_risk <- rep(quality_risk, length(information))
  if (!length(information) || length(information) != length(burden) || length(information) != length(quality_risk) || any(!is.finite(information)) || any(!is.finite(burden)) || any(!is.finite(quality_risk))) stop("inputs must be finite compatible vectors.", call. = FALSE)
  burden_weight <- as.numeric(burden_weight); quality_weight <- as.numeric(quality_weight)
  if (length(burden_weight) != 1L || !is.finite(burden_weight) || burden_weight < 0 || length(quality_weight) != 1L || !is.finite(quality_weight) || quality_weight < 0) stop("weights must be finite non-negative scalars.", call. = FALSE)
  score <- information - burden_weight * burden - quality_weight * quality_risk
  attr(score, "guardrail") <- "burden and quality-risk inputs are design quantities; this score does not infer cognitive or clinical state."
  score
}

#' @export
print.eye_irt_score <- function(x, ...) {
  cat("eyeprocess IRT score\n")
  cat("  method  :", x$method, "\n")
  cat("  estimate:", format(x$estimate, digits = 4), "\n")
  if (!is.null(x$se)) cat("  SE      :", format(x$se, digits = 4), "\n")
  invisible(x)
}
