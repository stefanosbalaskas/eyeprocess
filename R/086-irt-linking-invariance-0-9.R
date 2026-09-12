# eyeprocess 0.9 Milestone #2: IRT linking, equating diagnostics, DIF/DTF effect functions

.ep09m2_anchor_merge <- function(reference, focal, anchors = NULL) {
  reference <- .ep09m2_item_pars(reference); focal <- .ep09m2_item_pars(focal)
  ids <- intersect(reference$item_id, focal$item_id)
  if (!is.null(anchors)) ids <- intersect(ids, as.character(anchors))
  if (length(ids) < 2L) stop("At least two common anchor items are required.", call. = FALSE)
  r <- reference[match(ids, reference$item_id), , drop = FALSE]
  f <- focal[match(ids, focal$item_id), , drop = FALSE]
  list(reference = r, focal = f, item_id = ids)
}

#' Mean-sigma IRT linking coefficients
#' @param reference Reference-form or reference-group item parameters.
#' @param focal Focal-form or focal-group item parameters.
#' @param anchors Anchor-item identifiers.
#' @return An object of class "eye_irt_link", stored as a named list, with components "A", "B", "method", "anchors", "objective". It contains mean-sigma IRT linking coefficients and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_mean_sigma_link <- function(reference, focal, anchors = NULL) {
  z <- .ep09m2_anchor_merge(reference, focal, anchors)
  sr <- stats::sd(z$reference$b); sf <- stats::sd(z$focal$b)
  if (!is.finite(sr) || !is.finite(sf) || sf <= 0) stop("anchor difficulty SDs must be finite and focal SD > 0.", call. = FALSE)
  A <- sr / sf; B <- mean(z$reference$b) - A * mean(z$focal$b)
  structure(list(A = A, B = B, method = "mean-sigma", anchors = z$item_id, objective = NA_real_), class = "eye_irt_link")
}

#' Mean-mean IRT linking coefficients
#' @param reference Reference-form or reference-group item parameters.
#' @param focal Focal-form or focal-group item parameters.
#' @param anchors Anchor-item identifiers.
#' @return An object of class "eye_irt_link", stored as a named list, with components "A", "B", "method", "anchors", "objective". It contains mean-mean IRT linking coefficients and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_mean_mean_link <- function(reference, focal, anchors = NULL) {
  z <- .ep09m2_anchor_merge(reference, focal, anchors)
  ma_r <- mean(z$reference$a); ma_f <- mean(z$focal$a)
  if (!is.finite(ma_r) || !is.finite(ma_f) || ma_r <= 0 || ma_f <= 0) stop("mean discrimination must be positive.", call. = FALSE)
  A <- ma_f / ma_r; B <- mean(z$reference$b) - A * mean(z$focal$b)
  structure(list(A = A, B = B, method = "mean-mean", anchors = z$item_id, objective = NA_real_), class = "eye_irt_link")
}

.ep09m2_apply_link_df <- function(items, A, B) {
  items <- .ep09m2_item_pars(items)
  if (!is.finite(A) || A <= 0 || !is.finite(B)) stop("A must be positive finite and B finite.", call. = FALSE)
  items$a <- items$a / A
  items$b <- A * items$b + B
  items
}

#' Apply linear IRT scale-linking coefficients
#' @param items Item-parameter data frame or item collection.
#' @param link IRT scale-linking coefficients or linking object.
#' @return An R object containing linear IRT scale-linking coefficients. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
eyeprocess_irt_apply_link <- function(items, link) {
  if (!inherits(link, "eye_irt_link")) stop("link must inherit from eye_irt_link.", call. = FALSE)
  .ep09m2_apply_link_df(items, link$A, link$B)
}

#' Stocking-Lord characteristic-curve linking
#' @param reference Reference-form or reference-group item parameters.
#' @param focal Focal-form or focal-group item parameters.
#' @param anchors Anchor-item identifiers.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param weights Optional numerical weights.
#' @param start Starting values for numerical optimization.
#' @return An object of class "eye_irt_link", stored as a named list, with components "A", "B", "method", "anchors", "objective", "convergence". It contains stocking-Lord characteristic-curve linking and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_stocking_lord_link <- function(reference, focal, anchors = NULL, theta = seq(-4, 4, length.out = 81), weights = NULL, start = c(A = 1, B = 0)) {
  z <- .ep09m2_anchor_merge(reference, focal, anchors); theta <- .ep09m2_theta(theta)
  if (is.null(weights)) weights <- stats::dnorm(theta)
  weights <- as.numeric(weights)
  if (length(weights) != length(theta) || any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) stop("weights must be finite, non-negative, match theta, and have positive sum.", call. = FALSE)
  weights <- weights / sum(weights)
  start <- as.numeric(start)
  if (length(start) != 2L || any(!is.finite(start)) || start[1L] <= 0) stop("start must contain positive finite A and finite B.", call. = FALSE)
  tref <- eyeprocess_irt_test_characteristic_curve(theta, z$reference)$expected_score
  objective <- function(par) {
    A <- exp(par[1]); B <- par[2]
    linked <- .ep09m2_apply_link_df(z$focal, A, B)
    tfoc <- eyeprocess_irt_test_characteristic_curve(theta, linked)$expected_score
    sum(weights * (tref - tfoc)^2)
  }
  fit <- stats::optim(c(log(start[[1L]]), start[[2L]]), objective, method = "BFGS")
  structure(list(A = exp(fit$par[1]), B = fit$par[2], method = "Stocking-Lord", anchors = z$item_id, objective = fit$value, convergence = fit$convergence), class = "eye_irt_link")
}

#' Haebara item-characteristic-curve linking
#' @param reference Reference-form or reference-group item parameters.
#' @param focal Focal-form or focal-group item parameters.
#' @param anchors Anchor-item identifiers.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param weights Optional numerical weights.
#' @param start Starting values for numerical optimization.
#' @return An object of class "eye_irt_link", stored as a named list, with components "A", "B", "method", "anchors", "objective", "convergence". It contains haebara item-characteristic-curve linking and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_haebara_link <- function(reference, focal, anchors = NULL, theta = seq(-4, 4, length.out = 81), weights = NULL, start = c(A = 1, B = 0)) {
  z <- .ep09m2_anchor_merge(reference, focal, anchors); theta <- .ep09m2_theta(theta)
  if (is.null(weights)) weights <- stats::dnorm(theta)
  weights <- as.numeric(weights)
  if (length(weights) != length(theta) || any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) stop("weights must be finite, non-negative, match theta, and have positive sum.", call. = FALSE)
  weights <- weights / sum(weights)
  start <- as.numeric(start)
  if (length(start) != 2L || any(!is.finite(start)) || start[1L] <= 0) stop("start must contain positive finite A and finite B.", call. = FALSE)
  pref <- vapply(seq_len(nrow(z$reference)), function(j) eyeprocess_irt_4pl_probability(theta, z$reference$a[j], z$reference$b[j], z$reference$c[j], z$reference$d[j]), numeric(length(theta)))
  objective <- function(par) {
    linked <- .ep09m2_apply_link_df(z$focal, exp(par[1]), par[2])
    pf <- vapply(seq_len(nrow(linked)), function(j) eyeprocess_irt_4pl_probability(theta, linked$a[j], linked$b[j], linked$c[j], linked$d[j]), numeric(length(theta)))
    sum(weights * rowSums((pref - pf)^2))
  }
  fit <- stats::optim(c(log(start[[1L]]), start[[2L]]), objective, method = "BFGS")
  structure(list(A = exp(fit$par[1]), B = fit$par[2], method = "Haebara", anchors = z$item_id, objective = fit$value, convergence = fit$convergence), class = "eye_irt_link")
}

#' Compare linking estimates across anchor subsets
#' @param reference Reference-form or reference-group item parameters.
#' @param focal Focal-form or focal-group item parameters.
#' @param anchor_sets Value supplied for the anchor sets argument.
#' @param method Scoring, linking, or analysis method.
#' @return An object of class "eye_irt_link_stability", stored as a named list, with components "table", "sd_A", "sd_B", "method". It contains linking estimates across anchor subsets and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_link_stability <- function(reference, focal, anchor_sets, method = c("mean-sigma", "mean-mean", "Stocking-Lord", "Haebara")) {
  method <- match.arg(method)
  if (!is.list(anchor_sets) || !length(anchor_sets)) stop("anchor_sets must be a non-empty list.", call. = FALSE)
  fun <- switch(method, `mean-sigma` = eyeprocess_irt_mean_sigma_link, `mean-mean` = eyeprocess_irt_mean_mean_link,
                `Stocking-Lord` = eyeprocess_irt_stocking_lord_link, Haebara = eyeprocess_irt_haebara_link)
  out <- lapply(seq_along(anchor_sets), function(i) {
    z <- fun(reference, focal, anchors = anchor_sets[[i]])
    data.frame(set = if (!is.null(names(anchor_sets)) && nzchar(names(anchor_sets)[i])) names(anchor_sets)[i] else paste0("set_", i), n_anchors = length(z$anchors), A = z$A, B = z$B, objective = z$objective, stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, out)
  structure(list(table = tab, sd_A = .ep09m2_finite_sd(tab$A), sd_B = .ep09m2_finite_sd(tab$B), method = method), class = "eye_irt_link_stability")
}

#' Audit candidate anchor items using supplied DIF evidence
#' @param items Item-parameter data frame or item collection.
#' @param dif Differential-item-functioning evidence or summary.
#' @param max_abs_effect Maximum permitted absolute effect for an anchor candidate.
#' @param min_information Minimum required item information.
#' @return A data frame containing candidate anchor items using supplied DIF evidence. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
eyeprocess_irt_anchor_audit <- function(items, dif = NULL, max_abs_effect = 0.10, min_information = NULL) {
  items <- .ep09m2_item_pars(items); max_abs_effect <- as.numeric(max_abs_effect)
  if (!is.finite(max_abs_effect) || max_abs_effect < 0) stop("max_abs_effect must be non-negative.", call. = FALSE)
  out <- data.frame(item_id = items$item_id, eligible = TRUE, reason = "eligible", stringsAsFactors = FALSE)
  if (!is.null(dif)) {
    dif <- .ep09m2_as_df(dif, "dif"); .ep09m2_req_cols(dif, c("item_id", "effect"), "dif")
    eff <- setNames(abs(as.numeric(dif$effect)), dif$item_id); e <- eff[out$item_id]
    flag <- is.finite(e) & e > max_abs_effect; out$eligible[flag] <- FALSE; out$reason[flag] <- "DIF effect exceeds review threshold"
  }
  if (!is.null(min_information)) {
    min_information <- as.numeric(min_information)
    info <- vapply(seq_len(nrow(items)), function(j) eyeprocess_irt_item_information(0, "4pl", a = items$a[j], b = items$b[j], c = items$c[j], d = items$d[j]), numeric(1))
    flag <- info < min_information; out$eligible[flag] <- FALSE; out$reason[flag] <- "information below review threshold"
    out$information_theta0 <- info
  }
  attr(out, "guardrail") <- "Anchor eligibility is a screening aid; formal invariance evidence remains model-dependent."
  out
}

#' Iteratively remove anchors exceeding a supplied effect threshold
#' @param items Item-parameter data frame or item collection.
#' @param effect_fun Function that computes the anchor-screening effect.
#' @param initial Initial value, state, or anchor set.
#' @param threshold Decision or diagnostic threshold.
#' @param max_iter Maximum number of iterations.
#' @return An object of class "eye_irt_anchor_purification", stored as a named list, with components "anchors", "history", "threshold". It contains iteratively remove anchors exceeding a supplied effect threshold and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_anchor_purification <- function(items, effect_fun, initial = items$item_id, threshold = 0.10, max_iter = 10L) {
  items <- .ep09m2_item_pars(items)
  if (!is.function(effect_fun)) stop("effect_fun must be a function accepting anchor IDs and returning item_id/effect data.", call. = FALSE)
  anchors <- intersect(as.character(initial), items$item_id); max_iter <- as.integer(max_iter)
  history <- list()
  for (iter in seq_len(max_iter)) {
    eff <- effect_fun(anchors); eff <- .ep09m2_as_df(eff, "effect_fun result"); .ep09m2_req_cols(eff, c("item_id", "effect"), "effect_fun result")
    bad <- intersect(eff$item_id[is.finite(eff$effect) & abs(eff$effect) > threshold], anchors)
    history[[iter]] <- data.frame(iteration = iter, n_anchors = length(anchors), removed = paste(bad, collapse = ";"), stringsAsFactors = FALSE)
    if (!length(bad)) break
    anchors <- setdiff(anchors, bad)
    if (length(anchors) < 2L) stop("Anchor purification left fewer than two anchors.", call. = FALSE)
  }
  structure(list(anchors = anchors, history = do.call(rbind, history), threshold = threshold), class = "eye_irt_anchor_purification")
}

#' Differential item functioning effect curve from two parameter sets
#' @param reference_item Reference-group item parameters.
#' @param focal_item Focal-group item parameters.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @return An object of class "eye_irt_dif_curve", "data.frame", stored as a data frame, containing differential item functioning effect curve from two parameter sets and associated metadata needed to interpret the result.
#' @export
eyeprocess_irt_dif_effect_curve <- function(reference_item, focal_item, theta = seq(-4, 4, length.out = 81)) {
  reference_item <- .ep09m2_item_pars(reference_item); focal_item <- .ep09m2_item_pars(focal_item)
  if (nrow(reference_item) != 1L || nrow(focal_item) != 1L) stop("reference_item and focal_item must each contain exactly one item.", call. = FALSE)
  theta <- .ep09m2_theta(theta)
  pr <- eyeprocess_irt_4pl_probability(theta, reference_item$a, reference_item$b, reference_item$c, reference_item$d)
  pf <- eyeprocess_irt_4pl_probability(theta, focal_item$a, focal_item$b, focal_item$c, focal_item$d)
  structure(data.frame(theta = theta, reference = pr, focal = pf, signed_difference = pf - pr, absolute_difference = abs(pf - pr)), class = c("eye_irt_dif_curve", "data.frame"))
}

#' Differential test functioning effect curve
#' @param reference Reference-form or reference-group item parameters.
#' @param focal Focal-form or focal-group item parameters.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @return An object of class "eye_irt_dtf_curve", "data.frame", stored as a data frame, containing differential test functioning effect curve and associated metadata needed to interpret the result.
#' @export
eyeprocess_irt_dtf_curve <- function(reference, focal, theta = seq(-4, 4, length.out = 81)) {
  reference <- .ep09m2_item_pars(reference); focal <- .ep09m2_item_pars(focal)
  ids <- intersect(reference$item_id, focal$item_id)
  if (!length(ids)) stop("No common items.", call. = FALSE)
  r <- reference[match(ids, reference$item_id), , drop = FALSE]; f <- focal[match(ids, focal$item_id), , drop = FALSE]
  tr <- eyeprocess_irt_test_characteristic_curve(theta, r)$expected_score; tf <- eyeprocess_irt_test_characteristic_curve(theta, f)$expected_score
  structure(data.frame(theta = theta, reference = tr, focal = tf, signed_difference = tf - tr, absolute_difference = abs(tf - tr)), class = c("eye_irt_dtf_curve", "data.frame"))
}

#' Summarise DIF/DTF curve magnitude
#' @param curve Curve data to summarize or inspect.
#' @return A named list with components "max_abs", "mean_abs", "signed_area", containing dIF/DTF curve magnitude and associated metadata or diagnostics.
#' @export
eyeprocess_irt_functioning_effect_summary <- function(curve) {
  curve <- .ep09m2_as_df(curve, "curve"); .ep09m2_req_cols(curve, c("absolute_difference", "signed_difference"), "curve")
  abs_diff <- as.numeric(curve$absolute_difference)
  finite_abs <- abs_diff[is.finite(abs_diff)]
  signed_area <- NA_real_
  if (all(c("theta", "signed_difference") %in% names(curve))) {
    ok <- is.finite(curve$theta) & is.finite(curve$signed_difference)
    if (sum(ok) >= 2L) signed_area <- eyeprocess_irt_information_area(curve$theta[ok], curve$signed_difference[ok])
  }
  list(max_abs = if (length(finite_abs)) max(finite_abs) else NA_real_,
       mean_abs = if (length(finite_abs)) mean(finite_abs) else NA_real_,
       signed_area = signed_area)
}

#' Compare psychometric DIF effect sizes with process-channel contrasts
#' @param dif Differential-item-functioning evidence or summary.
#' @param process Process-measure columns or process object.
#' @param item_id Item identifier or vector of item identifiers.
#' @param dif_effect DIF effect used for concordance.
#' @param process_effect Process-side item effect used for concordance.
#' @return An object of class "eye_irt_process_dif_concordance", stored as a named list, with components "n", "correlation", "table", "guardrail". It contains psychometric DIF effect sizes with process-channel contrasts and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_process_dif_concordance <- function(dif, process, item_id = "item_id", dif_effect = "effect", process_effect = "effect") {
  dif <- .ep09m2_as_df(dif, "dif"); process <- .ep09m2_as_df(process, "process")
  .ep09m2_req_cols(dif, c(item_id, dif_effect), "dif"); .ep09m2_req_cols(process, c(item_id, process_effect), "process")
  z <- merge(dif[, c(item_id, dif_effect), drop = FALSE], process[, c(item_id, process_effect), drop = FALSE], by = item_id, suffixes = c("_dif", "_process"))
  if (!nrow(z)) return(structure(list(n = 0L, correlation = NA_real_, table = z), class = "eye_irt_process_dif_concordance"))
  a <- as.numeric(z[[paste0(dif_effect, "_dif")]]); b <- as.numeric(z[[paste0(process_effect, "_process")]])
  keep <- is.finite(a) & is.finite(b)
  corv <- if (sum(keep) >= 3L && stats::sd(a[keep]) > 0 && stats::sd(b[keep]) > 0) stats::cor(a[keep], b[keep]) else NA_real_
  structure(list(n = sum(keep), correlation = corv, table = z,
                 guardrail = "Concordance is descriptive and does not establish a causal explanation of DIF."), class = "eye_irt_process_dif_concordance")
}

#' Summarise item-parameter drift over sessions
#' @param parameters Item-parameter data frame or parameter estimates.
#' @param item_id Item identifier or vector of item identifiers.
#' @param session Session identifier or grouping variable.
#' @param parameter Name of the parameter to compare.
#' @return A tabular R object containing item-parameter drift over sessions; rows represent analysis units and columns contain the returned quantities.
#' @export
eyeprocess_irt_session_drift <- function(parameters, item_id = "item_id", session = "session", parameter = "b") {
  parameters <- .ep09m2_as_df(parameters, "parameters"); .ep09m2_req_cols(parameters, c(item_id, session, parameter), "parameters")
  split_rows <- split(seq_len(nrow(parameters)), parameters[[item_id]])
  do.call(rbind, lapply(split_rows, function(ii) {
    z <- parameters[ii, , drop = FALSE]
    ord <- order(z[[session]])
    z <- z[ord, , drop = FALSE]
    v <- as.numeric(z[[parameter]])
    vf <- v[is.finite(v)]
    data.frame(
      item_id = as.character(z[[item_id]][1L]),
      n_sessions = length(vf),
      first = if (length(vf)) vf[[1L]] else NA_real_,
      last = if (length(vf)) vf[[length(vf)]] else NA_real_,
      change = if (length(vf)) vf[[length(vf)]] - vf[[1L]] else NA_real_,
      range = if (length(vf)) diff(range(vf)) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

#' Summarise parameter drift across acquisition devices
#' @param parameters Item-parameter data frame or parameter estimates.
#' @param item_id Item identifier or vector of item identifiers.
#' @param device Device identifier or grouping variable.
#' @param parameter Name of the parameter to compare.
#' @return A tabular R object containing parameter drift across acquisition devices; rows represent analysis units and columns contain the returned quantities.
#' @export
eyeprocess_irt_device_drift <- function(parameters, item_id = "item_id", device = "device", parameter = "b") {
  parameters <- .ep09m2_as_df(parameters, "parameters"); .ep09m2_req_cols(parameters, c(item_id, device, parameter), "parameters")
  split_rows <- split(seq_len(nrow(parameters)), parameters[[item_id]])
  do.call(rbind, lapply(split_rows, function(ii) {
    z <- parameters[ii, , drop = FALSE]; v <- as.numeric(z[[parameter]]); v <- v[is.finite(v)]
    data.frame(item_id = as.character(z[[item_id]][1]), n_devices = length(unique(z[[device]])), mean = if (length(v)) mean(v) else NA_real_, sd = if (length(v) > 1L) stats::sd(v) else NA_real_, range = if (length(v)) diff(range(v)) else NA_real_, stringsAsFactors = FALSE)
  }))
}

#' Combine invariance evidence without converting it to a binary validity claim
#' @param anchor_audit Anchor-item audit result.
#' @param dif Differential-item-functioning evidence or summary.
#' @param dtf Differential-test-functioning evidence or curve.
#' @param linking Scale-linking evidence or result.
#' @param process_concordance Process-DIF concordance evidence.
#' @return An object of class "eye_irt_invariance_evidence", stored as a named list, with components "components", "present", "completeness", "interpretation". It contains combine invariance evidence without converting it to a binary validity claim and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_irt_invariance_evidence <- function(anchor_audit = NULL, dif = NULL, dtf = NULL, linking = NULL, process_concordance = NULL) {
  components <- list(anchor_audit = anchor_audit, dif = dif, dtf = dtf, linking = linking, process_concordance = process_concordance)
  present <- names(components)[!vapply(components, is.null, logical(1))]
  structure(list(components = components, present = present, completeness = length(present) / length(components),
                 interpretation = "Evidence components characterize scale stability and functioning; no universal invariance cutoff is imposed."), class = "eye_irt_invariance_evidence")
}

#' @export
print.eye_irt_link <- function(x, ...) {
  cat("eyeprocess IRT link\n")
  cat("  method :", x$method, "\n")
  cat("  A      :", format(x$A, digits = 6), "\n")
  cat("  B      :", format(x$B, digits = 6), "\n")
  cat("  anchors:", length(x$anchors), "\n")
  invisible(x)
}
