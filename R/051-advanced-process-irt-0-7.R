# eyeprocess 0.7.0.9000 -------------------------------------------------------
# Advanced process-IRT integrations and explicitly gated estimators.

.ep07_logsumexp <- function(x) {
  m <- max(x)
  if (!is.finite(m)) return(m)
  m + log(sum(exp(x - m)))
}

.ep07_mvn_diag_logdens <- function(x, mu, sd) {
  sum(stats::dnorm(x, mean = mu, sd = pmax(sd, 1e-6), log = TRUE))
}

.ep07_hmm_fb <- function(x, pi, A, mu, sd) {
  Tn <- nrow(x); K <- length(pi)
  logB <- matrix(NA_real_, Tn, K)
  for (t in seq_len(Tn)) for (k in seq_len(K)) logB[t, k] <- .ep07_mvn_diag_logdens(x[t, ], mu[k, ], sd[k, ])
  la <- matrix(-Inf, Tn, K)
  la[1, ] <- log(pmax(pi, 1e-300)) + logB[1, ]
  if (Tn > 1L) {
    for (t in 2:Tn) for (k in seq_len(K)) {
      la[t, k] <- logB[t, k] + .ep07_logsumexp(la[t - 1L, ] + log(pmax(A[, k], 1e-300)))
    }
  }
  ll <- .ep07_logsumexp(la[Tn, ])
  lb <- matrix(0, Tn, K)
  if (Tn > 1L) {
    for (t in (Tn - 1L):1L) for (j in seq_len(K)) {
      lb[t, j] <- .ep07_logsumexp(log(pmax(A[j, ], 1e-300)) + logB[t + 1L, ] + lb[t + 1L, ])
    }
  }
  loggamma <- la + lb - ll
  gamma <- exp(loggamma)
  xi <- array(0, dim = c(max(Tn - 1L, 0L), K, K))
  if (Tn > 1L) {
    for (t in seq_len(Tn - 1L)) {
      z <- outer(la[t, ], rep(1, K)) + log(pmax(A, 1e-300)) +
        outer(rep(1, K), logB[t + 1L, ] + lb[t + 1L, ]) - ll
      xi[t, , ] <- exp(z)
    }
  }
  list(logLik = ll, gamma = gamma, xi = xi)
}

#' Process-state HMM with an IRT response layer
#'
#' Fits a diagonal-Gaussian HMM to standardized process features within each
#' sequence, then uses state occupancy as explicit process evidence in a
#' response model. This two-stage reference engine is deliberately interpretable
#' and should be distinguished from a fully joint HMM-IRT likelihood.
#'
#' @param data Input data frame or compatible tabular object.
#' @param sequence_id Sequence identifier.
#' @param order Within-sequence ordering variable.
#' @param process_features Names of process-derived features.
#' @param response Response variable or response-column name.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param n_states Number of latent process states.
#' @param max_iter Maximum number of iterations.
#' @param tol Numerical convergence tolerance.
#' @param seed Random-number seed.
#' @export
fit_process_hmm_irt <- function(data,
                                sequence_id = "trial_id", order = "timestamp",
                                process_features = c("x", "y"),
                                response = "response",
                                person = "participant_id", item = "item_id",
                                n_states = 3L, max_iter = 100L, tol = 1e-5,
                                seed = 1) {
  n_states <- as.integer(n_states)
  if (n_states < 2L) stop("`n_states` must be at least 2.", call. = FALSE)
  cols <- unique(c(sequence_id, order, process_features, response, person, item))
  d <- .ep07_model_frame(data, cols)
  d <- d[stats::complete.cases(d[c(sequence_id, order, process_features)]), , drop = FALSE]
  X <- scale(as.matrix(d[process_features]))
  X[!is.finite(X)] <- 0
  seqs <- split(seq_len(nrow(d)), d[[sequence_id]])
  set.seed(seed)
  km <- stats::kmeans(X, centers = n_states, nstart = 10)
  mu <- km$centers
  global_sd <- apply(X, 2L, stats::sd)
  global_sd[!is.finite(global_sd) | global_sd < 1e-4] <- 1
  sds <- matrix(rep(global_sd, each = n_states), nrow = n_states)
  pi <- rep(1 / n_states, n_states)
  A <- matrix(1 / n_states, n_states, n_states)
  for (ids in seqs) {
    cl <- km$cluster[ids]
    if (length(cl) > 1L) for (t in seq_len(length(cl) - 1L)) A[cl[t], cl[t + 1L]] <- A[cl[t], cl[t + 1L]] + 1
  }
  A <- A / rowSums(A)

  ll_history <- numeric()
  gamma_all <- matrix(0, nrow(X), n_states)
  for (iter in seq_len(max_iter)) {
    pi_num <- rep(0, n_states)
    A_num <- matrix(0, n_states, n_states)
    A_den <- rep(0, n_states)
    mu_num <- matrix(0, n_states, ncol(X)); mu_den <- rep(0, n_states)
    second_num <- matrix(0, n_states, ncol(X))
    ll <- 0
    for (ids in seqs) {
      ord <- ids[order(d[[order]][ids])]
      fb <- .ep07_hmm_fb(X[ord, , drop = FALSE], pi, A, mu, sds)
      ll <- ll + fb$logLik
      gamma_all[ord, ] <- fb$gamma
      pi_num <- pi_num + fb$gamma[1L, ]
      if (length(ord) > 1L) {
        A_num <- A_num + apply(fb$xi, c(2, 3), sum)
        A_den <- A_den + colSums(fb$gamma[-length(ord), , drop = FALSE])
      }
      for (k in seq_len(n_states)) {
        w <- fb$gamma[, k]
        mu_num[k, ] <- mu_num[k, ] + colSums(X[ord, , drop = FALSE] * w)
        second_num[k, ] <- second_num[k, ] + colSums((X[ord, , drop = FALSE]^2) * w)
        mu_den[k] <- mu_den[k] + sum(w)
      }
    }
    pi <- (pi_num + 1e-6) / sum(pi_num + 1e-6)
    for (j in seq_len(n_states)) A[j, ] <- (A_num[j, ] + 1e-6) / sum(A_num[j, ] + 1e-6)
    mu <- mu_num / pmax(mu_den, 1e-8)
    var <- second_num / pmax(mu_den, 1e-8) - mu^2
    sds <- sqrt(pmax(var, 1e-4))
    ll_history <- c(ll_history, ll)
    if (length(ll_history) > 1L && abs(diff(tail(ll_history, 2L))) < tol) break
  }

  d$.process_state <- max.col(gamma_all, ties.method = "first")
  occupancy <- do.call(rbind, lapply(names(seqs), function(sid) {
    ids <- seqs[[sid]]
    p <- colMeans(gamma_all[ids, , drop = FALSE])
    row <- data.frame(sequence_id = sid, stringsAsFactors = FALSE)
    for (k in seq_len(n_states)) row[[paste0("state_", k, "_occupancy")]] <- p[k]
    row
  }))
  names(occupancy)[1L] <- sequence_id
  # attach one response/person/item record per sequence
  meta <- d[!duplicated(d[[sequence_id]]), c(sequence_id, response, person, item), drop = FALSE]
  summary_data <- merge(meta, occupancy, by = sequence_id, all.x = TRUE, sort = FALSE)
  occ_cols <- grep("_occupancy$", names(summary_data), value = TRUE)
  response_fit <- NULL
  if (all(stats::na.omit(summary_data[[response]]) %in% c(0, 1))) {
    rhs <- paste(occ_cols[-length(occ_cols)], collapse = " + ")
    if (!nzchar(rhs)) rhs <- "1"
    if (requireNamespace("lme4", quietly = TRUE)) {
      f <- stats::as.formula(sprintf("%s ~ %s + (1|%s) + (1|%s)", response, rhs, person, item))
      response_fit <- lme4::glmer(f, data = summary_data, family = stats::binomial())
    } else {
      f <- stats::as.formula(sprintf("%s ~ %s + factor(%s) + factor(%s)", response, rhs, person, item))
      response_fit <- stats::glm(f, data = summary_data, family = stats::binomial())
    }
  }
  structure(list(
    pi = pi, transition = A, means = mu, sds = sds,
    posterior_state = gamma_all, state = d$.process_state,
    row_data = d, occupancy = occupancy, summary_data = summary_data,
    response_model = response_fit, logLik = tail(ll_history, 1L), logLik_history = ll_history,
    scaling = list(center = attr(X, "scaled:center"), scale = attr(X, "scaled:scale")),
    process_features = process_features, n_states = n_states,
    status = "experimental-two-stage",
    note = "States are statistical process states and must not be assigned psychological labels without independent validation."
  ), class = "eye_process_hmm_irt")
}

#' Summarize HMM state occupancy
#' @param object A fitted eyeprocess model or audit object.
#' @export
process_state_occupancy <- function(object) {
  if (!inherits(object, "eye_process_hmm_irt")) stop("`object` must be an eye_process_hmm_irt.", call. = FALSE)
  object$occupancy
}

#' Summarize HMM process-state transitions
#' @param object A fitted eyeprocess model or audit object.
#' @export
process_state_transition_summary <- function(object) {
  if (!inherits(object, "eye_process_hmm_irt")) stop("`object` must be an eye_process_hmm_irt.", call. = FALSE)
  A <- object$transition
  out <- as.data.frame(as.table(A), stringsAsFactors = FALSE)
  names(out) <- c("from_state", "to_state", "probability")
  out
}

#' Cognitive-diagnosis model with process indicators
#'
#' Uses GDINA for the response layer when available and retains process features
#' as a parallel diagnostic channel. Process/mastery associations are reported
#' descriptively and do not redefine the Q-matrix or skill labels.
#'
#' @param response_matrix Person-by-item response matrix.
#' @param q_matrix Q-matrix for cognitive-diagnosis modeling.
#' @param process_data Process-data input used by the model.
#' @param process_features Names of process-derived features.
#' @param person_id Person or participant identifier.
#' @param engine Estimation engine.
#' @param external_engine Validated external fitting function.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
fit_cognitive_diagnosis_process <- function(response_matrix, q_matrix,
                                            process_data = NULL,
                                            process_features = NULL,
                                            person_id = NULL,
                                            engine = c("GDINA", "external"),
                                            external_engine = NULL, ...) {
  engine <- match.arg(engine)
  X <- as.matrix(response_matrix)
  Q <- as.matrix(q_matrix)
  if (ncol(X) != nrow(Q)) stop("Response-matrix columns must match Q-matrix rows.", call. = FALSE)
  if (engine == "GDINA") {
    if (!requireNamespace("GDINA", quietly = TRUE)) stop("Install optional package `GDINA`.", call. = FALSE)
    response_fit <- GDINA::GDINA(dat = X, Q = Q, verbose = 0, ...)
  } else {
    if (!is.function(external_engine)) stop("Supply `external_engine` as a function.", call. = FALSE)
    response_fit <- external_engine(response_matrix = X, q_matrix = Q, ...)
  }
  process_summary <- process_association <- NULL
  if (!is.null(process_data) && length(process_features)) {
    pd <- .ep07_model_frame(process_data, c(person_id, process_features))
    if (is.null(person_id)) stop("`person_id` is required when `process_data` is supplied.", call. = FALSE)
    agg <- stats::aggregate(pd[process_features], list(person_id = pd[[person_id]]), mean, na.rm = TRUE)
    M <- scale(as.matrix(agg[process_features]))
    M[!is.finite(M)] <- 0
    pc <- if (ncol(M) > 1L) stats::prcomp(M, center = FALSE, scale. = FALSE)$x[, 1L] else as.numeric(M[, 1L])
    process_summary <- data.frame(person_id = agg$person_id, process_surrogate = pc)
    mastery <- try({
      if (engine == "GDINA") GDINA::personparm(response_fit, what = "EAP") else NULL
    }, silent = TRUE)
    if (!inherits(mastery, "try-error") && !is.null(mastery)) {
      mastery <- as.matrix(mastery)
      n <- min(nrow(mastery), nrow(process_summary))
      process_association <- vapply(seq_len(ncol(mastery)), function(j) {
        .ep07_safe_cor(process_summary$process_surrogate[seq_len(n)], mastery[seq_len(n), j])
      }, numeric(1))
    }
  }
  structure(list(response_model = response_fit, process_summary = process_summary,
                 process_mastery_correlation = process_association,
                 q_matrix = Q, status = "experimental-integration"),
            class = "eye_cognitive_diagnosis_process")
}

#' Latent process-class IRT reference model
#'
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param process_features Names of process-derived features.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param n_classes Number of latent classes.
#' @param seed Random-number seed.
#' @export
fit_latent_class_process_irt <- function(data,
                                         response = "response",
                                         process_features,
                                         person = "participant_id", item = "item_id",
                                         n_classes = 2L, seed = 1) {
  d <- .ep07_model_frame(data, unique(c(response, process_features, person, item)))
  X <- scale(as.matrix(d[process_features]))
  X[!is.finite(X)] <- 0
  set.seed(seed)
  km <- stats::kmeans(X, centers = as.integer(n_classes), nstart = 25)
  d$.process_class <- factor(km$cluster)
  response_fit <- NULL
  if (all(stats::na.omit(d[[response]]) %in% c(0, 1))) {
    if (requireNamespace("lme4", quietly = TRUE)) {
      f <- stats::as.formula(sprintf("%s ~ .process_class + (1|%s) + (1|%s)", response, person, item))
      response_fit <- lme4::glmer(f, data = d, family = stats::binomial())
    } else {
      f <- stats::as.formula(sprintf("%s ~ .process_class + factor(%s) + factor(%s)", response, person, item))
      response_fit <- stats::glm(f, data = d, family = stats::binomial())
    }
  }
  structure(list(response_model = response_fit, class = d$.process_class,
                 centers = km$centers, data = d, process_features = process_features,
                 status = "experimental-two-stage"), class = "eye_latent_class_process_irt")
}

#' Cross-classified process IRT reference model
#'
#' Treats the process outcome as repeated evidence crossed by person and item,
#' with optional contextual grouping factors. This is useful when process events
#' themselves, not only item summaries, are the observations.
#' @param data Input data frame or compatible tabular object.
#' @param outcome Outcome variable.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param context Context or grouping variable.
#' @param family Statistical family used by the channel or model.
#' @param fixed Fixed-effects specification.
#' @export
fit_crossclassified_process_irt <- function(data, outcome,
                                            person = "participant_id", item = "item_id",
                                            context = NULL,
                                            family = c("gaussian", "binomial", "poisson", "negative_binomial"),
                                            fixed = NULL) {
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Install optional package `lme4`.", call. = FALSE)
  family <- match.arg(family)
  cols <- unique(c(outcome, person, item, context, fixed))
  d <- .ep07_model_frame(data, cols)
  random <- c(person, item, context)
  random <- random[!is.na(random) & nzchar(random)]
  rhs <- c(fixed, sprintf("(1|%s)", random))
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  f <- stats::as.formula(sprintf("%s ~ %s", outcome, if (length(rhs)) paste(rhs, collapse = " + ") else "1"))
  fit <- switch(family,
                gaussian = lme4::lmer(f, data = d),
                binomial = lme4::glmer(f, data = d, family = stats::binomial()),
                poisson = lme4::glmer(f, data = d, family = stats::poisson()),
                negative_binomial = lme4::glmer.nb(f, data = d))
  structure(list(model = fit, family = family, person = person, item = item,
                 context = context, status = "reference-estimator"),
            class = "eye_crossclassified_process_irt")
}

#' Fit a latent-space IRT model using LSMjml
#'
#' @param response_matrix Person-by-item matrix with lowest score coded zero.
#' @param dimensions Latent-space dimensionality.
#' @param penalty Optional L2 penalty passed to `LSMjml::LSMfit()`.
#' @param constraint Optional norm constraint `C` passed to `LSMfit()`.
#' @param starts Starting-value strategy.
#' @param tol Numerical convergence tolerance.
#' @param silent Whether engine messages are suppressed.
#' @export
fit_latent_space_irt <- function(response_matrix, dimensions = 2L,
                                 penalty = NULL, constraint = NULL,
                                 starts = NULL, tol = 1e-3, silent = TRUE) {
  if (!requireNamespace("LSMjml", quietly = TRUE)) {
    stop("Install optional package `LSMjml` (>= 0.6.0) for latent-space IRT.", call. = FALSE)
  }
  X <- as.matrix(response_matrix)
  if (any(X < 0, na.rm = TRUE)) stop("LSMjml requires item scores with lowest category coded 0.", call. = FALSE)
  fit <- LSMjml::LSMfit(X = X, ndim_z = as.integer(dimensions), penalty = penalty,
                        C = constraint, starts = starts, tol = tol, silent = silent)
  structure(list(model = fit, person_coordinates = fit$z, item_coordinates = fit$w,
                 person_intercept = fit$theta, item_intercept = fit$b,
                 dimensions = dimensions, engine = "LSMjml",
                 status = "external-validated-engine"), class = "eye_latent_space_irt")
}

#' Return person/item latent-space coordinates
#' @param object A fitted eyeprocess model or audit object.
#' @param entity Entity type to map or validate.
#' @export
process_residual_map <- function(object, entity = c("both", "person", "item")) {
  if (!inherits(object, "eye_latent_space_irt")) stop("`object` must be an eye_latent_space_irt.", call. = FALSE)
  entity <- match.arg(entity)
  p <- as.data.frame(object$person_coordinates)
  p$entity_id <- rownames(object$person_coordinates) %||% seq_len(nrow(p)); p$entity_type <- "person"
  i <- as.data.frame(object$item_coordinates)
  i$entity_id <- rownames(object$item_coordinates) %||% seq_len(nrow(i)); i$entity_type <- "item"
  if (entity == "person") return(p)
  if (entity == "item") return(i)
  rbind(p, i)
}

#' Validate latent-space proximity against process similarity
#'
#' @param process_matrix Rows correspond to persons or items in the same order
#'   as the fitted latent coordinates.
#' @param object A fitted eyeprocess model or audit object.
#' @param entity Entity type to map or validate.
#' @export
validate_latent_space_process_similarity <- function(object, process_matrix,
                                                     entity = c("person", "item")) {
  if (!inherits(object, "eye_latent_space_irt")) stop("`object` must be an eye_latent_space_irt.", call. = FALSE)
  entity <- match.arg(entity)
  coord <- if (entity == "person") object$person_coordinates else object$item_coordinates
  P <- as.matrix(process_matrix)
  if (nrow(P) != nrow(coord)) stop("`process_matrix` rows must align with the selected latent-space entities.", call. = FALSE)
  d_latent <- stats::dist(scale(coord))
  d_process <- stats::dist(scale(P))
  rho <- suppressWarnings(stats::cor(as.numeric(d_latent), as.numeric(d_process), method = "spearman", use = "pairwise.complete.obs"))
  structure(list(entity = entity, spearman_distance_correlation = rho,
                 latent_distance = d_latent, process_distance = d_process,
                 interpretation = "Positive distance association supports convergent structure but is not proof of a shared construct."),
            class = "eye_latent_space_process_validation")
}

.ep07_icc <- function(theta, a, b) stats::plogis(outer(theta, a, "*") - rep(a * b, each = length(theta)))

#' Equate IRT scales using anchor item parameters
#'
#' Implements mean-sigma, mean-mean, Stocking-Lord, and Haebara linking for
#' dichotomous 2PL-style item parameters. New-form parameters are transformed
#' onto the reference scale using theta_ref = A * theta_new + B.
#'
#' @param reference Reference-scale parameters or data.
#' @param new New-scale parameters or data.
#' @param method Method used for estimation, linking, or comparison.
#' @param theta_grid Grid of latent-trait values used for evaluation.
#' @export
equate_irt_scales <- function(reference, new,
                               method = c("stocking-lord", "haebara", "mean-sigma", "mean-mean"),
                               theta_grid = seq(-4, 4, length.out = 81)) {
  method <- match.arg(method)
  for (nm in c("reference", "new")) {
    z <- get(nm)
    if (!is.data.frame(z) || !all(c("a", "b") %in% names(z))) stop(sprintf("`%s` must contain columns `a` and `b`.", nm), call. = FALSE)
  }
  if (nrow(reference) != nrow(new)) stop("Reference and new anchor sets must contain the same number of items in corresponding rows.", call. = FALSE)
  if (method == "mean-sigma") {
    A <- stats::sd(reference$b) / stats::sd(new$b)
    B <- mean(reference$b) - A * mean(new$b)
  } else if (method == "mean-mean") {
    A <- mean(new$a) / mean(reference$a)
    B <- mean(reference$b) - A * mean(new$b)
  } else {
    objective <- function(par) {
      A <- exp(par[1L]); B <- par[2L]
      a_t <- new$a / A; b_t <- A * new$b + B
      pref <- .ep07_icc(theta_grid, reference$a, reference$b)
      pnew <- .ep07_icc(theta_grid, a_t, b_t)
      if (method == "stocking-lord") {
        sum((rowSums(pref) - rowSums(pnew))^2)
      } else {
        sum((pref - pnew)^2)
      }
    }
    opt <- stats::optim(c(0, 0), objective, method = "BFGS")
    A <- exp(opt$par[1L]); B <- opt$par[2L]
  }
  transformed <- new
  transformed$a <- new$a / A
  transformed$b <- A * new$b + B
  structure(list(A = A, B = B, method = method, transformed = transformed,
                 reference = reference, new = new,
                 equation = "theta_reference = A * theta_new + B"),
            class = "eye_irt_equating")
}

#' Joint response-process person-fit diagnostic
#'
#' Produces model-discrepancy evidence; it never labels a participant as
#' dishonest, impaired, disengaged, or otherwise psychologically categorized.
#'
#' @param object A fitted eyeprocess model or audit object.
#' @param data Input data frame or compatible tabular object.
#' @param person Person or participant identifier column.
#' @param response_weight Weight assigned to response discrepancy.
#' @param rt_weight Weight assigned to response-time discrepancy.
#' @param process_weight Weight assigned to process discrepancy.
#' @export
process_person_fit <- function(object, data = NULL, person = NULL,
                               response_weight = 1, rt_weight = 1, process_weight = 1) {
  if (!inherits(object, "eye_joint_gaze_rt_irt")) stop("Currently supports eye_joint_gaze_rt_irt objects.", call. = FALSE)
  if (object$engine != "reference") stop("For brms fits use posterior predictive person-fit checks through a custom discrepancy function.", call. = FALSE)
  if (is.null(data)) stop("Supply the data used for the fitted reference model.", call. = FALSE)
  d <- .ep07_as_data_frame(data, "data")
  cols <- object$columns
  if (is.null(person)) person <- cols$person
  .ep07_req_cols(d, unlist(cols), "data")
  p_resp <- stats::predict(object$response_model, newdata = d, type = "response", allow.new.levels = TRUE)
  r_resp <- (d[[cols$response]] - p_resp) / sqrt(pmax(p_resp * (1 - p_resp), 1e-6))
  p_rt <- stats::predict(object$rt_model, newdata = d, allow.new.levels = TRUE)
  rr <- log(pmax(d[[cols$rt]], .Machine$double.eps)) - p_rt
  r_rt <- as.numeric(scale(rr))
  p_gaze <- stats::predict(object$gaze_model, newdata = d, type = "response", allow.new.levels = TRUE)
  r_gaze <- (d[[cols$gaze]] - p_gaze) / sqrt(pmax(p_gaze, 1e-6))
  score_row <- sqrt((response_weight * r_resp^2 + rt_weight * r_rt^2 + process_weight * r_gaze^2) /
                      (response_weight + rt_weight + process_weight))
  groups <- split(seq_len(nrow(d)), d[[person]])
  out <- do.call(rbind, lapply(names(groups), function(id) {
    ii <- groups[[id]]
    data.frame(participant_id = id, n = length(ii),
               response_rms = sqrt(mean(r_resp[ii]^2, na.rm = TRUE)),
               rt_rms = sqrt(mean(r_rt[ii]^2, na.rm = TRUE)),
               process_rms = sqrt(mean(r_gaze[ii]^2, na.rm = TRUE)),
               combined_rms = sqrt(mean(score_row[ii]^2, na.rm = TRUE)),
               stringsAsFactors = FALSE)
  }))
  out$empirical_percentile <- rank(out$combined_rms, ties.method = "average") / nrow(out)
  structure(out, class = c("eye_process_person_fit", "data.frame"))
}

#' Construct a process-data nuisance surrogate for DIF analysis
#'
#' @param data Input data frame or compatible tabular object.
#' @param process_features Names of process-derived features.
#' @param person Person or participant identifier column.
#' @param aggregate Aggregation rule for process features.
#' @export
process_dif_nuisance_surrogate <- function(data, process_features,
                                           person = "participant_id",
                                           aggregate = TRUE) {
  d <- .ep07_model_frame(data, c(person, process_features))
  z <- if (isTRUE(aggregate)) stats::aggregate(d[process_features], list(person_id = d[[person]]), mean, na.rm = TRUE) else d
  X <- scale(as.matrix(z[process_features]))
  X[!is.finite(X)] <- 0
  pc <- if (ncol(X) == 1L) as.numeric(X[, 1L]) else stats::prcomp(X, center = FALSE, scale. = FALSE)$x[, 1L]
  data.frame(person_id = if (aggregate) z$person_id else d[[person]], process_nuisance = pc,
             stringsAsFactors = FALSE)
}

#' Audit DIF before and after process-data adjustment
#'
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param ability Value supplied to `ability`; see Details for its model-specific role.
#' @param group Value supplied to `group`; see Details for its model-specific role.
#' @param item Item identifier, name, or item column.
#' @param process_features Names of process-derived features.
#' @param person Person or participant identifier column.
#' @export
audit_process_adjusted_dif <- function(data,
                                       response = "response", ability,
                                       group, item = "item_id",
                                       process_features,
                                       person = "participant_id") {
  cols <- c(response, ability, group, item, person, process_features)
  d <- .ep07_model_frame(data, cols)
  surrogate <- process_dif_nuisance_surrogate(d, process_features, person = person, aggregate = TRUE)
  names(surrogate)[1L] <- person
  d <- merge(d, surrogate, by = person, all.x = TRUE, sort = FALSE)
  f0 <- stats::as.formula(sprintf("%s ~ %s + factor(%s) * factor(%s)", response, ability, group, item))
  f1 <- stats::as.formula(sprintf("%s ~ %s + process_nuisance + factor(%s) * factor(%s)", response, ability, group, item))
  m0 <- stats::glm(f0, data = d, family = stats::binomial())
  m1 <- stats::glm(f1, data = d, family = stats::binomial())
  coef_extract <- function(m) {
    cf <- stats::coef(m)
    cf[grepl(":", names(cf))]
  }
  c0 <- coef_extract(m0); c1 <- coef_extract(m1)
  alln <- union(names(c0), names(c1))
  tab <- data.frame(term = alln, unadjusted = unname(c0[alln]), adjusted = unname(c1[alln]), stringsAsFactors = FALSE)
  tab$absolute_reduction <- abs(tab$unadjusted) - abs(tab$adjusted)
  structure(list(unadjusted_model = m0, adjusted_model = m1, coefficients = tab,
                 surrogate = surrogate,
                 note = "Process adjustment is a diagnostic nuisance-control analysis; causal explanations of DIF require study-specific evidence."),
            class = "eye_process_adjusted_dif")
}

#' N-gram features from process sequences
#' @param sequence Sequence input.
#' @param n Requested count or n-gram order, depending on context.
#' @param separator Sequence-token separator.
#' @export
process_ngram_features <- function(sequence, n = c(1L, 2L, 3L), separator = ">") {
  if (is.factor(sequence)) sequence <- as.character(sequence)
  if (!is.list(sequence)) {
    if (is.character(sequence) && length(sequence) == 1L) sequence <- list(strsplit(sequence, separator, fixed = TRUE)[[1L]])
    else sequence <- list(sequence)
  }
  n <- sort(unique(as.integer(n[n > 0])))
  grams_one <- function(z) {
    z <- as.character(z)
    out <- character()
    for (k in n) if (length(z) >= k) {
      out <- c(out, vapply(seq_len(length(z) - k + 1L), function(i) paste(z[i:(i + k - 1L)], collapse = separator), character(1)))
    }
    table(out)
  }
  tabs <- lapply(sequence, grams_one)
  vocab <- sort(unique(unlist(lapply(tabs, names))))
  M <- matrix(0, nrow = length(tabs), ncol = length(vocab), dimnames = list(NULL, vocab))
  for (i in seq_along(tabs)) if (length(tabs[[i]])) M[i, names(tabs[[i]])] <- as.numeric(tabs[[i]])
  M
}

#' Low-dimensional embedding of response-process sequences
#'
#' Uses TF-IDF-weighted n-gram features and truncated SVD. This is a transparent
#' classical embedding; sequence autoencoders can be supplied later through an
#' external engine without changing the downstream contract.
#' @param sequence Sequence input.
#' @param n Requested count or n-gram order, depending on context.
#' @param dimensions Number of embedding dimensions.
#' @export
process_sequence_embedding <- function(sequence, n = c(1L, 2L, 3L), dimensions = 5L) {
  X <- process_ngram_features(sequence, n = n)
  if (!ncol(X)) stop("No n-grams could be constructed.", call. = FALSE)
  tf <- X / pmax(rowSums(X), 1)
  idf <- log((nrow(X) + 1) / (colSums(X > 0) + 1)) + 1
  Z <- sweep(tf, 2L, idf, "*")
  sv <- base::svd(Z, nu = min(nrow(Z), dimensions), nv = min(ncol(Z), dimensions))
  k <- min(as.integer(dimensions), length(sv$d))
  emb <- sv$u[, seq_len(k), drop = FALSE] %*% diag(sv$d[seq_len(k)], nrow = k)
  colnames(emb) <- paste0("process_embedding_", seq_len(k))
  structure(emb, class = c("eye_process_sequence_embedding", class(emb)),
            vocabulary = colnames(X), idf = idf)
}

#' Fit an IRT response model augmented by sequence embeddings
#' @param data Input data frame or compatible tabular object.
#' @param sequences Sequence inputs.
#' @param response Response variable or response-column name.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param dimensions Number of embedding dimensions.
#' @param n Requested count or n-gram order, depending on context.
#' @export
fit_response_process_embedding_irt <- function(data, sequences,
                                               response = "response",
                                               person = "participant_id", item = "item_id",
                                               dimensions = 5L, n = c(1L, 2L, 3L)) {
  d <- .ep07_model_frame(data, c(response, person, item))
  emb <- process_sequence_embedding(sequences, n = n, dimensions = dimensions)
  if (nrow(emb) != nrow(d)) stop("`sequences` must align row-for-row with `data`.", call. = FALSE)
  dd <- cbind(d, as.data.frame(emb))
  terms <- colnames(emb)
  if (requireNamespace("lme4", quietly = TRUE)) {
    f <- stats::as.formula(sprintf("%s ~ %s + (1|%s) + (1|%s)", response, paste(terms, collapse = " + "), person, item))
    fit <- lme4::glmer(f, data = dd, family = stats::binomial())
  } else {
    f <- stats::as.formula(sprintf("%s ~ %s + factor(%s) + factor(%s)", response, paste(terms, collapse = " + "), person, item))
    fit <- stats::glm(f, data = dd, family = stats::binomial())
  }
  structure(list(model = fit, embedding = emb, data = dd,
                 status = "experimental-feature-integration"),
            class = "eye_response_process_embedding_irt")
}

#' GPIRT model-criticism interface
#'
#' `external` is the only exact GPIRT path. `spline_reference` fits flexible
#' logistic spline IRFs solely as a nonparametric stress test for conventional
#' logistic IRF shape; it is deliberately not described as a Gaussian process.
#'
#' @param response_matrix Person-by-item response matrix.
#' @param engine Estimation engine.
#' @param external_engine Validated external fitting function.
#' @param spline_df Degrees of freedom for the spline reference model.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
fit_gpirt <- function(response_matrix,
                      engine = c("spline_reference", "external"),
                      external_engine = NULL, spline_df = 5L, ...) {
  engine <- match.arg(engine)
  X <- as.matrix(response_matrix)
  if (engine == "external") {
    if (!is.function(external_engine)) stop("Supply a validated GPIRT fitter through `external_engine`.", call. = FALSE)
    fit <- external_engine(response_matrix = X, ...)
    return(structure(list(model = fit, engine = "external", exact_gpirt = TRUE,
                          status = "experimental-external"), class = "eye_gpirt"))
  }
  p <- rowMeans(X, na.rm = TRUE)
  n <- rowSums(!is.na(X))
  theta <- stats::qlogis(pmin(pmax((p * n + 0.5) / (n + 1), 1e-5), 1 - 1e-5))
  fits <- lapply(seq_len(ncol(X)), function(j) {
    dat <- data.frame(y = X[, j], theta = theta)
    stats::glm(y ~ splines::ns(theta, df = spline_df), data = dat, family = stats::binomial())
  })
  names(fits) <- colnames(X) %||% paste0("item_", seq_len(ncol(X)))
  structure(list(response_matrix = X, models = fits, theta_proxy = theta, engine = "spline_reference",
                 exact_gpirt = FALSE, status = "experimental-model-criticism",
                 note = "Flexible spline IRFs are a surrogate shape audit, not a Gaussian-process IRT estimator."),
            class = "eye_gpirt")
}

#' Compare conventional logistic and flexible IRF shapes
#' @param response_matrix Person-by-item response matrix.
#' @param gpirt_object Value supplied to `gpirt_object`; see Details for its model-specific role.
#' @param theta_grid Grid of latent-trait values used for evaluation.
#' @export
compare_parametric_nonparametric_irf <- function(response_matrix, gpirt_object = NULL,
                                                 theta_grid = seq(-4, 4, length.out = 101)) {
  X <- as.matrix(response_matrix)
  if (is.null(gpirt_object)) gpirt_object <- fit_gpirt(X, engine = "spline_reference")
  if (!inherits(gpirt_object, "eye_gpirt") || gpirt_object$engine != "spline_reference") {
    stop("This comparison currently requires a spline-reference eye_gpirt object.", call. = FALSE)
  }
  theta <- gpirt_object$theta_proxy
  rows <- list()
  for (j in seq_len(ncol(X))) {
    dat <- data.frame(y = X[, j], theta = theta)
    lin <- stats::glm(y ~ theta, data = dat, family = stats::binomial())
    p_param <- stats::predict(lin, newdata = data.frame(theta = theta_grid), type = "response")
    p_flex <- stats::predict(gpirt_object$models[[j]], newdata = data.frame(theta = theta_grid), type = "response")
    rows[[j]] <- data.frame(item = names(gpirt_object$models)[j], theta = theta_grid,
                            parametric = p_param, flexible = p_flex,
                            absolute_difference = abs(p_param - p_flex), stringsAsFactors = FALSE)
  }
  structure(do.call(rbind, rows), class = c("eye_irf_comparison", "data.frame"))
}

#' Audit item response-function shape departures
#' @param comparison Value supplied to `comparison`; see Details for its model-specific role.
#' @param mean_absolute_threshold Threshold for mean absolute IRF departure.
#' @param max_absolute_threshold Threshold for maximum absolute IRF departure.
#' @export
audit_irf_shape <- function(comparison, mean_absolute_threshold = 0.05,
                            max_absolute_threshold = 0.15) {
  if (!inherits(comparison, "eye_irf_comparison")) stop("Use compare_parametric_nonparametric_irf().", call. = FALSE)
  split_c <- split(comparison, comparison$item)
  out <- do.call(rbind, lapply(names(split_c), function(it) {
    z <- split_c[[it]]$absolute_difference
    data.frame(item = it, mean_absolute_difference = mean(z, na.rm = TRUE),
               max_absolute_difference = max(z, na.rm = TRUE),
               flag = mean(z, na.rm = TRUE) > mean_absolute_threshold || max(z, na.rm = TRUE) > max_absolute_threshold,
               stringsAsFactors = FALSE)
  }))
  out
}

#' Dynamic GPIRT external-engine gate
#' @param data Input data frame or compatible tabular object.
#' @param external_engine Validated external fitting function.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
fit_dynamic_gpirt <- function(data, external_engine = NULL, ...) {
  if (!is.function(external_engine)) {
    stop("Dynamic GPIRT is experimental and no approximate production estimator is bundled. Supply a validated `external_engine`.", call. = FALSE)
  }
  structure(list(model = external_engine(data = data, ...), engine = "external",
                 status = "experimental-gated"), class = "eye_dynamic_gpirt")
}

#' Continuous-time IRT external-engine gate
#' @param data Input data frame or compatible tabular object.
#' @param external_engine Validated external fitting function.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
fit_continuous_time_irt <- function(data, external_engine = NULL, ...) {
  if (!is.function(external_engine)) {
    stop("Continuous-time IRT requires a validated external engine (for example a study-specific lcmm implementation).", call. = FALSE)
  }
  structure(list(model = external_engine(data = data, ...), status = "experimental-gated"),
            class = "eye_continuous_time_irt")
}

#' Estimate a descriptive continuous-time latent trajectory
#'
#' @param time Time values.
#' @param theta Latent-trait values.
#' @param spar Value supplied to `spar`; see Details for its model-specific role.
#' @export
latent_trait_trajectory <- function(time, theta, spar = NULL) {
  ok <- is.finite(time) & is.finite(theta)
  if (sum(ok) < 4L) stop("At least four finite time/theta pairs are required.", call. = FALSE)
  fit <- stats::smooth.spline(time[ok], theta[ok], spar = spar)
  structure(list(model = fit, time = time[ok], theta = theta[ok]), class = "eye_latent_trait_trajectory")
}

#' Predict a latent trait at arbitrary times
#' @param object A fitted eyeprocess model or audit object.
#' @param time Time values.
#' @export
predict_theta_at_time <- function(object, time) {
  if (!inherits(object, "eye_latent_trait_trajectory")) stop("`object` must come from latent_trait_trajectory().", call. = FALSE)
  stats::predict(object$model, x = time)
}

#' Flow-MIRT external-engine gate
#' @param response_matrix Person-by-item response matrix.
#' @param external_engine Validated external fitting function.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
fit_flow_mirt <- function(response_matrix, external_engine = NULL, ...) {
  if (!is.function(external_engine)) {
    stop("Flow-MIRT remains a 2026 experimental method. eyeprocess intentionally bundles no unvalidated production implementation; supply a validated external engine.", call. = FALSE)
  }
  structure(list(model = external_engine(response_matrix = response_matrix, ...),
                 status = "experimental-gated", engine = "external"), class = "eye_flow_mirt")
}

#' Variational IRT external-engine gate
#' @param response_matrix Person-by-item response matrix.
#' @param external_engine Validated external fitting function.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @export
fit_variational_irt <- function(response_matrix, external_engine = NULL, ...) {
  if (!is.function(external_engine)) {
    stop("Supply a validated variational IRT engine. The eyeprocess API gate prevents an approximate estimator from being silently presented as validated IRT.", call. = FALSE)
  }
  structure(list(model = external_engine(response_matrix = response_matrix, ...),
                 status = "experimental-gated", engine = "external"), class = "eye_variational_irt")
}

#' 2PL response item information
#' @param theta Latent-trait values.
#' @param a Item discrimination parameter or parameters.
#' @param b Item difficulty/location parameter or parameters.
#' @param process_information Information supplied by the process channel.
#' @param rt_information Information supplied by response time.
#' @param weights Weights used to combine information components.
#' @param expected_time Expected response time or burden.
#' @param burden_weight Penalty applied to expected burden.
#' @export
process_item_information <- function(theta, a, b,
                                     process_information = 0,
                                     rt_information = 0,
                                     weights = c(response = 1, rt = 0, process = 0),
                                     expected_time = 0, burden_weight = 0) {
  theta <- as.numeric(theta); a <- as.numeric(a); b <- as.numeric(b)
  if (length(a) != length(b)) stop("`a` and `b` must have equal length.", call. = FALSE)
  p <- stats::plogis(outer(theta, a, "*") - rep(a * b, each = length(theta)))
  response_info <- sweep(p * (1 - p), 2L, a^2, "*")
  proc <- rep(process_information, length.out = length(a))
  rti <- rep(rt_information, length.out = length(a))
  et <- rep(expected_time, length.out = length(a))
  utility <- weights["response"] * response_info +
    matrix(weights["rt"] * rti + weights["process"] * proc - burden_weight * et,
           nrow = length(theta), ncol = length(a), byrow = TRUE)
  colnames(utility) <- names(a) %||% paste0("item_", seq_along(a))
  structure(list(theta = theta, response_information = response_info, utility = utility),
            class = "eye_process_item_information")
}

#' Expected process-aware item utility under a theta distribution
#' @param info Value supplied to `info`; see Details for its model-specific role.
#' @param theta_weights Weights over the theta distribution.
#' @export
expected_process_information <- function(info, theta_weights = NULL) {
  if (!inherits(info, "eye_process_item_information")) stop("`info` must come from process_item_information().", call. = FALSE)
  if (is.null(theta_weights)) theta_weights <- rep(1 / length(info$theta), length(info$theta))
  theta_weights <- theta_weights / sum(theta_weights)
  drop(crossprod(theta_weights, info$utility))
}

#' Select the next item using response/process utility
#' @param theta Latent-trait values.
#' @param item_bank Value supplied to `item_bank`; see Details for its model-specific role.
#' @param used Items already used or unavailable for selection.
#' @param weights Weights used to combine information components.
#' @param burden_weight Penalty applied to expected burden.
#' @export
select_next_item_process <- function(theta, item_bank,
                                     used = character(),
                                     weights = c(response = 1, rt = 0, process = 0),
                                     burden_weight = 0) {
  if (!is.data.frame(item_bank) || !all(c("item_id", "a", "b") %in% names(item_bank))) {
    stop("`item_bank` must contain item_id, a, and b.", call. = FALSE)
  }
  avail <- item_bank[!item_bank$item_id %in% used, , drop = FALSE]
  if (!nrow(avail)) stop("No unused items remain.", call. = FALSE)
  pi <- if ("process_information" %in% names(avail)) avail$process_information else 0
  ri <- if ("rt_information" %in% names(avail)) avail$rt_information else 0
  et <- if ("expected_time" %in% names(avail)) avail$expected_time else 0
  info <- process_item_information(theta, avail$a, avail$b, pi, ri, weights = weights,
                                   expected_time = et, burden_weight = burden_weight)
  u <- as.numeric(info$utility[1L, ])
  j <- which.max(u)
  list(item_id = avail$item_id[j], utility = u[j], row = avail[j, , drop = FALSE],
       all_utilities = data.frame(item_id = avail$item_id, utility = u))
}

#' Simulate a simple process-aware CAT policy
#'
#' This is a design simulator, not a production testing engine.
#' @param item_bank Value supplied to `item_bank`; see Details for its model-specific role.
#' @param true_theta Simulated true latent-trait value or values.
#' @param n_items Number of items.
#' @param weights Weights used to combine information components.
#' @param burden_weight Penalty applied to expected burden.
#' @param seed Random-number seed.
#' @export
simulate_process_cat <- function(item_bank, true_theta = 0, n_items = 10L,
                                 weights = c(response = 1, rt = 0, process = 0),
                                 burden_weight = 0, seed = 1) {
  set.seed(seed)
  theta_hat <- 0; used <- character(); rows <- list()
  for (step in seq_len(min(n_items, nrow(item_bank)))) {
    sel <- select_next_item_process(theta_hat, item_bank, used = used, weights = weights,
                                    burden_weight = burden_weight)
    it <- sel$row
    p <- stats::plogis(it$a * (true_theta - it$b))
    y <- stats::rbinom(1, 1, p)
    used <- c(used, as.character(it$item_id))
    # One-step bounded ML/grid update for transparency and stability.
    grid <- seq(-4, 4, length.out = 321)
    sub <- item_bank[match(used, item_bank$item_id), , drop = FALSE]
    yy <- c(vapply(rows, function(z) z$response, numeric(1)), y)
    ll <- vapply(grid, function(th) {
      pp <- stats::plogis(sub$a * (th - sub$b))
      sum(stats::dbinom(yy, 1, pp, log = TRUE))
    }, numeric(1))
    theta_hat <- grid[which.max(ll)]
    rows[[step]] <- data.frame(step = step, item_id = it$item_id, response = y,
                               selection_utility = sel$utility, theta_hat = theta_hat,
                               true_theta = true_theta, stringsAsFactors = FALSE)
  }
  structure(do.call(rbind, rows), class = c("eye_process_cat_simulation", "data.frame"))
}
