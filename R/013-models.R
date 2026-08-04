response_matrix <- function(x, value = c("score", "response"), duplicate = c("error", "last", "mean")) {
  .assert_eye_dataset(x)
  value <- match.arg(value); duplicate <- match.arg(duplicate)
  d <- x$responses
  if (!nrow(d)) .eye_stop("No responses are available.")
  d <- d[!is.na(d$participant_id) & !is.na(d$item_id), ]
  key <- paste(d$participant_id, d$item_id, sep = "\r")
  if (anyDuplicated(key)) {
    if (duplicate == "error") .eye_stop("Duplicate participant-item responses detected.")
    if (duplicate == "last") d <- d[!duplicated(key, fromLast = TRUE), ]
    if (duplicate == "mean") {
      if (!is.numeric(d[[value]])) .eye_stop("`duplicate = \"mean\"` requires a numeric response value.")
      d <- stats::aggregate(d[[value]], d[c("participant_id", "item_id")], mean, na.rm = TRUE)
      names(d)[3L] <- value
    }
  }
  persons <- sort(unique(d$participant_id)); items <- sort(unique(d$item_id))
  mat <- matrix(NA_real_, nrow = length(persons), ncol = length(items), dimnames = list(persons, items))
  i <- match(d$participant_id, persons); j <- match(d$item_id, items)
  mat[cbind(i, j)] <- if (value == "score") .safe_numeric(d[[value]]) else as.numeric(factor(d[[value]])) - 1L
  mat
}

response_time_matrix <- function(x, log_transform = FALSE, duplicate = c("error", "last", "mean")) {
  .assert_eye_dataset(x)
  duplicate <- match.arg(duplicate)
  d <- x$responses
  d <- d[is.finite(d$response_time) & d$response_time > 0 & !is.na(d$participant_id) & !is.na(d$item_id), ]
  if (!nrow(d)) .eye_stop("No positive response times are available.")
  key <- paste(d$participant_id, d$item_id, sep = "\r")
  if (anyDuplicated(key)) {
    if (duplicate == "error") .eye_stop("Duplicate participant-item response times detected.")
    if (duplicate == "last") d <- d[!duplicated(key, fromLast = TRUE), ]
    if (duplicate == "mean") d <- stats::aggregate(response_time ~ participant_id + item_id, d, mean)
  }
  persons <- sort(unique(d$participant_id)); items <- sort(unique(d$item_id))
  mat <- matrix(NA_real_, nrow = length(persons), ncol = length(items), dimnames = list(persons, items))
  mat[cbind(match(d$participant_id, persons), match(d$item_id, items))] <- d$response_time
  if (log_transform) mat <- log(mat)
  mat
}

align_response_matrices <- function(Y, RT) {
  persons <- intersect(rownames(Y), rownames(RT)); items <- intersect(colnames(Y), colnames(RT))
  if (!length(persons) || !length(items)) .eye_stop("Response and response-time matrices have no common persons/items.")
  list(Y = Y[persons, items, drop = FALSE], RT = RT[persons, items, drop = FALSE])
}

model_data <- function(x, include_features = TRUE, aggregate_features = mean) {
  .assert_eye_dataset(x)
  d <- x$responses
  if (!nrow(d)) .eye_stop("No responses available.")
  if (include_features && nrow(x$features)) {
    fw <- features_wide(x, id_cols = c("recording_id", "participant_id", "trial_id", "item_id"), aggregate = aggregate_features)
    keys <- intersect(c("recording_id", "participant_id", "trial_id", "item_id"), names(fw))
    d <- merge(d, fw, by = keys, all.x = TRUE, sort = FALSE)
  }
  d$participant_id <- factor(d$participant_id)
  d$item_id <- factor(d$item_id)
  d
}

.new_eyeprocess_model <- function(fit, engine, model_type, data, call, metadata = list(), experimental = FALSE) {
  structure(list(
    fit = fit, engine = engine, model_type = model_type, data = data,
    call = call, metadata = metadata, experimental = isTRUE(experimental),
    fitted_at = .now_utc()
  ), class = "eyeprocess_model")
}

print.eyeprocess_model <- function(x, ...) {
  cat("<eyeprocess_model>\n")
  cat("  Type:         ", x$model_type, "\n", sep = "")
  cat("  Engine:       ", x$engine, "\n", sep = "")
  cat("  Observations: ", if (is.data.frame(x$data)) nrow(x$data) else length(x$data), "\n", sep = "")
  cat("  Experimental: ", x$experimental, "\n", sep = "")
  invisible(x)
}

summary.eyeprocess_model <- function(object, ...) {
  fit_summary <- tryCatch(summary(object$fit, ...), error = function(e) e$message)
  structure(list(
    model_type = object$model_type, engine = object$engine,
    experimental = object$experimental, metadata = object$metadata,
    fit_summary = fit_summary
  ), class = "summary.eyeprocess_model")
}

print.summary.eyeprocess_model <- function(x, ...) {
  cat("eyeprocess model summary\n")
  cat("Type:   ", x$model_type, "\n", sep = "")
  cat("Engine: ", x$engine, "\n", sep = "")
  if (x$experimental) cat("Status: EXPERIMENTAL\n")
  print(x$fit_summary)
  invisible(x)
}

fit_irt <- function(
    x,
    engine = c("mirt", "TAM", "rasch_glm"),
    model = 1,
    itemtype = "2PL",
    value = "score",
    ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  Y <- response_matrix(x, value = value)
  if (engine == "mirt") {
    .require_namespace("mirt", "for the `mirt` IRT engine")
    fit <- mirt::mirt(data = Y, model = model, itemtype = itemtype, ...)
  } else if (engine == "TAM") {
    .require_namespace("TAM", "for the `TAM` IRT engine")
    fit <- if (itemtype %in% c("2PL", "3PL")) TAM::tam.mml.2pl(resp = Y, ...) else TAM::tam.mml(resp = Y, ...)
  } else {
    d <- x$responses[is.finite(x$responses$score), ]
    d$participant_id <- factor(d$participant_id); d$item_id <- factor(d$item_id)
    fit <- stats::glm(score ~ 0 + participant_id + item_id, family = stats::binomial(), data = d, ...)
  }
  .new_eyeprocess_model(fit, engine, "IRT", Y, match.call(), list(model = model, itemtype = itemtype, value = value, approximation = engine == "rasch_glm"))
}

fit_explanatory_irt <- function(
    x,
    formula,
    engine = c("lme4", "glm", "brms"),
    participant_random = TRUE,
    item_random = TRUE,
    family = "binomial",
    ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  d <- model_data(x, include_features = TRUE)
  response <- all.vars(formula)[1L]
  if (!response %in% names(d)) .eye_stop("Formula response `", response, "` is not available in model data.")
  if (engine == "lme4") {
    .require_namespace("lme4", "for explanatory mixed-effects IRT")
    rhs <- paste(deparse(formula[[3L]]), collapse = "")
    if (participant_random) rhs <- paste(rhs, "+ (1 | participant_id)")
    if (item_random) rhs <- paste(rhs, "+ (1 | item_id)")
    mixed_formula <- stats::as.formula(paste(response, "~", rhs), env = environment(formula))
    fam <- if (is.character(family)) get(family, mode = "function", envir = asNamespace("stats"))() else family
    fit <- lme4::glmer(mixed_formula, data = d, family = fam, ...)
    used_formula <- mixed_formula
  } else if (engine == "brms") {
    .require_namespace("brms", "for Bayesian explanatory IRT")
    rhs <- paste(deparse(formula[[3L]]), collapse = "")
    if (participant_random) rhs <- paste(rhs, "+ (1 | participant_id)")
    if (item_random) rhs <- paste(rhs, "+ (1 | item_id)")
    used_formula <- stats::as.formula(paste(response, "~", rhs), env = environment(formula))
    fam <- if (is.character(family)) family else family$family
    fit <- brms::brm(used_formula, data = d, family = fam, ...)
  } else {
    rhs <- paste(deparse(formula[[3L]]), collapse = "")
    if (participant_random) rhs <- paste(rhs, "+ participant_id")
    if (item_random) rhs <- paste(rhs, "+ item_id")
    used_formula <- stats::as.formula(paste(response, "~", rhs), env = environment(formula))
    fam <- if (is.character(family)) get(family, mode = "function", envir = asNamespace("stats"))() else family
    fit <- stats::glm(used_formula, data = d, family = fam, ...)
  }
  .new_eyeprocess_model(
    fit, engine, "explanatory_irt", d, match.call(),
    list(formula = used_formula, participant_random = participant_random, item_random = item_random,
      warning = if (engine == "glm") "Participant and item effects are fixed, not random." else NULL)
  )
}

fit_accuracy_rt <- function(
    x,
    engine = c("LNIRT", "two_stage"),
    iterations = 1000,
    burnin = 10,
    residual = FALSE,
    ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  Y <- response_matrix(x, value = "score")
  RT <- response_time_matrix(x, log_transform = TRUE)
  aligned <- align_response_matrices(Y, RT); Y <- aligned$Y; RT <- aligned$RT
  if (engine == "LNIRT") {
    .require_namespace("LNIRT", "for joint response-accuracy/response-time modelling")
    fit <- LNIRT::LNIRT(Y = Y, RT = RT, XG = iterations, burnin = burnin, resid = residual, ...)
  } else {
    irt <- fit_irt(x, engine = "rasch_glm")
    d <- model_data(x, include_features = FALSE)
    d <- d[is.finite(d$response_time) & d$response_time > 0, ]
    d$log_rt <- log(d$response_time)
    rt_fit <- stats::lm(log_rt ~ participant_id + item_id, data = d)
    ability_proxy <- tapply(d$score, d$participant_id, mean, na.rm = TRUE)
    speed_proxy <- -tapply(d$log_rt, d$participant_id, mean, na.rm = TRUE)
    common <- intersect(names(ability_proxy), names(speed_proxy))
    fit <- list(irt = irt, response_time = rt_fit,
      ability_speed_correlation = stats::cor(ability_proxy[common], speed_proxy[common], use = "complete.obs"))
    class(fit) <- "eye_two_stage_rt"
  }
  .new_eyeprocess_model(fit, engine, "accuracy_response_time", list(Y = Y, RT = RT), match.call(),
    list(iterations = iterations, RT_is_log = TRUE, joint = engine == "LNIRT"))
}

print.eye_two_stage_rt <- function(x, ...) {
  cat("Two-stage accuracy/response-time model\n")
  cat("Ability-speed proxy correlation: ", format(x$ability_speed_correlation, digits = 4), "\n", sep = "")
  invisible(x)
}

fit_dif <- function(x, group, engine = c("logistic", "mirt"), items = NULL, ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  d <- model_data(x, include_features = FALSE)
  if (length(group) == nrow(d)) d$.group <- factor(group) else {
    if (is.character(group) && length(group) == 1L && group %in% names(d)) d$.group <- factor(d[[group]]) else .eye_stop("`group` must be a model-data column name or a vector with one value per response row.")
  }
  if (engine == "mirt") {
    .require_namespace("mirt", "for multiple-group DIF")
    Y <- response_matrix(x)
    person_groups <- unique(d[c("participant_id", ".group")])
    g <- person_groups$.group[match(rownames(Y), person_groups$participant_id)]
    fit <- mirt::multipleGroup(Y, 1, group = g, ...)
    result <- list(model = fit, dif = if (!is.null(items)) mirt::DIF(fit, which.par = c("a1", "d"), items2test = items) else NULL)
  } else {
    d$total_score <- ave(d$score, d$participant_id, FUN = function(z) sum(z, na.rm = TRUE)) - d$score
    item_levels <- if (is.null(items)) levels(d$item_id) else items
    result <- lapply(item_levels, function(item) {
      z <- d[d$item_id == item & is.finite(d$score), ]
      if (nrow(z) < 10L || length(unique(z$.group)) < 2L) return(data.frame(item_id = item, status = "insufficient_data"))
      fit0 <- stats::glm(score ~ total_score, family = stats::binomial(), data = z)
      fit1 <- stats::glm(score ~ total_score + .group + total_score:.group, family = stats::binomial(), data = z)
      test <- stats::anova(fit0, fit1, test = "Chisq")
      data.frame(item_id = item, chisq = test$Deviance[2L], df = test$Df[2L], p_value = test$`Pr(>Chi)`[2L], status = "estimated", stringsAsFactors = FALSE)
    })
    result <- do.call(.bind_rows_base, result)
  }
  .new_eyeprocess_model(result, engine, "DIF", d, match.call(), list(group = group))
}

fit_shared_process_factor <- function(
    x,
    features,
    n_factors = 1L,
    center = TRUE,
    scale. = TRUE,
    append = TRUE,
    prefix = "process_factor") {
  .assert_eye_dataset(x)
  wide <- features_wide(x, id_cols = c("recording_id", "participant_id", "trial_id", "item_id"))
  missing <- setdiff(features, names(wide))
  if (length(missing)) .eye_stop("Requested process feature(s) absent: ", paste(missing, collapse = ", "), ".")
  complete <- stats::complete.cases(wide[features])
  if (sum(complete) <= n_factors) .eye_stop("Insufficient complete rows for factor extraction.")
  fit <- stats::prcomp(wide[complete, features, drop = FALSE], center = center, scale. = scale., rank. = n_factors)
  scores <- matrix(NA_real_, nrow(wide), ncol = n_factors)
  scores[complete, ] <- fit$x[, seq_len(n_factors), drop = FALSE]
  feature_rows <- list(); k <- 0L
  for (j in seq_len(n_factors)) {
    for (i in seq_len(nrow(wide))) {
      k <- k + 1L
      base <- as.list(wide[i, intersect(c("recording_id", "participant_id", "trial_id", "item_id"), names(wide)), drop = FALSE])
      base$stimulus_id <- NA_character_; base$aoi_id <- NA_character_
      vals <- setNames(scores[i, j], paste0(prefix, "_", j))
      feature_rows[[k]] <- .feature_rows(base, vals, "standardized_score", "trial", "principal_components",
        parameters = paste0("inputs=", paste(features, collapse = ",")))
    }
  }
  if (append) x$features <- standardize_eye_table(.bind_rows_base(x$features, do.call(.bind_rows_base, feature_rows)), "features")
  model <- .new_eyeprocess_model(fit, "stats::prcomp", "shared_process_factor", wide, match.call(),
    list(features = features, n_factors = n_factors, neutral_label = TRUE))
  list(data = x, model = model)
}

item_parameters <- function(model, ...) {
  if (!inherits(model, "eyeprocess_model")) .eye_stop("Expected an `eyeprocess_model`.")
  if (model$engine == "mirt") {
    co <- mirt::coef(model$fit, IRTpars = TRUE, simplify = TRUE)
    items <- co$items
    return(data.frame(item_id = rownames(items), items, row.names = NULL, check.names = FALSE))
  }
  if (model$engine == "TAM") {
    fit <- model$fit
    if (!is.null(fit$item)) return(data.frame(item_id = rownames(fit$item), fit$item, row.names = NULL, check.names = FALSE))
  }
  if (model$engine == "rasch_glm") {
    cf <- stats::coef(model$fit); idx <- grepl("^item_id", names(cf))
    return(data.frame(item_id = sub("^item_id", "", names(cf)[idx]), difficulty = -unname(cf[idx]), stringsAsFactors = FALSE))
  }
  data.frame()
}

person_scores <- function(model, ...) {
  if (!inherits(model, "eyeprocess_model")) .eye_stop("Expected an `eyeprocess_model`.")
  if (model$engine == "mirt") {
    fs <- mirt::fscores(model$fit, ...)
    return(data.frame(participant_id = rownames(fs), fs, row.names = NULL, check.names = FALSE))
  }
  if (model$engine == "TAM") {
    wle <- TAM::tam.wle(model$fit, ...)
    return(data.frame(participant_id = rownames(wle$theta), theta = as.numeric(wle$theta), stringsAsFactors = FALSE))
  }
  if (model$engine == "rasch_glm") {
    cf <- stats::coef(model$fit); idx <- grepl("^participant_id", names(cf))
    return(data.frame(participant_id = sub("^participant_id", "", names(cf)[idx]), theta = unname(cf[idx]), stringsAsFactors = FALSE))
  }
  data.frame()
}

model_fit_statistics <- function(model) {
  if (!inherits(model, "eyeprocess_model")) .eye_stop("Expected an `eyeprocess_model`.")
  fit <- model$fit
  data.frame(
    engine = model$engine, model_type = model$model_type,
    logLik = tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_),
    AIC = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    BIC = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    nobs = tryCatch(stats::nobs(fit), error = function(e) if (is.data.frame(model$data)) nrow(model$data) else NA_real_),
    stringsAsFactors = FALSE
  )
}

predict.eyeprocess_model <- function(object, newdata = NULL, ...) {
  if (object$engine %in% c("glm", "lme4", "brms", "rasch_glm") || inherits(object$fit, c("glm", "lm", "merMod"))) {
    return(stats::predict(object$fit, newdata = newdata, ...))
  }
  if (object$engine == "mirt") return(mirt::fscores(object$fit, ...))
  .eye_stop("Prediction is not implemented for engine `", object$engine, "`.")
}

check_local_dependence <- function(model, ...) {
  if (!inherits(model, "eyeprocess_model")) .eye_stop("Expected an `eyeprocess_model`.")
  if (model$engine == "mirt") return(mirt::residuals(model$fit, type = "Q3", ...))
  .eye_warn("Local-dependence diagnostics currently require the `mirt` engine.")
  NULL
}

fit_joint_process_model <- function(
    x,
    accuracy_formula,
    rt_formula,
    process_formulas = NULL,
    engine = c("brms", "separate"),
    ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  d <- model_data(x, include_features = TRUE)
  d$log_response_time <- log(d$response_time)
  if (engine == "brms") {
    .require_namespace("brms", "for multivariate joint process models")
    bf_acc <- brms::bf(accuracy_formula, family = brms::bernoulli())
    bf_rt <- brms::bf(rt_formula, family = stats::gaussian())
    bfs <- list(bf_acc, bf_rt)
    if (!is.null(process_formulas)) {
      bfs <- c(bfs, lapply(process_formulas, function(f) brms::bf(f, family = stats::gaussian())))
    }
    mv <- Reduce(`+`, bfs) + brms::set_rescor(FALSE)
    fit <- brms::brm(mv, data = d, ...)
  } else {
    acc <- stats::glm(accuracy_formula, data = d, family = stats::binomial())
    rt <- stats::lm(rt_formula, data = d)
    proc <- if (is.null(process_formulas)) list() else lapply(process_formulas, stats::lm, data = d)
    fit <- list(accuracy = acc, response_time = rt, process = proc)
    class(fit) <- "eye_separate_process_models"
  }
  .new_eyeprocess_model(fit, engine, "joint_process_model", d, match.call(),
    list(accuracy_formula = accuracy_formula, rt_formula = rt_formula, process_formulas = process_formulas,
      estimand_warning = if (engine == "separate") "Separate models do not propagate cross-outcome uncertainty." else NULL),
    experimental = TRUE)
}

fit_dynamic_aoi_model <- function(x, source = c("visits", "fixations", "samples"), smoothing = 0.5) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  counts <- transition_matrix(x, normalize = "none", source = source)
  if (!length(counts)) .eye_stop("No AOI transitions are available.")
  probabilities <- sweep(counts + smoothing, 1L, rowSums(counts + smoothing), "/")
  fit <- list(counts = counts, probabilities = probabilities, smoothing = smoothing)
  class(fit) <- "eye_dynamic_aoi"
  .new_eyeprocess_model(fit, "empirical_markov", "dynamic_aoi", scanpath_sequence(x, source = source), match.call(),
    list(source = source, smoothing = smoothing, order = 1L), experimental = TRUE)
}

print.eye_dynamic_aoi <- function(x, ...) {
  cat("First-order AOI Markov model\n")
  print(round(x$probabilities, 3))
  invisible(x)
}
