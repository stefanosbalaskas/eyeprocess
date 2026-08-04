process_irt_spec <- function(
    response = "score",
    response_time = "response_time",
    gaze_features = character(),
    pupil_features = character(),
    biometric_features = character(),
    participant_effect = TRUE,
    item_effect = TRUE,
    estimand = "association",
    confirmatory = FALSE) {
  structure(list(
    response = response, response_time = response_time,
    gaze_features = gaze_features, pupil_features = pupil_features,
    biometric_features = biometric_features,
    participant_effect = isTRUE(participant_effect), item_effect = isTRUE(item_effect),
    estimand = estimand, confirmatory = isTRUE(confirmatory)
  ), class = "process_irt_spec")
}

print.process_irt_spec <- function(x, ...) {
  cat("<process_irt_spec>\n")
  cat("  Response: ", x$response, "\n", sep = "")
  cat("  Process predictors: ", paste(c(x$gaze_features, x$pupil_features, x$biometric_features), collapse = ", "), "\n", sep = "")
  cat("  Estimand: ", x$estimand, "\n", sep = "")
  cat("  Mode: ", if (x$confirmatory) "confirmatory" else "exploratory", "\n", sep = "")
  invisible(x)
}

fit_process_irt <- function(
    x,
    spec,
    engine = c("lme4", "glm", "brms"),
    ...) {
  if (!inherits(spec, "process_irt_spec")) .eye_stop("`spec` must be created with `process_irt_spec()`.")
  predictors <- c(spec$gaze_features, spec$pupil_features, spec$biometric_features)
  rhs <- if (length(predictors)) paste(predictors, collapse = " + ") else "1"
  formula <- stats::as.formula(paste(spec$response, "~", rhs))
  fit_explanatory_irt(
    x, formula = formula, engine = match.arg(engine),
    participant_random = spec$participant_effect,
    item_random = spec$item_effect, ...
  )
}

fit_gaze_informed_irt <- function(x, response = "score", gaze_features, engine = c("lme4", "glm", "brms"), ...) {
  spec <- process_irt_spec(response = response, gaze_features = gaze_features)
  fit_process_irt(x, spec, engine = match.arg(engine), ...)
}

fit_pupil_informed_irt <- function(x, response = "score", pupil_features, engine = c("lme4", "glm", "brms"), ...) {
  spec <- process_irt_spec(response = response, pupil_features = pupil_features)
  fit_process_irt(x, spec, engine = match.arg(engine), ...)
}

fit_multimodal_irt <- function(x, response = "score", gaze_features = character(), pupil_features = character(), biometric_features = character(), engine = c("lme4", "glm", "brms"), ...) {
  spec <- process_irt_spec(response, gaze_features = gaze_features, pupil_features = pupil_features, biometric_features = biometric_features)
  fit_process_irt(x, spec, engine = match.arg(engine), ...)
}

process_irt_diagnostics <- function(model) {
  if (!inherits(model, "eyeprocess_model")) .eye_stop("Expected an `eyeprocess_model`.")
  list(
    fit = model_fit_statistics(model),
    local_dependence = tryCatch(check_local_dependence(model), error = function(e) NULL),
    warnings = c(
      if (model$experimental) "Model is marked experimental.",
      model$metadata$warning,
      model$metadata$estimand_warning
    )
  )
}

functional_pupil_features <- function(
    x,
    df = 5L,
    grid_points = 100L,
    append = TRUE,
    prefix = "pupil_basis") {
  .assert_eye_dataset(x)
  d <- x$eye_samples
  trials <- trial_table(x)
  if (!nrow(d) || !nrow(trials)) return(if (append) x else data.frame())
  groups <- .group_split(d[!is.na(d$trial_id), ], c("recording_id", "trial_id", "eye"))
  rows <- list(); k <- 0L
  for (z in groups) {
    tr <- trials[trials$recording_id == z$recording_id[1L] & trials$trial_id == z$trial_id[1L], ]
    if (!nrow(tr)) next
    ok <- is.finite(z$timestamp_seconds) & is.finite(z$pupil_diameter)
    if (sum(ok) < df + 2L) next
    rel <- (z$timestamp_seconds[ok] - tr$start_time[1L]) / (tr$end_time[1L] - tr$start_time[1L])
    basis <- splines::ns(rel, df = df)
    fit <- stats::lm.fit(cbind(1, basis), z$pupil_diameter[ok])
    coef <- fit$coefficients[-1L]
    names(coef) <- paste0(prefix, "_", seq_along(coef))
    base <- list(recording_id = z$recording_id[1L], participant_id = tr$participant_id[1L],
      trial_id = z$trial_id[1L], item_id = tr$item_id[1L], stimulus_id = tr$stimulus_id[1L], aoi_id = NA_character_)
    k <- k + 1L
    rows[[k]] <- .feature_rows(base, coef, "basis_coefficient", "trial_eye", "natural_spline_pupil",
      parameters = paste0("df=", df, ";eye=", z$eye[1L]), observed_fraction = mean(ok))
  }
  f <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("features")
  if (!append) return(f)
  x$features <- standardize_eye_table(.bind_rows_base(x$features, f), "features")
  add_provenance(x, "functional_pupil_features", "features", paste0(nrow(f), " rows;df=", df))
}

fit_strategy_mixture <- function(
    x,
    features,
    centers = 2L,
    response_formula = NULL,
    seed = 1,
    append = TRUE) {
  .assert_eye_dataset(x)
  wide <- features_wide(x, id_cols = c("recording_id", "participant_id", "trial_id", "item_id"))
  missing <- setdiff(features, names(wide))
  if (length(missing)) .eye_stop("Missing strategy feature(s): ", paste(missing, collapse = ", "), ".")
  ok <- stats::complete.cases(wide[features])
  if (sum(ok) < centers * 3L) .eye_stop("Insufficient complete observations for the requested number of strategy clusters.")
  set.seed(seed)
  scaled <- scale(wide[ok, features, drop = FALSE])
  km <- stats::kmeans(scaled, centers = centers)
  wide$strategy_class <- NA_integer_; wide$strategy_class[ok] <- km$cluster
  class_rows <- lapply(seq_len(nrow(wide)), function(i) {
    base <- as.list(wide[i, intersect(c("recording_id", "participant_id", "trial_id", "item_id"), names(wide)), drop = FALSE])
    base$stimulus_id <- NA_character_; base$aoi_id <- NA_character_
    .feature_rows(base, setNames(wide$strategy_class[i], "strategy_class"), "class", "trial", "kmeans_strategy",
      parameters = paste0("features=", paste(features, collapse = ","), ";centers=", centers))
  })
  if (append) x$features <- standardize_eye_table(.bind_rows_base(x$features, do.call(.bind_rows_base, class_rows)), "features")
  response_fit <- NULL
  if (!is.null(response_formula)) {
    d <- merge(model_data(x, include_features = FALSE), wide[c("recording_id", "participant_id", "trial_id", "item_id", "strategy_class")],
      by = c("recording_id", "participant_id", "trial_id", "item_id"), all.x = TRUE)
    response_fit <- stats::glm(response_formula, data = d, family = stats::binomial())
  }
  fit <- list(kmeans = km, response_model = response_fit, features = features)
  class(fit) <- "eye_strategy_mixture"
  list(
    data = x,
    model = .new_eyeprocess_model(fit, "kmeans+glm", "strategy_mixture", wide, match.call(),
      list(centers = centers, construct_warning = "Classes are descriptive and must not be named as cognitive strategies without external validation."), experimental = TRUE)
  )
}

print.eye_strategy_mixture <- function(x, ...) {
  cat("Exploratory process-strategy mixture\n")
  print(x$kmeans)
  invisible(x)
}

estimate_ez_diffusion <- function(
    x,
    accuracy = "score",
    response_time = "response_time",
    by = c("item_id"),
    scale = 0.1) {
  if (is_eye_dataset(x)) d <- x$responses else d <- x
  .assert_data_frame(d, "x")
  .assert_columns(d, c(accuracy, response_time, by))
  groups <- .group_split(d, by)
  out <- lapply(groups, function(z) {
    complete <- is.finite(z[[accuracy]]) & is.finite(z[[response_time]]) & z[[response_time]] > 0
    zc <- z[complete, , drop = FALSE]
    n <- nrow(zc)
    pc_raw <- if (n) mean(zc[[accuracy]]) else NA_real_
    vrt <- if (n > 1L) stats::var(zc[[response_time]]) else NA_real_
    mrt <- if (n) mean(zc[[response_time]]) else NA_real_
    drift <- boundary <- nondecision <- NA_real_
    status <- "ok"
    if (n < 3L || !is.finite(vrt) || vrt <= 0 || !is.finite(pc_raw)) {
      status <- "insufficient_data"
    } else {
      pc <- min(max(pc_raw, 1 / (2 * n)), 1 - 1 / (2 * n))
      logit_p <- stats::qlogis(pc)
      xterm <- logit_p * (logit_p * pc^2 - logit_p * pc + pc - 0.5) / vrt
      if (!is.finite(xterm) || xterm <= 0 || abs(pc - 0.5) < sqrt(.Machine$double.eps)) {
        status <- "undefined_at_chance"
      } else {
        drift <- sign(pc - 0.5) * scale * xterm^(1 / 4)
        boundary <- scale^2 * logit_p / drift
        y <- -drift * boundary / scale^2
        mdt <- (boundary / (2 * drift)) * (1 - exp(y)) / (1 + exp(y))
        nondecision <- mrt - mdt
      }
    }
    cbind(z[1L, by, drop = FALSE], data.frame(
      accuracy = pc_raw, mean_rt = mrt, variance_rt = vrt,
      drift_rate = drift, boundary_separation = boundary,
      nondecision_time = nondecision, n = n, status = status,
      stringsAsFactors = FALSE
    ))
  })
  do.call(rbind, out)
}

fit_gaze_weighted_choice <- function(
    x,
    response = "score",
    dwell_features,
    engine = c("glm", "lme4"),
    ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  formula <- stats::as.formula(paste(response, "~", paste(dwell_features, collapse = " + ")))
  fit_explanatory_irt(x, formula, engine = engine, ...)
}

model_missing_process <- function(x, feature_name, predictors = c("score", "response_time"), engine = c("glm", "lme4"), ...) {
  .assert_eye_dataset(x)
  engine <- match.arg(engine)
  d <- model_data(x, include_features = TRUE)
  if (!feature_name %in% names(d)) .eye_stop("Feature `", feature_name, "` is absent.")
  d$.missing_process <- as.integer(!is.finite(d[[feature_name]]))
  formula <- stats::as.formula(paste(".missing_process ~", paste(predictors, collapse = " + ")))
  if (engine == "lme4") {
    .require_namespace("lme4", "for multilevel missing-process models")
    fit <- lme4::glmer(stats::update(formula, . ~ . + (1 | participant_id) + (1 | item_id)), data = d, family = stats::binomial(), ...)
  } else {
    fit <- stats::glm(stats::update(formula, . ~ . + participant_id + item_id), data = d, family = stats::binomial(), ...)
  }
  .new_eyeprocess_model(fit, engine, "missing_process_model", d, match.call(), list(feature_name = feature_name), experimental = FALSE)
}

sensitivity_missing_process <- function(x, feature_name, formula, methods = c("complete_case", "median_indicator")) {
  .assert_eye_dataset(x)
  d <- model_data(x, include_features = TRUE)
  if (!feature_name %in% names(d)) .eye_stop("Feature `", feature_name, "` is absent.")
  fits <- list()
  if ("complete_case" %in% methods) fits$complete_case <- stats::glm(formula, data = d[is.finite(d[[feature_name]]), ], family = stats::binomial())
  if ("median_indicator" %in% methods) {
    z <- d; z[[paste0(feature_name, "_missing")]] <- as.integer(!is.finite(z[[feature_name]]))
    z[[feature_name]][!is.finite(z[[feature_name]])] <- stats::median(z[[feature_name]], na.rm = TRUE)
    f2 <- stats::update(formula, paste(". ~ . +", paste0(feature_name, "_missing")))
    fits$median_indicator <- stats::glm(f2, data = z, family = stats::binomial())
  }
  structure(list(fits = fits, feature_name = feature_name, methods = methods), class = "eye_missing_sensitivity")
}

print.eye_missing_sensitivity <- function(x, ...) {
  cat("Missing-process sensitivity analysis\n")
  cat("Feature: ", x$feature_name, "\n", sep = "")
  for (nm in names(x$fits)) {
    cat("\nMethod: ", nm, "\n", sep = "")
    print(stats::coef(summary(x$fits[[nm]])))
  }
  invisible(x)
}
