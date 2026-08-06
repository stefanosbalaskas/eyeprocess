# Functional pupil-IRT engine -------------------------------------------------

.fp_stop <- function(...) {
  if (exists(".eye_stop", mode = "function", inherits = TRUE)) .eye_stop(...) else stop(paste0(...), call. = FALSE)
}

.fp_or <- function(x, y) if (is.null(x) || !length(x)) y else x

.fp_find_column <- function(data, candidates, required = TRUE, label = "column") {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) return(hit[1L])
  if (isTRUE(required)) .fp_stop("Could not identify ", label, ". Tried: ", paste(candidates, collapse = ", "))
  NULL
}

.fp_first_table <- function(x, candidates) {
  for (name in candidates) if (is.data.frame(x[[name]]) && nrow(x[[name]])) return(x[[name]])
  data.frame()
}

.fp_nonempty_name <- function(x, label, allow_null = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) return(NULL)
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) .fp_stop("`", label, "` must be one non-empty column name.")
  x
}

.fp_group_key <- function(participant, trial) {
  participant <- as.character(participant); trial <- as.character(trial)
  if (anyNA(participant) || anyNA(trial) || any(!nzchar(participant)) || any(!nzchar(trial))) {
    .fp_stop("Participant and trial identifiers must be non-missing and non-empty.")
  }
  paste(nchar(participant), participant, nchar(trial), trial, sep = "|")
}

.fp_time_to_ms <- function(value, column) {
  value <- as.numeric(value)
  finite <- sort(unique(value[is.finite(value)]))
  increments <- diff(finite)
  typical <- if (length(increments)) stats::median(increments[increments > 0], na.rm = TRUE) else NA_real_
  if (grepl("timestamp", column, ignore.case = TRUE) && is.finite(typical) && typical < 1) value <- 1000 * value
  value
}

.fp_pupil_vector <- function(samples, pupil_column) {
  paired <- intersect(c("pupil_left", "pupil_right", "pupil_left_mm", "pupil_right_mm"), names(samples))
  if (pupil_column %in% paired && length(paired) >= 2L) {
    value <- rowMeans(as.data.frame(lapply(samples[paired], as.numeric)), na.rm = TRUE)
    value[!is.finite(value)] <- NA_real_
    return(value)
  }
  as.numeric(samples[[pupil_column]])
}

.fp_left_join_unique <- function(left, right, keys, fields, label = "mapping") {
  fields <- setdiff(intersect(fields, names(right)), keys)
  if (!length(keys) || !length(fields)) return(left)
  map <- unique(right[c(keys, fields)])
  key <- do.call(paste, c(lapply(map[keys], function(z) paste(nchar(as.character(z)), as.character(z), sep = ":")), sep = "|"))
  duplicated_key <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(duplicated_key)) {
    bad <- unique(key[duplicated_key])
    .fp_stop("The ", label, " is not unique for ", length(bad), " key(s); resolve conflicting trial metadata before fitting.")
  }
  left$.fp_row_order <- seq_len(nrow(left))
  out <- merge(left, map, by = keys, all.x = TRUE, sort = FALSE, suffixes = c("", ".mapped"))
  if (nrow(out) != nrow(left)) .fp_stop("The ", label, " changed the number of pupil samples.")
  out <- out[order(out$.fp_row_order), , drop = FALSE]
  out$.fp_row_order <- NULL
  for (field in fields) {
    mapped <- paste0(field, ".mapped")
    if (mapped %in% names(out)) {
      if (field %in% names(left)) {
        use <- is.na(out[[field]])
        out[[field]][use] <- out[[mapped]][use]
        out[[mapped]] <- NULL
      } else names(out)[names(out) == mapped] <- field
    }
  }
  out
}

.fp_binary_response <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    levels <- unique(x[!is.na(x)])
    if (length(levels) != 2L) .fp_stop("Functional pupil IRT currently requires a binary response with exactly two observed levels.")
    return(ifelse(is.na(x), NA_integer_, as.integer(x == levels[2L])))
  }
  value <- suppressWarnings(as.numeric(x))
  levels <- sort(unique(value[is.finite(value)]))
  if (identical(levels, c(0, 1))) return(as.integer(value))
  if (identical(levels, c(1, 2))) return(ifelse(is.na(value), NA_integer_, as.integer(value == 2)))
  if (identical(levels, c(-1, 1))) return(ifelse(is.na(value), NA_integer_, as.integer(value == 1)))
  .fp_stop("Functional pupil IRT currently requires binary responses coded 0/1, 1/2, -1/1, logical, factor, or two-level character.")
}

#' Specify a functional pupil-IRT model
#'
#' @param df Basis degrees of freedom.
#' @param basis Natural spline or B-spline basis.
#' @param response Response field.
#' @param engine Two-stage baseline, multilevel baseline, brms bridge, or
#'   bundled CmdStan joint model.
#' @param alignment Trial- or event-aligned time.
#' @param event_time_column Event timestamp column required for event alignment.
#' @param latency_ms Physiological latency shift applied before basis creation.
#' @param baseline_window Numeric time window used for baseline correction.
#' @param baseline_method Subtract, percent change, or z score.
#' @param min_baseline_samples Minimum finite baseline samples per trial.
#' @param drop_invalid_baseline Whether to exclude trials with invalid baselines.
#' @param time_window Optional analysis time window.
#' @param pupil_column,time_column,participant_column,item_column,trial_column Column names.
#' @param luminance_column,gaze_x_column,gaze_y_column Optional nuisance columns.
#' @param blink_column,interpolated_column Optional quality columns.
#' @param max_interpolated_fraction Maximum interpolated fraction per trial.
#' @param nuisance_by_participant Whether nuisance residualization includes participant fixed effects.
#' @param include_response_time Include response time in supported joint bridges.
#' @param ar1 Include trial-wise AR(1) residual structure in Stan.
#' @param participant_effect,item_effect Include participant/item pupil effects.
#' @param chains,parallel_chains,iter_warmup,iter_sampling CmdStan controls.
#' @param adapt_delta,max_treedepth CmdStan sampler controls.
#' @return An `eye_functional_pupil_irt_spec`.
#' @export
functional_pupil_irt_spec <- function(
    df = 6L,
    basis = c("natural_spline", "bspline"),
    response = "score",
    engine = c("two_stage_glm", "two_stage_lme4", "brms", "stan"),
    alignment = c("trial", "event"),
    event_time_column = NULL,
    latency_ms = 200,
    baseline_window = c(-200, 0),
    baseline_method = c("subtract", "percent", "zscore"),
    min_baseline_samples = 3L,
    drop_invalid_baseline = TRUE,
    time_window = NULL,
    pupil_column = NULL,
    time_column = NULL,
    participant_column = "participant_id",
    item_column = "item_id",
    trial_column = "trial_id",
    luminance_column = NULL,
    gaze_x_column = NULL,
    gaze_y_column = NULL,
    blink_column = NULL,
    interpolated_column = NULL,
    max_interpolated_fraction = 0.20,
    nuisance_by_participant = FALSE,
    include_response_time = TRUE,
    ar1 = TRUE,
    participant_effect = TRUE,
    item_effect = TRUE,
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L) {
  df <- as.integer(df)
  if (length(df) != 1L || is.na(df) || df < 2L) .fp_stop("`df` must be an integer of at least two.")
  response <- .fp_nonempty_name(response, "response")
  alignment <- match.arg(alignment)
  event_time_column <- .fp_nonempty_name(event_time_column, "event_time_column", allow_null = TRUE)
  if (alignment == "event" && is.null(event_time_column)) .fp_stop("`event_time_column` is required when `alignment = 'event'`.")
  latency_ms <- as.numeric(latency_ms)
  if (length(latency_ms) != 1L || !is.finite(latency_ms)) .fp_stop("`latency_ms` must be one finite number.")
  baseline_window <- as.numeric(baseline_window)
  if (length(baseline_window) != 2L || any(!is.finite(baseline_window)) || baseline_window[1L] >= baseline_window[2L]) .fp_stop("`baseline_window` must contain increasing finite endpoints.")
  min_baseline_samples <- as.integer(min_baseline_samples)
  if (length(min_baseline_samples) != 1L || is.na(min_baseline_samples) || min_baseline_samples < 1L) .fp_stop("`min_baseline_samples` must be a positive integer.")
  if (!is.null(time_window)) {
    time_window <- as.numeric(time_window)
    if (length(time_window) != 2L || any(!is.finite(time_window)) || time_window[1L] >= time_window[2L]) .fp_stop("`time_window` must contain increasing finite endpoints.")
  }
  max_interpolated_fraction <- as.numeric(max_interpolated_fraction)
  if (length(max_interpolated_fraction) != 1L || !is.finite(max_interpolated_fraction) || max_interpolated_fraction < 0 || max_interpolated_fraction > 1) .fp_stop("`max_interpolated_fraction` must be between zero and one.")
  controls <- list(chains = chains, parallel_chains = parallel_chains, iter_warmup = iter_warmup, iter_sampling = iter_sampling, max_treedepth = max_treedepth)
  controls <- lapply(controls, as.integer)
  if (any(vapply(controls, function(z) length(z) != 1L || is.na(z) || z < 1L, logical(1)))) .fp_stop("CmdStan iteration, chain, and tree-depth controls must be positive integers.")
  if (controls$parallel_chains > controls$chains) .fp_stop("`parallel_chains` cannot exceed `chains`.")
  adapt_delta <- as.numeric(adapt_delta)
  if (length(adapt_delta) != 1L || !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) .fp_stop("`adapt_delta` must be strictly between zero and one.")
  column_args <- list(
    pupil_column = pupil_column, time_column = time_column, participant_column = participant_column,
    item_column = item_column, trial_column = trial_column, luminance_column = luminance_column,
    gaze_x_column = gaze_x_column, gaze_y_column = gaze_y_column, blink_column = blink_column,
    interpolated_column = interpolated_column
  )
  column_args <- lapply(names(column_args), function(name) .fp_nonempty_name(column_args[[name]], name, allow_null = name %in% c("pupil_column", "time_column", "luminance_column", "gaze_x_column", "gaze_y_column", "blink_column", "interpolated_column")))
  names(column_args) <- c("pupil_column", "time_column", "participant_column", "item_column", "trial_column", "luminance_column", "gaze_x_column", "gaze_y_column", "blink_column", "interpolated_column")
  structure(c(list(
    df = df,
    basis = match.arg(basis),
    response = response,
    engine = match.arg(engine),
    alignment = alignment,
    event_time_column = event_time_column,
    latency_ms = latency_ms,
    baseline_window = baseline_window,
    baseline_method = match.arg(baseline_method),
    min_baseline_samples = min_baseline_samples,
    drop_invalid_baseline = isTRUE(drop_invalid_baseline),
    time_window = time_window,
    max_interpolated_fraction = max_interpolated_fraction,
    nuisance_by_participant = isTRUE(nuisance_by_participant),
    include_response_time = isTRUE(include_response_time),
    ar1 = isTRUE(ar1),
    participant_effect = isTRUE(participant_effect),
    item_effect = isTRUE(item_effect),
    chains = controls$chains,
    parallel_chains = controls$parallel_chains,
    iter_warmup = controls$iter_warmup,
    iter_sampling = controls$iter_sampling,
    adapt_delta = adapt_delta,
    max_treedepth = controls$max_treedepth,
    interpretation = "Pupil trajectories are physiological observations and are not automatic measures of cognitive load or any named latent construct."
  ), column_args), class = "eye_functional_pupil_irt_spec")
}

.fp_extract_eye_data <- function(x, spec) {
  samples <- .fp_first_table(x, c("eye_samples", "biometrics", "gaze_samples"))
  if (!nrow(samples)) .fp_stop("No eye/pupil sample table is available.")
  pupil_column <- spec$pupil_column
  if (is.null(pupil_column)) pupil_column <- .fp_find_column(samples, c("pupil", "pupil_size", "pupil_diameter", "pupil_mean", "pupil_left", "pupil_right", "pupil_left_mm", "pupil_right_mm"), label = "pupil column")
  if (!pupil_column %in% names(samples)) .fp_stop("Pupil column `", pupil_column, "` is unavailable.")
  time_column <- spec$time_column
  if (is.null(time_column)) time_column <- .fp_find_column(samples, c("time", "time_ms", "relative_time", "timestamp", "timestamp_ms", "sample_time"), label = "time column")
  if (!time_column %in% names(samples)) .fp_stop("Time column `", time_column, "` is unavailable.")
  samples$.pupil <- .fp_pupil_vector(samples, pupil_column)
  samples$.time <- .fp_time_to_ms(samples[[time_column]], time_column)
  trials <- tryCatch(trial_table(x), error = function(e) data.frame())
  responses <- if (is.data.frame(x$responses)) x$responses else data.frame()
  if (!spec$trial_column %in% names(samples) && nrow(trials) && all(c("recording_id", "trial_id", "start_time", "end_time") %in% names(trials)) && "recording_id" %in% names(samples)) {
    assigned <- assign_trials(x)
    samples <- .fp_first_table(assigned, c("eye_samples", "biometrics", "gaze_samples"))
    trials <- tryCatch(trial_table(assigned), error = function(e) trials)
    responses <- if (is.data.frame(assigned$responses)) assigned$responses else responses
    if (!pupil_column %in% names(samples) || !time_column %in% names(samples)) .fp_stop("Trial assignment did not preserve the requested pupil/time columns.")
    samples$.pupil <- .fp_pupil_vector(samples, pupil_column)
    samples$.time <- .fp_time_to_ms(samples[[time_column]], time_column)
  }
  needed <- setdiff(c(spec$participant_column, spec$item_column, spec$trial_column, spec$event_time_column), names(samples))
  needed <- needed[!is.na(needed) & nzchar(needed)]
  if (length(needed)) {
    mapping_sources <- Filter(function(z) is.data.frame(z) && nrow(z) && spec$trial_column %in% names(z), list(trials, responses))
    mapping_columns <- unique(c(spec$trial_column, "recording_id", spec$participant_column, spec$item_column, spec$event_time_column))
    mapping_columns <- mapping_columns[!is.na(mapping_columns) & nzchar(mapping_columns)]
    for (mapping in mapping_sources) {
      keys <- intersect(c(spec$trial_column, "recording_id"), intersect(names(samples), names(mapping)))
      fields <- intersect(mapping_columns, names(mapping))
      samples <- .fp_left_join_unique(samples, mapping, keys, fields, "trial metadata mapping")
    }
  }
  list(samples = samples, trials = trials, responses = responses, pupil_column = pupil_column, time_column = time_column)
}

.fp_prepare_frame <- function(x, spec) {
  if (inherits(x, "eye_dataset")) {
    extracted <- .fp_extract_eye_data(x, spec)
    d <- extracted$samples
    responses <- extracted$responses
  } else {
    if (!is.data.frame(x)) .fp_stop("`x` must be an eye dataset or a long pupil data frame.")
    d <- x
    responses <- data.frame()
    pupil_column <- .fp_or(spec$pupil_column, .fp_find_column(d, c("pupil", "pupil_size", "pupil_diameter"), label = "pupil column"))
    time_column <- .fp_or(spec$time_column, .fp_find_column(d, c("time", "time_ms", "relative_time", "timestamp"), label = "time column"))
    d$.pupil <- .fp_pupil_vector(d, pupil_column)
    d$.time <- .fp_time_to_ms(d[[time_column]], time_column)
  }
  required <- c(spec$participant_column, spec$item_column, spec$trial_column)
  missing <- setdiff(required, names(d))
  if (length(missing)) .fp_stop("Pupil data are missing identifiers: ", paste(missing, collapse = ", "))
  d$participant_id <- as.character(d[[spec$participant_column]])
  d$item_id <- as.character(d[[spec$item_column]])
  d$trial_id <- as.character(d[[spec$trial_column]])
  if (anyNA(d$participant_id) || anyNA(d$item_id) || anyNA(d$trial_id) || any(!nzchar(d$participant_id)) || any(!nzchar(d$item_id)) || any(!nzchar(d$trial_id))) .fp_stop("Participant, item, and trial identifiers must be non-missing and non-empty.")
  if (!".pupil" %in% names(d)) {
    pupil_column <- .fp_or(spec$pupil_column, .fp_find_column(d, c("pupil", "pupil_size", "pupil_diameter"), label = "pupil column"))
    d$.pupil <- .fp_pupil_vector(d, pupil_column)
  }
  if (!".time" %in% names(d)) {
    time_column <- .fp_or(spec$time_column, .fp_find_column(d, c("time", "time_ms", "relative_time", "timestamp"), label = "time column"))
    d$.time <- .fp_time_to_ms(d[[time_column]], time_column)
  }
  if (nrow(responses)) {
    keys <- intersect(c("participant_id", "item_id", "trial_id"), intersect(names(d), names(responses)))
    response_fields <- unique(c(spec$response, "response_time", "log_response_time", spec$event_time_column))
    response_fields <- response_fields[!is.na(response_fields) & nzchar(response_fields)]
    d <- .fp_left_join_unique(d, responses, keys, response_fields, "response mapping")
  }
  if (!spec$response %in% names(d)) .fp_stop("Response field `", spec$response, "` is unavailable.")
  d[[spec$response]] <- .fp_binary_response(d[[spec$response]])
  d
}

.fp_group_indices <- function(d) split(seq_len(nrow(d)), .fp_group_key(d$participant_id, d$trial_id), drop = TRUE)

.fp_baseline_correct <- function(d, spec) {
  groups <- .fp_group_indices(d)
  corrected <- baseline <- baseline_sd <- baseline_se <- rep(NA_real_, nrow(d))
  baseline_n <- rep(0L, nrow(d)); baseline_valid <- rep(FALSE, nrow(d)); baseline_reason <- rep("no_baseline_samples", nrow(d))
  for (index in groups) {
    z <- d[index, , drop = FALSE]
    baseline_rows <- z$.time >= spec$baseline_window[1L] & z$.time <= spec$baseline_window[2L] & is.finite(z$.pupil)
    n_baseline <- sum(baseline_rows)
    mean_baseline <- if (n_baseline) mean(z$.pupil[baseline_rows], na.rm = TRUE) else NA_real_
    sd_baseline <- if (n_baseline > 1L) stats::sd(z$.pupil[baseline_rows], na.rm = TRUE) else NA_real_
    reason <- "ok"
    valid <- n_baseline >= spec$min_baseline_samples && is.finite(mean_baseline)
    if (!valid) reason <- if (n_baseline < spec$min_baseline_samples) "insufficient_baseline_samples" else "nonfinite_baseline"
    if (valid && spec$baseline_method == "percent" && abs(mean_baseline) <= sqrt(.Machine$double.eps)) { valid <- FALSE; reason <- "zero_baseline_mean" }
    if (valid && spec$baseline_method == "zscore" && (!is.finite(sd_baseline) || sd_baseline <= sqrt(.Machine$double.eps))) { valid <- FALSE; reason <- "zero_baseline_sd" }
    value <- rep(NA_real_, nrow(z))
    if (valid) {
      if (spec$baseline_method == "subtract") value <- z$.pupil - mean_baseline
      else if (spec$baseline_method == "percent") value <- 100 * (z$.pupil - mean_baseline) / mean_baseline
      else value <- (z$.pupil - mean_baseline) / sd_baseline
    }
    corrected[index] <- value; baseline[index] <- mean_baseline; baseline_sd[index] <- sd_baseline
    baseline_se[index] <- if (n_baseline > 1L && is.finite(sd_baseline)) sd_baseline / sqrt(n_baseline) else NA_real_
    baseline_n[index] <- n_baseline; baseline_valid[index] <- valid; baseline_reason[index] <- reason
  }
  d$pupil_corrected <- corrected
  d$baseline_pupil <- baseline
  d$baseline_sd <- baseline_sd
  d$baseline_se <- baseline_se
  d$baseline_n <- baseline_n
  d$baseline_valid <- baseline_valid
  d$baseline_reason <- baseline_reason
  d
}

.fp_quality_filter <- function(d, spec) {
  keep <- is.finite(d$.pupil) & is.finite(d$.time)
  if (!is.null(spec$blink_column) && spec$blink_column %in% names(d)) keep <- keep & !(as.logical(d[[spec$blink_column]]) %in% TRUE)
  if (!is.null(spec$time_window)) keep <- keep & d$.time >= spec$time_window[1L] & d$.time <= spec$time_window[2L]
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) .fp_stop("No pupil samples remain after quality/time filtering.")
  if (!is.null(spec$interpolated_column) && spec$interpolated_column %in% names(d)) {
    group <- .fp_group_key(d$participant_id, d$trial_id)
    indicator <- as.numeric(as.logical(d[[spec$interpolated_column]]))
    indicator[!is.finite(indicator)] <- 0
    fraction <- ave(indicator, group, FUN = mean)
    d$interpolated_fraction <- fraction
    d <- d[fraction <= spec$max_interpolated_fraction, , drop = FALSE]
    if (!nrow(d)) .fp_stop("No pupil trials remain after the interpolation-quality threshold.")
  } else d$interpolated_fraction <- NA_real_
  d
}

.fp_residualize_nuisance <- function(d, spec) {
  nuisance <- character()
  if (!is.null(spec$luminance_column) && spec$luminance_column %in% names(d)) nuisance <- c(nuisance, spec$luminance_column)
  if (!is.null(spec$gaze_x_column) && spec$gaze_x_column %in% names(d)) nuisance <- c(nuisance, spec$gaze_x_column)
  if (!is.null(spec$gaze_y_column) && spec$gaze_y_column %in% names(d)) nuisance <- c(nuisance, spec$gaze_y_column)
  d$pupil_adjusted <- d$pupil_corrected
  fit <- NULL
  if (length(nuisance)) {
    terms <- nuisance
    if (isTRUE(spec$nuisance_by_participant) && length(unique(d$participant_id)) > 1L) terms <- c(terms, "factor(participant_id)")
    formula <- stats::as.formula(paste("pupil_corrected ~", paste(terms, collapse = " + ")))
    fit <- tryCatch(stats::lm(formula, data = d, na.action = stats::na.exclude), error = function(e) e)
    if (!inherits(fit, "error")) {
      residual <- as.numeric(stats::residuals(fit))
      adjusted <- if (length(residual) == nrow(d)) residual else rep(NA_real_, nrow(d))
      intercept <- unname(stats::coef(fit)[1L]); if (!is.finite(intercept)) intercept <- 0
      d$pupil_adjusted <- adjusted + intercept
    }
  }
  attr(d, "nuisance_model") <- fit
  d
}

#' Prepare aligned, corrected functional pupil data
#'
#' @param x Eye dataset or long pupil data.
#' @param spec Functional pupil specification.
#' @return An `eye_functional_pupil_data` object.
#' @export
prepare_functional_pupil_data <- function(x, spec = functional_pupil_irt_spec()) {
  if (!inherits(spec, "eye_functional_pupil_irt_spec")) .fp_stop("`spec` must be created by `functional_pupil_irt_spec()`.")
  d <- .fp_prepare_frame(x, spec)
  key <- .fp_group_key(d$participant_id, d$trial_id)
  if (identical(spec$alignment, "trial")) {
    trial_start <- ave(d$.time, key, FUN = function(z) { z <- z[is.finite(z)]; if (length(z)) min(z) else NA_real_ })
    d$.time <- d$.time - trial_start
  } else {
    if (!spec$event_time_column %in% names(d)) .fp_stop("Event-alignment column `", spec$event_time_column, "` is unavailable.")
    event_time <- as.numeric(d[[spec$event_time_column]])
    invariant <- vapply(split(event_time, key), function(z) length(unique(z[is.finite(z)])) == 1L, logical(1))
    if (!all(invariant)) .fp_stop("Each trial must contain one finite, invariant event-alignment time.")
    d$.time <- d$.time - event_time
  }
  d$.time <- d$.time - spec$latency_ms
  d <- .fp_quality_filter(d, spec)
  d <- .fp_baseline_correct(d, spec)
  baseline_audit <- unique(d[c("participant_id", "item_id", "trial_id", "baseline_pupil", "baseline_sd", "baseline_se", "baseline_n", "baseline_valid", "baseline_reason")])
  if (isTRUE(spec$drop_invalid_baseline)) d <- d[d$baseline_valid, , drop = FALSE]
  if (!nrow(d)) .fp_stop("No pupil trials remain after baseline-quality checks.")
  d <- .fp_residualize_nuisance(d, spec)
  nuisance_model <- attr(d, "nuisance_model")
  d <- d[is.finite(d$pupil_adjusted), , drop = FALSE]
  if (!nrow(d)) .fp_stop("No finite pupil samples remain after baseline and nuisance adjustment.")
  d <- d[order(d$participant_id, d$trial_id, d$.time), , drop = FALSE]
  d$person_index <- match(d$participant_id, unique(d$participant_id))
  d$item_index <- match(d$item_id, unique(d$item_id))
  trial_key <- .fp_group_key(d$participant_id, d$trial_id)
  d$trial_index <- match(trial_key, unique(trial_key))
  d$previous_index <- 0L
  groups <- split(seq_len(nrow(d)), d$trial_index)
  for (index in groups) if (length(index) > 1L) d$previous_index[index[-1L]] <- head(index, -1L)
  time_sd <- stats::sd(d$.time)
  if (!is.finite(time_sd) || time_sd <= 0) .fp_stop("Pupil sample times must vary after alignment.")
  d$time_scaled <- as.numeric(scale(d$.time))
  trial_rows <- lapply(split(seq_len(nrow(d)), d$trial_index), function(index) {
    response <- unique(d[[spec$response]][index][!is.na(d[[spec$response]][index])])
    if (length(response) != 1L) .fp_stop("Each trial must contain one non-missing, invariant response value.")
    data.frame(
      trial_index = d$trial_index[index[1L]], participant_id = d$participant_id[index[1L]],
      item_id = d$item_id[index[1L]], trial_id = d$trial_id[index[1L]], response = as.integer(response),
      stringsAsFactors = FALSE
    )
  })
  trials <- do.call(rbind, trial_rows); rownames(trials) <- NULL
  counts <- table(d$trial_index)
  out <- list(
    data = d,
    spec = spec,
    participants = unique(d$participant_id),
    items = unique(d$item_id),
    trials = trials,
    baseline_audit = baseline_audit,
    nuisance_model = nuisance_model,
    quality = data.frame(
      samples = nrow(d), trials = nrow(trials), participants = length(unique(d$participant_id)), items = length(unique(d$item_id)),
      invalid_baseline_trials = sum(!baseline_audit$baseline_valid), min_samples_per_trial = min(counts), stringsAsFactors = FALSE
    )
  )
  class(out) <- "eye_functional_pupil_data"
  out
}

#' @export
print.eye_functional_pupil_data <- function(x, ...) {
  cat("Prepared functional pupil data\n")
  print(x$quality, row.names = FALSE)
  invisible(x)
}

#' Construct a functional basis for pupil trajectories
#'
#' @param x Prepared functional pupil data or numeric time vector.
#' @param df Degrees of freedom.
#' @param basis Natural spline or B-spline.
#' @param degree B-spline polynomial degree; ignored for natural splines.
#' @param boundary_knots Optional boundary knots.
#' @param knots Optional internal knots.
#' @return Basis matrix with construction metadata.
#' @export
functional_pupil_basis <- function(x, df = 6L, basis = c("natural_spline", "bspline"), degree = 3L, boundary_knots = NULL, knots = NULL) {
  time <- if (inherits(x, "eye_functional_pupil_data")) x$data$time_scaled else as.numeric(x)
  df <- as.integer(df); basis <- match.arg(basis); degree <- as.integer(degree)
  if (length(df) != 1L || is.na(df) || df < 2L) .fp_stop("`df` must be an integer of at least two.")
  if (length(degree) != 1L || is.na(degree) || degree < 1L) .fp_stop("`degree` must be a positive integer.")
  if (length(time) < df + 1L || any(!is.finite(time)) || length(unique(time)) < df) .fp_stop("Time vector is insufficient for the requested basis.")
  if (is.null(boundary_knots)) boundary_knots <- range(time)
  boundary_knots <- as.numeric(boundary_knots)
  if (length(boundary_knots) != 2L || any(!is.finite(boundary_knots)) || boundary_knots[1L] >= boundary_knots[2L]) .fp_stop("`boundary_knots` must contain increasing finite endpoints.")
  matrix <- if (basis == "natural_spline") splines::ns(time, df = df, knots = knots, Boundary.knots = boundary_knots, intercept = TRUE) else splines::bs(time, df = df, knots = knots, degree = degree, Boundary.knots = boundary_knots, intercept = TRUE)
  colnames(matrix) <- paste0("pupil_basis_", seq_len(ncol(matrix)))
  attr(matrix, "basis") <- basis; attr(matrix, "df") <- df; attr(matrix, "degree") <- degree
  attr(matrix, "boundary_knots") <- boundary_knots; attr(matrix, "knots") <- .fp_or(attr(matrix, "knots"), knots)
  matrix
}

.fp_trial_coefficients <- function(prepared, basis_matrix) {
  d <- prepared$data
  if (!is.matrix(basis_matrix) || nrow(basis_matrix) != nrow(d)) .fp_stop("Basis matrix must have one row per prepared pupil sample.")
  groups <- split(seq_len(nrow(d)), d$trial_index)
  rows <- lapply(groups, function(index) {
    z <- d[index, , drop = FALSE]; B <- basis_matrix[index, , drop = FALSE]
    supported <- nrow(B) >= ncol(B) && qr(B)$rank == ncol(B)
    fit <- if (supported) tryCatch(stats::lm.fit(B, z$pupil_adjusted), error = function(e) NULL) else NULL
    coefficients <- if (is.null(fit)) rep(NA_real_, ncol(B)) else fit$coefficients
    trial <- z[1L, c("trial_index", "participant_id", "item_id", "trial_id"), drop = FALSE]
    trial$response <- z[[prepared$spec$response]][1L]
    if ("response_time" %in% names(z)) trial$response_time <- z$response_time[1L]
    for (j in seq_along(coefficients)) trial[[colnames(B)[j]]] <- coefficients[j]
    trial$pupil_peak <- max(z$pupil_adjusted, na.rm = TRUE)
    trial$pupil_auc <- if (nrow(z) > 1L) sum(diff(z$.time) * (head(z$pupil_adjusted, -1L) + tail(z$pupil_adjusted, -1L)) / 2, na.rm = TRUE) else NA_real_
    trial$pupil_mean <- mean(z$pupil_adjusted, na.rm = TRUE)
    trial$basis_supported <- supported
    trial$samples <- nrow(z)
    trial
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

.fp_stan_data <- function(prepared, basis_matrix) {
  d <- prepared$data
  trials <- prepared$trials[order(prepared$trials$trial_index), , drop = FALSE]
  if (length(prepared$participants) < 2L || length(prepared$items) < 2L) .fp_stop("The bundled Stan model requires at least two participants and two items.")
  response <- as.integer(trials$response)
  if (any(!response %in% c(0L, 1L))) .fp_stop("The bundled Stan model currently requires binary responses coded 0/1.")
  get_nuisance <- function(column) if (!is.null(column) && column %in% names(d)) as.numeric(d[[column]]) else rep(0, nrow(d))
  list(
    N_trial = nrow(trials),
    N_sample = nrow(d),
    P = length(prepared$participants),
    J = length(prepared$items),
    B = ncol(basis_matrix),
    response = response,
    trial_person = as.integer(match(trials$participant_id, prepared$participants)),
    trial_item = as.integer(match(trials$item_id, prepared$items)),
    sample_trial = as.integer(d$trial_index),
    sample_person = as.integer(d$person_index),
    sample_item = as.integer(d$item_index),
    pupil = as.numeric(d$pupil_adjusted),
    basis = unname(basis_matrix),
    luminance = get_nuisance(prepared$spec$luminance_column),
    gaze_x = get_nuisance(prepared$spec$gaze_x_column),
    gaze_y = get_nuisance(prepared$spec$gaze_y_column),
    previous_index = as.integer(d$previous_index),
    use_ar1 = as.integer(prepared$spec$ar1),
    use_person_pupil = as.integer(prepared$spec$participant_effect),
    use_item_pupil = as.integer(prepared$spec$item_effect)
  )
}

.fp_cmdstan_diagnostics <- function(fit) {
  summary <- fit$summary()
  sampler <- tryCatch(fit$diagnostic_summary(), error = function(e) NULL)
  data.frame(
    converged = any(is.finite(summary$rhat)) && all(summary$rhat[is.finite(summary$rhat)] <= 1.05),
    divergences = if (!is.null(sampler) && "num_divergent" %in% names(sampler)) sum(sampler$num_divergent) else NA_integer_,
    max_rhat = if (any(is.finite(summary$rhat))) max(summary$rhat, na.rm = TRUE) else NA_real_,
    min_ess_bulk = if (any(is.finite(summary$ess_bulk))) min(summary$ess_bulk, na.rm = TRUE) else NA_real_,
    min_ess_tail = if (any(is.finite(summary$ess_tail))) min(summary$ess_tail, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Fit the bundled joint functional pupil-IRT Stan model
#'
#' @param prepared Prepared functional pupil data.
#' @param basis_matrix Functional basis matrix.
#' @param seed Random seed.
#' @param refresh CmdStan refresh interval.
#' @param output_dir Optional CmdStan output directory.
#' @param ... Additional sampling arguments.
#' @return An `eye_functional_pupil_stan` object.
#' @export
fit_functional_pupil_stan <- function(prepared, basis_matrix = NULL, seed = 1L, refresh = 0L, output_dir = NULL, ...) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) .fp_stop("The `cmdstanr` package is required for the Stan engine.")
  if (!inherits(prepared, "eye_functional_pupil_data")) .fp_stop("Expected prepared functional pupil data.")
  if (is.null(basis_matrix)) basis_matrix <- functional_pupil_basis(prepared, prepared$spec$df, prepared$spec$basis)
  stan_file <- system.file("stan", "functional_pupil_irt.stan", package = "eyeprocess")
  if (!nzchar(stan_file)) .fp_stop("Bundled functional pupil Stan program is unavailable.")
  model <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)
  fit <- model$sample(
    data = .fp_stan_data(prepared, basis_matrix), seed = as.integer(seed),
    chains = prepared$spec$chains, parallel_chains = prepared$spec$parallel_chains,
    iter_warmup = prepared$spec$iter_warmup, iter_sampling = prepared$spec$iter_sampling,
    adapt_delta = prepared$spec$adapt_delta, max_treedepth = prepared$spec$max_treedepth,
    refresh = refresh, output_dir = output_dir, ...
  )
  out <- list(prepared = prepared, basis = basis_matrix, fit = fit, summary = fit$summary(), diagnostics = .fp_cmdstan_diagnostics(fit), stan_file = stan_file)
  class(out) <- "eye_functional_pupil_stan"
  out
}

# Legacy eye-dataset bridge retained for the 0.4.x public contract.
.fp_fit_legacy_eye_dataset <- function(x, spec, ...) {
  y <- functional_pupil_features(x, df = spec$df, append = TRUE, prefix = "functional_pupil")
  feature_names <- sort(unique(y$features$feature_name[grepl("^functional_pupil_", y$features$feature_name)]))
  if (!length(feature_names)) .fp_stop("No functional pupil coefficients could be derived.")
  formula <- stats::as.formula(paste(spec$response, "~", paste(feature_names, collapse = " + ")))
  if (spec$engine == "two_stage_glm") {
    fit <- fit_explanatory_irt(y, formula, engine = "glm", participant_random = FALSE, item_random = FALSE, ...)
  } else if (spec$engine == "two_stage_lme4") {
    fit <- fit_explanatory_irt(y, formula, engine = "lme4", participant_random = TRUE, item_random = TRUE, ...)
  } else if (spec$engine == "brms") {
    process_formulas <- lapply(feature_names, function(name) stats::as.formula(paste(name, "~ 1 + (1 | participant_id) + (1 | item_id)")))
    accuracy_formula <- stats::as.formula(paste(spec$response, "~ 1 + (1 | participant_id) + (1 | item_id)"))
    rt_formula <- if (isTRUE(spec$include_response_time)) log_response_time ~ 1 + (1 | participant_id) + (1 | item_id) else log_response_time ~ 1
    fit <- fit_joint_process_model(y, accuracy_formula, rt_formula, process_formulas, engine = "brms", ...)
  } else {
    .fp_stop("The Stan functional pupil engine requires long pupil samples with an explicit time column.")
  }
  out <- list(
    data = y, model = fit, spec = spec, feature_names = feature_names,
    diagnostics = data.frame(converged = TRUE, stringsAsFactors = FALSE),
    legacy = TRUE,
    warning = "Spline coefficients are measurement summaries; substantive interpretation requires latency, luminance, gaze-position, autocorrelation, and preprocessing sensitivity analyses."
  )
  class(out) <- "eye_functional_pupil_irt"
  out
}

#' Fit a functional pupil-informed IRT workflow
#'
#' @param x Eye dataset or long pupil data.
#' @param spec Functional pupil specification.
#' @param seed Random seed.
#' @param ... Engine-specific arguments.
#' @return An `eye_functional_pupil_irt` object.
#' @export
fit_joint_functional_pupil_irt <- function(x, spec = functional_pupil_irt_spec(), seed = 1L, ...) {
  if (!inherits(spec, "eye_functional_pupil_irt_spec")) .fp_stop("`spec` must be created by `functional_pupil_irt_spec()`.")
  seed <- as.integer(seed); if (length(seed) != 1L || is.na(seed)) .fp_stop("`seed` must be one integer.")
  prepared <- tryCatch(prepare_functional_pupil_data(x, spec), error = identity)
  if (inherits(prepared, "error")) {
    message <- conditionMessage(prepared)
    legacy_time_failure <- inherits(x, "eye_dataset") && grepl("time column|sample table is available", message, ignore.case = TRUE)
    if (legacy_time_failure) return(.fp_fit_legacy_eye_dataset(x, spec, ...))
    stop(prepared)
  }
  basis_matrix <- functional_pupil_basis(prepared, spec$df, spec$basis)
  coefficients <- .fp_trial_coefficients(prepared, basis_matrix)
  feature_names <- grep("^pupil_basis_", names(coefficients), value = TRUE)
  usable <- stats::complete.cases(coefficients[c("response", feature_names)]) & coefficients$basis_supported
  if (spec$engine != "stan") coefficients <- coefficients[usable, , drop = FALSE]
  if (spec$engine != "stan" && nrow(coefficients) < max(10L, length(feature_names) + 2L)) .fp_stop("Too few supported complete trials remain for the requested two-stage model.")
  if (spec$engine %in% c("two_stage_glm", "two_stage_lme4")) {
    formula <- stats::as.formula(paste("response ~", paste(feature_names, collapse = " + ")))
    if (spec$engine == "two_stage_glm") {
      model <- stats::glm(formula, family = stats::binomial(), data = coefficients, ...)
      class(model) <- unique(c(class(model), "eyeprocess_model"))
    } else {
      if (!requireNamespace("lme4", quietly = TRUE)) .fp_stop("The `lme4` package is required for the multilevel two-stage engine.")
      formula <- stats::as.formula(paste("response ~", paste(feature_names, collapse = " + "), "+ (1 | participant_id) + (1 | item_id)"))
      model <- lme4::glmer(formula, family = stats::binomial(), data = coefficients, ...)
    }
    diagnostics <- data.frame(converged = if (inherits(model, "merMod")) is.null(model@optinfo$conv$lme4$messages) else isTRUE(model$converged), stringsAsFactors = FALSE)
  } else if (spec$engine == "brms") {
    if (!requireNamespace("brms", quietly = TRUE)) .fp_stop("The `brms` package is required for `engine = 'brms'`.")
    formula <- brms::bf(stats::as.formula(paste("response ~", paste(feature_names, collapse = " + "), "+ (1 | participant_id) + (1 | item_id)")), family = brms::bernoulli())
    model <- brms::brm(formula, data = coefficients, seed = seed, ...)
    diagnostics <- data.frame(converged = TRUE, stringsAsFactors = FALSE)
  } else {
    model <- fit_functional_pupil_stan(prepared, basis_matrix, seed = seed, ...)
    diagnostics <- model$diagnostics
  }
  out <- list(
    data = prepared, trial_coefficients = coefficients, basis = basis_matrix, model = model, spec = spec,
    feature_names = feature_names, diagnostics = diagnostics,
    warning = "Pupil trajectories are physiological observations. Shared latent effects must not be labelled cognitive load without experimental, luminance-controlled, and externally reproduced evidence."
  )
  class(out) <- "eye_functional_pupil_irt"
  out
}

#' @export
print.eye_functional_pupil_irt <- function(x, ...) {
  cat("Functional pupil-informed IRT workflow\n")
  cat("Engine:       ", x$spec$engine, "\n", sep = "")
  cat("Basis:        ", x$spec$basis, " (df = ", x$spec$df, ")\n", sep = "")
  if (isTRUE(x$legacy)) {
    cat("Features:     ", length(x$feature_names), "\n", sep = "")
  } else {
    cat("Samples:      ", nrow(x$data$data), "\n", sep = "")
    cat("Trials:       ", nrow(x$trial_coefficients), "\n", sep = "")
  }
  cat("Latency shift:", x$spec$latency_ms, " ms\n", sep = "")
  print(x$diagnostics, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_functional_pupil_irt <- function(x, type = c("trajectories", "coefficients", "recovery"), ...) {
  type <- match.arg(type)
  if (isTRUE(x$legacy)) {
    data <- features_wide(x$data, id_cols = c("participant_id", "trial_id", "item_id"))
    features <- intersect(x$feature_names, names(data))
    if (!length(features)) .fp_stop("Functional pupil features are unavailable for plotting.")
    graphics::matplot(as.matrix(data[features]), type = "l", lty = 1, xlab = "Trial row", ylab = "Basis coefficient", main = "Functional pupil coefficients", ...)
    return(invisible(data[features]))
  }
  if (type == "trajectories") {
    d <- x$data$data
    groups <- split(seq_len(nrow(d)), d$trial_index)
    selected <- head(groups, 30L)
    xlim <- range(d$.time, na.rm = TRUE); ylim <- range(d$pupil_adjusted, na.rm = TRUE)
    graphics::plot(NA, xlim = xlim, ylim = ylim, xlab = "Aligned time (ms)", ylab = "Adjusted pupil", main = "Functional pupil trajectories", ...)
    for (index in selected) graphics::lines(d$.time[index], d$pupil_adjusted[index], col = grDevices::adjustcolor("black", alpha.f = 0.15))
    return(invisible(d))
  }
  if (type == "coefficients") {
    matrix <- as.matrix(x$trial_coefficients[x$feature_names])
    graphics::matplot(matrix, type = "l", lty = 1, xlab = "Trial", ylab = "Basis coefficient", main = "Functional pupil coefficients", ...)
    return(invisible(matrix))
  }
  if (!inherits(x$model, "eye_functional_pupil_stan")) .fp_stop("Posterior recovery plots require the Stan engine.")
  summary <- x$model$summary
  rows <- summary[grepl("theta_loading|response_loading", summary$variable), , drop = FALSE]
  graphics::plot(rows$mean, seq_len(nrow(rows)), xlab = "Posterior mean", ylab = "Basis coefficient", yaxt = "n", main = "Shared pupil effects", ...)
  graphics::axis(2, at = seq_len(nrow(rows)), labels = rows$variable, las = 2)
  invisible(rows)
}

#' Extract functional pupil parameters for validation
#'
#' @param x Functional pupil fit.
#' @param pattern Optional parameter regex.
#' @param confidence Credible/confidence interval level.
#' @return Parameter data frame.
#' @export
extract_functional_pupil_parameters <- function(x, pattern = NULL, confidence = 0.95) {
  if (!inherits(x, "eye_functional_pupil_irt")) .fp_stop("Expected an `eye_functional_pupil_irt`.")
  if (inherits(x$model, "eye_functional_pupil_stan")) {
    alpha <- (1 - confidence) / 2
    d <- x$model$fit$summary(probs = c(alpha, 1 - alpha))
    quantile_columns <- setdiff(names(d), c("variable", "mean", "median", "sd", "mad", "rhat", "ess_bulk", "ess_tail"))
    if (length(quantile_columns) < 2L) .fp_stop("CmdStan summary did not return the requested interval columns.")
    out <- data.frame(parameter = d$variable, estimate = d$mean, std_error = d$sd, lower = d[[quantile_columns[1L]]], upper = d[[quantile_columns[length(quantile_columns)]]], stringsAsFactors = FALSE)
  } else {
    co <- stats::coef(summary(x$model))
    z <- stats::qnorm(1 - (1 - confidence) / 2)
    out <- data.frame(parameter = rownames(co), estimate = co[, 1L], std_error = co[, 2L], lower = co[, 1L] - z * co[, 2L], upper = co[, 1L] + z * co[, 2L], stringsAsFactors = FALSE)
  }
  if (!is.null(pattern)) out <- out[grepl(pattern, out$parameter), , drop = FALSE]
  out
}

#' Diagnose functional pupil model and preprocessing quality
#'
#' @param x Functional pupil fit.
#' @return An `eye_functional_pupil_diagnostics` object.
#' @export
functional_pupil_diagnostics <- function(x) {
  if (!inherits(x, "eye_functional_pupil_irt")) .fp_stop("Expected an `eye_functional_pupil_irt`.")
  d <- x$data$data
  keys <- d[c("participant_id", "item_id", "trial_id")]
  sample_count <- stats::aggregate(rep(1L, nrow(d)), keys, sum); names(sample_count)[ncol(sample_count)] <- "samples"
  interpolation <- stats::aggregate(d$interpolated_fraction, keys, function(z) if (all(is.na(z))) NA_real_ else mean(z, na.rm = TRUE)); names(interpolation)[ncol(interpolation)] <- "interpolated_fraction"
  baseline <- unique(d[c("participant_id", "item_id", "trial_id", "baseline_pupil", "baseline_sd", "baseline_se", "baseline_n", "baseline_valid", "baseline_reason")])
  trial_quality <- merge(sample_count, interpolation, by = c("participant_id", "item_id", "trial_id"), all = TRUE, sort = FALSE)
  trial_quality <- merge(trial_quality, baseline, by = c("participant_id", "item_id", "trial_id"), all = TRUE, sort = FALSE)
  residual_acf <- .ve_bind_rows(lapply(split(seq_len(nrow(d)), d$trial_index), function(index) {
    z <- d$pupil_adjusted[index]
    if (length(z) < 3L) return(NULL)
    value <- suppressWarnings(stats::cor(head(z, -1L), tail(z, -1L), use = "complete.obs"))
    data.frame(trial_index = d$trial_index[index[1L]], lag1 = value, stringsAsFactors = FALSE)
  }))
  convergence <- x$diagnostics$converged
  convergence_pass <- length(convergence) && any(!is.na(convergence)) && all(convergence[!is.na(convergence)])
  min_support <- min(table(d$trial_index))
  checks <- data.frame(
    check = c("finite_pupil", "baseline_available", "trial_sample_support", "interpolation_threshold", "sampler_convergence"),
    value = c(
      mean(is.finite(d$pupil_adjusted)), mean(d$baseline_valid), min_support,
      if (all(is.na(d$interpolated_fraction))) NA_real_ else max(d$interpolated_fraction, na.rm = TRUE),
      as.numeric(convergence_pass)
    ),
    pass = c(
      all(is.finite(d$pupil_adjusted)), all(d$baseline_valid), min_support >= x$spec$df + 1L,
      all(is.na(d$interpolated_fraction)) || max(d$interpolated_fraction, na.rm = TRUE) <= x$spec$max_interpolated_fraction,
      convergence_pass
    ), stringsAsFactors = FALSE
  )
  out <- list(checks = checks, trial_quality = trial_quality, residual_acf = residual_acf, nuisance_model = x$data$nuisance_model)
  class(out) <- "eye_functional_pupil_diagnostics"
  out
}

#' @export
print.eye_functional_pupil_diagnostics <- function(x, ...) {
  cat("Functional pupil diagnostics\n")
  print(x$checks, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_functional_pupil_diagnostics <- function(x, ...) {
  graphics::hist(x$residual_acf$lag1, xlab = "Within-trial lag-1 correlation", main = "Pupil residual autocorrelation", ...)
  invisible(x$residual_acf)
}

#' Create a preprocessing sensitivity grid for pupil analysis
#'
#' @param baseline_windows List of baseline windows.
#' @param latency_ms Latency shifts.
#' @param basis_df Basis degrees of freedom.
#' @param baseline_methods Baseline corrections.
#' @param max_interpolated_fraction Interpolation thresholds.
#' @return Sensitivity design data frame.
#' @export
pupil_preprocessing_grid <- function(
    baseline_windows = list(c(-200, 0), c(-500, 0)),
    latency_ms = c(100, 200, 300),
    basis_df = c(4L, 6L, 8L),
    baseline_methods = c("subtract", "percent"),
    max_interpolated_fraction = c(0.10, 0.20)) {
  if (!is.list(baseline_windows) || !length(baseline_windows)) .fp_stop("`baseline_windows` must be a non-empty list.")
  baseline_windows <- lapply(baseline_windows, function(z) {
    z <- as.numeric(z); if (length(z) != 2L || any(!is.finite(z)) || z[1L] >= z[2L]) .fp_stop("Every baseline window must contain increasing finite endpoints."); z
  })
  latency_ms <- as.numeric(latency_ms); if (!length(latency_ms) || any(!is.finite(latency_ms))) .fp_stop("`latency_ms` must contain finite values.")
  basis_df <- as.integer(basis_df); if (!length(basis_df) || anyNA(basis_df) || any(basis_df < 2L)) .fp_stop("`basis_df` must contain integers of at least two.")
  baseline_methods <- match.arg(baseline_methods, c("subtract", "percent", "zscore"), several.ok = TRUE)
  max_interpolated_fraction <- as.numeric(max_interpolated_fraction)
  if (!length(max_interpolated_fraction) || any(!is.finite(max_interpolated_fraction)) || any(max_interpolated_fraction < 0 | max_interpolated_fraction > 1)) .fp_stop("Interpolation thresholds must be between zero and one.")
  rows <- expand.grid(
    baseline_index = seq_along(baseline_windows), latency_ms = latency_ms, df = basis_df,
    baseline_method = baseline_methods, max_interpolated_fraction = max_interpolated_fraction,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  rows$baseline_start <- vapply(rows$baseline_index, function(i) baseline_windows[[i]][1L], numeric(1))
  rows$baseline_end <- vapply(rows$baseline_index, function(i) baseline_windows[[i]][2L], numeric(1))
  rows$baseline_index <- NULL
  unique(rows)
}

#' Run functional pupil preprocessing sensitivity analysis
#'
#' @param x Eye dataset or long pupil data.
#' @param grid Sensitivity grid.
#' @param base_spec Base functional pupil specification.
#' @param fit Whether to fit each specification.
#' @param extractor Optional result extractor.
#' @param continue_on_error Record errors rather than stopping.
#' @param ... Passed to model fitting.
#' @return An `eye_functional_pupil_sensitivity` object.
#' @export
pupil_preprocessing_sensitivity <- function(x, grid = pupil_preprocessing_grid(), base_spec = functional_pupil_irt_spec(engine = "two_stage_glm"), fit = TRUE, extractor = extract_functional_pupil_parameters, continue_on_error = TRUE, ...) {
  if (!is.data.frame(grid) || !nrow(grid)) .fp_stop("Sensitivity grid must be a non-empty data frame.")
  required <- c("baseline_start", "baseline_end", "latency_ms", "df", "baseline_method", "max_interpolated_fraction")
  missing <- setdiff(required, names(grid)); if (length(missing)) .fp_stop("Sensitivity grid is missing: ", paste(missing, collapse = ", "))
  if (!inherits(base_spec, "eye_functional_pupil_irt_spec")) .fp_stop("`base_spec` must be a functional pupil specification.")
  if (!is.function(extractor)) .fp_stop("`extractor` must be a function.")
  rows <- list(); models <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    spec <- base_spec
    spec$baseline_window <- c(grid$baseline_start[i], grid$baseline_end[i])
    spec$latency_ms <- grid$latency_ms[i]
    spec$df <- as.integer(grid$df[i])
    spec$baseline_method <- grid$baseline_method[i]
    spec$max_interpolated_fraction <- grid$max_interpolated_fraction[i]
    result <- tryCatch(if (isTRUE(fit)) fit_joint_functional_pupil_irt(x, spec, ...) else prepare_functional_pupil_data(x, spec), error = identity)
    models[[i]] <- result
    if (inherits(result, "error")) {
      row <- data.frame(specification = i, parameter = ".error", estimate = NA_real_, error = conditionMessage(result), stringsAsFactors = FALSE)
      if (!continue_on_error) stop(result)
    } else if (isTRUE(fit)) {
      extracted <- tryCatch(extractor(result), error = identity)
      if (inherits(extracted, "error")) row <- data.frame(specification = i, parameter = ".extract", estimate = NA_real_, error = conditionMessage(extracted), stringsAsFactors = FALSE) else {
        row <- extracted; row$specification <- i; row$error <- NA_character_
      }
    } else {
      row <- data.frame(specification = i, parameter = "samples", estimate = nrow(result$data), error = NA_character_, stringsAsFactors = FALSE)
    }
    for (name in names(grid)) row[[name]] <- grid[[name]][i]
    rows[[i]] <- row
  }
  out <- list(grid = grid, results = .ve_bind_rows(rows), models = models, base_spec = base_spec)
  class(out) <- "eye_functional_pupil_sensitivity"
  out
}

#' @export
print.eye_functional_pupil_sensitivity <- function(x, ...) {
  cat("Functional pupil preprocessing sensitivity\n")
  cat("Specifications: ", nrow(x$grid), "\n", sep = "")
  cat("Failed:         ", sum(!is.na(x$results$error) & nzchar(x$results$error)), "\n", sep = "")
  invisible(x)
}

#' @export
plot.eye_functional_pupil_sensitivity <- function(x, parameter = NULL, ...) {
  d <- x$results
  if (!is.null(parameter)) d <- d[d$parameter %in% parameter, , drop = FALSE]
  d <- d[is.finite(d$estimate), , drop = FALSE]
  graphics::plot(d$specification, d$estimate, xlab = "Specification", ylab = "Estimate", main = "Pupil preprocessing sensitivity", ...)
  invisible(d)
}

#' Compare functional and scalar pupil summaries
#'
#' @param x Functional pupil fit or prepared data.
#' @param scalar_features Scalar feature names.
#' @param criterion AIC or cross-validated log loss.
#' @param folds Grouped folds for cross-validation.
#' @param seed Random seed.
#' @return An `eye_functional_scalar_comparison` data frame.
#' @export
compare_functional_scalar_models <- function(x, scalar_features = c("pupil_peak", "pupil_auc", "pupil_mean"), criterion = c("AIC", "log_loss"), folds = 5L, seed = 1L) {
  criterion <- match.arg(criterion)
  coefficients <- if (inherits(x, "eye_functional_pupil_irt")) x$trial_coefficients else {
    if (!inherits(x, "eye_functional_pupil_data")) .fp_stop("Expected a functional pupil fit or prepared data.")
    basis <- functional_pupil_basis(x, x$spec$df, x$spec$basis)
    .fp_trial_coefficients(x, basis)
  }
  functional_features <- grep("^pupil_basis_", names(coefficients), value = TRUE)
  if (!length(functional_features)) .fp_stop("No functional pupil basis features are available.")
  scalar_features <- intersect(scalar_features, names(coefficients))
  if (!length(scalar_features)) .fp_stop("No requested scalar pupil features are available.")
  coefficients <- coefficients[stats::complete.cases(coefficients[c("response", functional_features, scalar_features, "participant_id")]), , drop = FALSE]
  if (nrow(coefficients) < 10L || length(unique(coefficients$participant_id)) < 2L) .fp_stop("Too few complete grouped observations are available for model comparison.")
  folds <- as.integer(folds); if (length(folds) != 1L || is.na(folds) || folds < 2L) .fp_stop("`folds` must be an integer of at least two.")
  folds <- min(folds, length(unique(coefficients$participant_id)))
  formulas <- list(
    functional = stats::as.formula(paste("response ~", paste(functional_features, collapse = " + "))),
    scalar = stats::as.formula(paste("response ~", paste(scalar_features, collapse = " + ")))
  )
  rows <- lapply(names(formulas), function(name) {
    if (criterion == "AIC") {
      fit <- stats::glm(formulas[[name]], family = stats::binomial(), data = coefficients)
      data.frame(model = name, criterion = "AIC", value = stats::AIC(fit), stringsAsFactors = FALSE)
    } else {
      fold <- grouped_folds(coefficients, group = "participant_id", v = folds, seed = seed)
      loss <- numeric(nrow(fold))
      for (i in seq_len(nrow(fold))) {
        fit <- stats::glm(formulas[[name]], family = stats::binomial(), data = coefficients[fold$analysis[[i]], , drop = FALSE])
        p <- pmin(pmax(stats::predict(fit, newdata = coefficients[fold$assessment[[i]], , drop = FALSE], type = "response"), 1e-8), 1 - 1e-8)
        y <- coefficients$response[fold$assessment[[i]]]
        loss[i] <- -mean(y * log(p) + (1 - y) * log1p(-p))
      }
      data.frame(model = name, criterion = "log_loss", value = mean(loss), stringsAsFactors = FALSE)
    }
  })
  out <- do.call(rbind, rows)
  out$rank <- rank(out$value)
  class(out) <- c("eye_functional_scalar_comparison", "data.frame")
  out
}

#' @export
print.eye_functional_scalar_comparison <- function(x, ...) {
  cat("Functional versus scalar pupil model comparison\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}
