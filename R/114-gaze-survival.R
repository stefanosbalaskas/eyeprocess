# Censored gaze-latency survival analysis ---------------------------------

.gaze_surv_columns <- c(
  "participant_id", "trial_id", "stimulus_id", "condition", "target_aoi",
  "time_origin", "event_time", "censor_time", "analysis_time",
  "event_observed", "event_type", "n_valid_samples", "valid_data_fraction",
  "trial_duration"
)

.gaze_surv_require <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package `", pkg, "` is required",
      if (!is.null(reason)) paste0(" ", reason) else "",
      ".",
      call. = FALSE
    )
  }
}

.gaze_surv_rhs <- function(formula) {
  f <- if (inherits(formula, "formula")) {
    paste(deparse(formula), collapse = "")
  } else {
    as.character(formula)
  }
  rhs <- if (grepl("~", f, fixed = TRUE)) {
    trimws(strsplit(f, "~", fixed = TRUE)[[1L]][2L])
  } else {
    trimws(f)
  }
  if (!nzchar(rhs)) stop("Formula must contain at least one predictor.", call. = FALSE)
  rhs
}

.gaze_surv_quality_rules <- function(x) {
  if (is.null(x) || !length(x)) return("")
  if (!is.list(x)) return(paste(as.character(x), collapse = ";"))
  nm <- names(x)
  if (is.null(nm) || any(!nzchar(nm))) return(paste(vapply(x, as.character, character(1L)), collapse = ";"))
  ord <- order(nm)
  paste0(nm[ord], "=", vapply(x[ord], function(z) paste(as.character(z), collapse = ","), character(1L)), collapse = ";")
}

.gaze_surv_provenance <- function(data, model_specification = NULL, estimator = NULL) {
  fields <- intersect(
    c(
      "source_data", "preprocessing_specification", "event_detector",
      "event_join_key", "aoi_specification", "quality_rules", "time_origin"
    ),
    names(data)
  )
  p <- lapply(fields, function(nm) sort(unique(as.character(stats::na.omit(data[[nm]])))))
  names(p) <- fields
  p$model_specification <- model_specification
  p$estimator <- estimator
  p$software <- list(
    R = as.character(getRversion()),
    eyeprocess = tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_),
    survival = tryCatch(as.character(utils::packageVersion("survival")), error = function(e) NA_character_),
    coxme = tryCatch(as.character(utils::packageVersion("coxme")), error = function(e) NA_character_)
  )
  p
}

.gaze_surv_event_time <- function(z, target_aoi, event_type, time_col, aoi_col, episode_type_col) {
  if (!nrow(z)) return(NA_real_)
  z <- z[order(z[[time_col]], method = "radix"), , drop = FALSE]

  if (!is.null(episode_type_col) && episode_type_col %in% names(z)) {
    et <- tolower(as.character(z[[episode_type_col]]))
    if (identical(event_type, "first_fixation")) {
      z <- z[et == "fixation", , drop = FALSE]
    } else if (event_type %in% c(
      "first_aoi_entry", "first_evidence_inspection", "first_revisit",
      "first_transition_into_target", "disengagement"
    )) {
      keep <- et %in% c("aoi_visit", "fixation")
      if (any(keep)) z <- z[keep, , drop = FALSE]
    }
  }
  if (!nrow(z)) return(NA_real_)

  labels <- as.character(z[[aoi_col]])
  target <- labels == as.character(target_aoi)
  if (event_type %in% c("first_fixation", "first_aoi_entry", "first_evidence_inspection")) {
    q <- which(target)
    return(if (length(q)) as.numeric(z[[time_col]][q[1L]]) else NA_real_)
  }

  # Revisit, transition and disengagement are visit-level concepts. Collapse
  # consecutive identical labels so multiple fixations in one visit do not
  # become false revisits.
  run_start <- c(TRUE, labels[-1L] != labels[-length(labels)])
  runs <- z[run_start, , drop = FALSE]
  run_labels <- as.character(runs[[aoi_col]])
  run_target <- run_labels == as.character(target_aoi)

  if (identical(event_type, "first_revisit")) {
    q <- which(run_target)
    return(if (length(q) >= 2L) as.numeric(runs[[time_col]][q[2L]]) else NA_real_)
  }
  if (identical(event_type, "first_transition_into_target")) {
    prev <- c(NA_character_, head(run_labels, -1L))
    q <- which(run_target & !is.na(prev) & prev != as.character(target_aoi))
    return(if (length(q)) as.numeric(runs[[time_col]][q[1L]]) else NA_real_)
  }
  if (identical(event_type, "disengagement")) {
    idx <- which(run_target)
    if (!length(idx) || idx[1L] >= nrow(runs)) return(NA_real_)
    after <- seq.int(idx[1L] + 1L, nrow(runs))
    q <- after[run_labels[after] != as.character(target_aoi)]
    return(if (length(q)) as.numeric(runs[[time_col]][q[1L]]) else NA_real_)
  }
  stop("Unsupported event_type `", event_type, "`.", call. = FALSE)
}

#' Prepare right-censored gaze-latency data
#'
#' Builds one row per participant-trial. A trial without a qualifying target
#' event is right-censored only when a complete observation window is known.
#' Missing or unusable gaze is retained as a review state and is never silently
#' converted to ordinary censoring.
#'
#' @return A data.frame with the canonical gaze-survival contract.
#' @export
prepare_gaze_survival_data <- function(
    trials,
    events = NULL,
    target_aoi = NULL,
    event_type = "first_fixation",
    participant_col = "participant_id",
    trial_col = "trial_id",
    recording_col = "recording_id",
    stimulus_col = "stimulus_id",
    condition_col = "condition",
    trial_start_col = NULL,
    trial_end_col = NULL,
    time_origin_col = NULL,
    observation_end_reason_col = NULL,
    event_time_col = "start_time",
    event_aoi_col = "aoi_id",
    event_trial_col = "trial_id",
    event_participant_col = "participant_id",
    event_recording_col = "recording_id",
    episode_type_col = "episode_type",
    event_observed_col = "event_observed",
    supplied_event_time_col = "event_time",
    supplied_censor_time_col = "censor_time",
    n_valid_samples_col = "n_valid_samples",
    valid_fraction_col = "valid_data_fraction",
    valid_observation_col = NULL,
    min_valid_fraction = NULL,
    time_origin = "trial_start",
    time_unit = c("seconds", "milliseconds"),
    source_data = NULL,
    preprocessing_specification = NULL,
    event_detector = NULL,
    aoi_specification = NULL,
    quality_rules = list()) {
  if (!is.data.frame(trials)) stop("`trials` must be a data.frame.", call. = FALSE)
  d <- trials
  time_unit <- match.arg(time_unit)
  miss <- setdiff(c(participant_col, trial_col), names(d))
  if (length(miss)) stop("Missing trial columns: ", paste(miss, collapse = ", "), ".", call. = FALSE)

  key <- paste(d[[participant_col]], d[[trial_col]], sep = "\r")
  if (anyDuplicated(key)) stop("Duplicated participant/trial rows are not allowed.", call. = FALSE)
  if (!is.null(min_valid_fraction) &&
      (length(min_valid_fraction) != 1L || !is.finite(min_valid_fraction) ||
       min_valid_fraction < 0 || min_valid_fraction > 1)) {
    stop("`min_valid_fraction` must be in [0, 1].", call. = FALSE)
  }

  first_existing <- function(candidates) {
    hit <- candidates[candidates %in% names(d)]
    if (length(hit)) hit[1L] else NULL
  }
  start_col <- if (!is.null(trial_start_col)) trial_start_col else first_existing(c("start_time", "trial_start", "trial_start_time"))
  end_col <- if (!is.null(trial_end_col)) trial_end_col else first_existing(c("end_time", "trial_end", "trial_end_time"))
  if ((!is.null(start_col) && !start_col %in% names(d)) || (!is.null(end_col) && !end_col %in% names(d))) {
    stop("Explicit trial start/end columns must exist in `trials`.", call. = FALSE)
  }
  if ((is.null(start_col) || is.null(end_col)) && !supplied_censor_time_col %in% names(d)) {
    stop("A complete observation window requires trial start/end columns or explicit censor_time.", call. = FALSE)
  }

  scale <- if (identical(time_unit, "milliseconds")) 0.001 else 1
  starts <- if (!is.null(start_col)) suppressWarnings(as.numeric(d[[start_col]])) * scale else rep(0, nrow(d))
  ends <- if (!is.null(end_col)) suppressWarnings(as.numeric(d[[end_col]])) * scale else rep(NA_real_, nrow(d))

  origin_col <- time_origin_col
  if (is.null(origin_col) && !identical(time_origin, "trial_start") && time_origin %in% names(d)) origin_col <- time_origin
  if (!is.null(origin_col)) {
    if (!origin_col %in% names(d)) stop("`time_origin_col` is absent.", call. = FALSE)
    origins <- suppressWarnings(as.numeric(d[[origin_col]])) * scale
  } else if (identical(time_origin, "trial_start")) {
    origins <- starts
  } else if (is.null(end_col) && supplied_censor_time_col %in% names(d)) {
    origins <- rep(0, nrow(d))
  } else {
    stop("A non-trial-start time origin requires `time_origin_col` or a matching trial column.", call. = FALSE)
  }

  if (!is.null(end_col)) {
    censor <- ends - origins
    trial_duration <- ends - starts
  } else {
    censor <- suppressWarnings(as.numeric(d[[supplied_censor_time_col]])) * scale
    trial_duration <- if ("trial_duration" %in% names(d)) suppressWarnings(as.numeric(d$trial_duration)) * scale else censor
  }

  condition_source <- if (condition_col %in% names(d)) {
    condition_col
  } else if (identical(condition_col, "condition") && "condition_id" %in% names(d)) {
    "condition_id"
  } else {
    NULL
  }

  out <- data.frame(
    participant_id = as.character(d[[participant_col]]),
    trial_id = as.character(d[[trial_col]]),
    stimulus_id = if (stimulus_col %in% names(d)) as.character(d[[stimulus_col]]) else NA_character_,
    condition = if (!is.null(condition_source)) as.character(d[[condition_source]]) else NA_character_,
    target_aoi = if (!is.null(target_aoi)) rep(as.character(target_aoi), nrow(d)) else if ("target_aoi" %in% names(d)) as.character(d$target_aoi) else NA_character_,
    time_origin = rep(as.character(time_origin), nrow(d)),
    event_time = NA_real_,
    censor_time = as.numeric(censor),
    analysis_time = NA_real_,
    event_observed = NA_real_,
    event_type = rep(as.character(event_type), nrow(d)),
    n_valid_samples = if (n_valid_samples_col %in% names(d)) suppressWarnings(as.numeric(d[[n_valid_samples_col]])) else NA_real_,
    valid_data_fraction = if (valid_fraction_col %in% names(d)) suppressWarnings(as.numeric(d[[valid_fraction_col]])) else NA_real_,
    trial_duration = as.numeric(trial_duration),
    censor_reason = rep("target_event_not_observed", nrow(d)),
    analysis_eligible = rep(TRUE, nrow(d)),
    review_required = rep(FALSE, nrow(d)),
    stringsAsFactors = FALSE
  )
  if (recording_col %in% names(d)) out$recording_id <- as.character(d[[recording_col]])
  if (!is.null(observation_end_reason_col)) {
    if (!observation_end_reason_col %in% names(d)) stop("`observation_end_reason_col` is absent.", call. = FALSE)
    out$observation_end_reason <- as.character(d[[observation_end_reason_col]])
  } else {
    out$observation_end_reason <- if (!is.null(end_col)) "trial_window_end" else "explicit_censor_time"
  }

  complete <- is.finite(out$censor_time) & out$censor_time >= 0 & is.finite(out$trial_duration) & out$trial_duration >= 0
  if (!is.null(valid_observation_col)) {
    if (!valid_observation_col %in% names(d)) stop("`valid_observation_col` is absent.", call. = FALSE)
    valid_obs <- !is.na(d[[valid_observation_col]]) & as.logical(d[[valid_observation_col]])
    invalid <- !valid_obs
    complete <- complete & valid_obs
    out$censor_reason[invalid] <- "invalid_observation_flag"
    out$analysis_eligible[invalid] <- FALSE
    out$review_required[invalid] <- TRUE
  }
  if (!is.null(min_valid_fraction) && valid_fraction_col %in% names(d)) {
    poor <- !is.na(out$valid_data_fraction) & out$valid_data_fraction < min_valid_fraction
    complete[poor] <- FALSE
    out$censor_reason[poor] <- "unusable_gaze_quality"
    out$analysis_eligible[poor] <- FALSE
    out$review_required[poor] <- TRUE
  }

  event_join_key <- NULL
  if (!is.null(events)) {
    if (is.null(target_aoi)) stop("`target_aoi` is required when deriving events from an event table.", call. = FALSE)
    if (!is.data.frame(events)) stop("`events` must be a data.frame.", call. = FALSE)
    needed <- c(event_time_col, event_aoi_col, event_trial_col)
    missing_events <- setdiff(needed, names(events))
    if (length(missing_events)) stop("Event table is missing required columns: ", paste(missing_events, collapse = ", "), ".", call. = FALSE)
    ev <- events

    participant_key_available <- event_participant_col %in% names(ev) && all(!is.na(ev[[event_participant_col]])) && all(nzchar(as.character(ev[[event_participant_col]])))
    recording_key_available <- recording_col %in% names(d) && event_recording_col %in% names(ev) && all(!is.na(ev[[event_recording_col]])) && all(nzchar(as.character(ev[[event_recording_col]])))
    if (participant_key_available) {
      event_join_key <- "participant_trial"
    } else if (recording_key_available) {
      event_join_key <- "recording_trial"
    } else if (!anyDuplicated(as.character(d[[trial_col]]))) {
      event_join_key <- "trial_only"
    } else {
      stop(
        "events lacks participant and recording identity while trial IDs repeat across participants. ",
        "Provide participant or recording keys; trial-only matching would be ambiguous.",
        call. = FALSE
      )
    }

    event_times <- rep(NA_real_, nrow(d))
    for (i in seq_len(nrow(d))) {
      mask <- as.character(ev[[event_trial_col]]) == as.character(d[[trial_col]][i])
      if (identical(event_join_key, "participant_trial")) {
        mask <- mask & as.character(ev[[event_participant_col]]) == as.character(d[[participant_col]][i])
      } else if (identical(event_join_key, "recording_trial")) {
        mask <- mask & as.character(ev[[event_recording_col]]) == as.character(d[[recording_col]][i])
      }
      q <- ev[mask, , drop = FALSE]
      et <- .gaze_surv_event_time(
        q,
        target_aoi,
        event_type,
        event_time_col,
        event_aoi_col,
        if (episode_type_col %in% names(q)) episode_type_col else NULL
      )
      if (is.finite(et)) event_times[i] <- et * scale - origins[i]
    }
    out$event_time <- event_times
    observed <- !is.na(out$event_time) & complete
    out$event_observed[complete] <- as.numeric(observed[complete])
    out$censor_reason[observed] <- "event_observed"
  } else {
    if (!event_observed_col %in% names(d)) {
      stop(
        "Without an event table, `event_observed` must be supplied explicitly; ",
        "missing event times are not automatically censored.",
        call. = FALSE
      )
    }
    obs <- suppressWarnings(as.numeric(d[[event_observed_col]]))
    if (any(!is.na(obs) & !obs %in% c(0, 1))) stop("`event_observed` must contain only 0/1 (plus NA).", call. = FALSE)
    if (supplied_event_time_col %in% names(d)) out$event_time <- suppressWarnings(as.numeric(d[[supplied_event_time_col]])) * scale
    out$event_observed <- obs
    out$censor_reason[out$event_observed == 1] <- "event_observed"
    out$event_observed[!complete] <- NA_real_
  }

  out$analysis_eligible[!complete] <- FALSE
  out$review_required[!complete] <- TRUE
  idx <- !complete & out$censor_reason == "target_event_not_observed"
  out$censor_reason[idx] <- "incomplete_observation_window"
  out$analysis_time <- ifelse(
    out$event_observed == 1,
    out$event_time,
    ifelse(out$event_observed == 0, out$censor_time, NA_real_)
  )

  qrules <- quality_rules
  if (!is.list(qrules)) qrules <- list(rule = qrules)
  if (!is.null(min_valid_fraction) && is.null(qrules$min_valid_fraction)) qrules$min_valid_fraction <- min_valid_fraction
  if (!is.null(valid_observation_col) && is.null(qrules$valid_observation_col)) qrules$valid_observation_col <- valid_observation_col

  out$source_data <- if (!is.null(source_data)) source_data else if (!is.null(events)) "trial_table+event_table" else "trial_level_survival_inputs"
  if (!is.null(event_join_key)) out$event_join_key <- event_join_key
  out$event_detector <- if (!is.null(event_detector)) event_detector else if (!is.null(events)) "canonical_episodes" else "supplied"
  out$aoi_specification <- if (!is.null(aoi_specification)) aoi_specification else if (!is.null(target_aoi)) paste0("target_aoi=", target_aoi) else NA_character_
  out$quality_rules <- .gaze_surv_quality_rules(qrules)
  out$preprocessing_specification <- if (!is.null(preprocessing_specification)) preprocessing_specification else NA_character_
  out$model_specification <- NA_character_
  out$software_version <- paste0(
    "R=", getRversion(),
    ";eyeprocess=", tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_)
  )

  issues <- validate_gaze_survival_data(out, raise_on_error = FALSE)
  errors <- issues[issues$severity == "error", , drop = FALSE]
  if (nrow(errors)) stop("Invalid gaze survival data: ", paste(errors$message, collapse = "; "), call. = FALSE)
  for (msg in issues$message[issues$severity == "warning"]) warning(msg, call. = FALSE)
  rownames(out) <- NULL
  out
}

#' Validate gaze-survival data
#' @return A data.frame of validation issues.
#' @export
validate_gaze_survival_data <- function(data, raise_on_error = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  rows <- list()
  k <- 0L
  add <- function(severity, code, message, n = 0L) {
    k <<- k + 1L
    rows[[k]] <<- data.frame(
      severity = severity,
      code = code,
      n = as.integer(n),
      message = message,
      stringsAsFactors = FALSE
    )
  }

  miss <- setdiff(.gaze_surv_columns, names(data))
  if (length(miss)) {
    add("error", "missing_columns", paste0("Missing canonical columns: ", paste(miss, collapse = ", ")), length(miss))
  } else {
    dup <- duplicated(paste(data$participant_id, data$trial_id, sep = "\r"))
    if (any(dup)) add("error", "duplicated_trials", "Participant/trial keys must be unique.", sum(dup))

    obs <- suppressWarnings(as.numeric(data$event_observed))
    at <- suppressWarnings(as.numeric(data$analysis_time))
    et <- suppressWarnings(as.numeric(data$event_time))
    ct <- suppressWarnings(as.numeric(data$censor_time))
    td <- suppressWarnings(as.numeric(data$trial_duration))
    bad <- !is.na(obs) & !obs %in% c(0, 1)
    if (any(bad)) add("error", "invalid_event_indicator", "event_observed must be 0/1 or NA for non-analyzable rows.", sum(bad))

    neg <- (!is.na(at) & at < 0) | (!is.na(et) & et < 0) | (!is.na(ct) & ct < 0) | (!is.na(td) & td < 0)
    if (any(neg)) add("error", "negative_time", "Latency, censoring, and trial-duration values cannot be negative.", sum(neg))

    analyzable <- !is.na(obs) & obs %in% c(0, 1)
    missing_window <- is.na(ct) | is.na(td)
    if (any(missing_window & analyzable)) add("error", "missing_observation_window", "Analyzable rows require known censor_time and trial_duration.", sum(missing_window & analyzable))

    x <- obs == 1 & is.na(et)
    x[is.na(x)] <- FALSE
    if (any(x)) add("error", "observed_missing_event_time", "Observed events require event_time.", sum(x))

    x <- obs == 1 & !is.na(et) & !is.na(ct) & et > ct + 1e-12
    x[is.na(x)] <- FALSE
    if (any(x)) add("error", "event_after_censor", "event_time cannot exceed censor_time.", sum(x))

    x <- obs == 1 & !is.na(et) & !is.na(at) & abs(at - et) > 1e-12
    x[is.na(x)] <- FALSE
    if (any(x)) add("error", "analysis_time_event_mismatch", "Observed rows require analysis_time == event_time.", sum(x))

    x <- obs == 0 & !is.na(ct) & !is.na(at) & abs(at - ct) > 1e-12
    x[is.na(x)] <- FALSE
    if (any(x)) add("error", "analysis_time_censor_mismatch", "Right-censored rows require analysis_time == censor_time.", sum(x))

    x <- obs == 1 & et == 0
    x[is.na(x)] <- FALSE
    if (any(x)) add("warning", "event_at_time_zero", "Target event occurs at time zero; verify time origin and initial fixation.", sum(x))

    x <- obs == 0 & ct == 0
    x[is.na(x)] <- FALSE
    if (any(x)) add("warning", "zero_followup_censoring", "Right-censored trials with zero follow-up contribute no time at risk; verify the observation window.", sum(x))

    if (any(is.na(obs))) add("warning", "non_analyzable_rows", "Rows with unknown event status are retained for review and are not valid right-censored observations.", sum(is.na(obs)))
    ne <- sum(obs == 1, na.rm = TRUE)
    na_n <- sum(analyzable)
    if (na_n && ne < 5) add("warning", "sparse_events", "Fewer than five observed events are available; inferential survival models may be unstable.", ne)
    if (na_n && ne / na_n < .1) add("warning", "extreme_censoring", "Observed-event proportion is below 10%; report censoring and consider sensitivity analyses.", ne)

    vf <- suppressWarnings(as.numeric(data$valid_data_fraction))
    badvf <- !is.na(vf) & (vf < 0 | vf > 1)
    if (any(badvf)) add("error", "invalid_valid_fraction", "valid_data_fraction must lie in [0, 1].", sum(badvf))
  }

  out <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(severity = character(), code = character(), n = integer(), message = character(), stringsAsFactors = FALSE)
  }
  if (isTRUE(raise_on_error) && any(out$severity == "error")) {
    stop(paste(out$message[out$severity == "error"], collapse = "; "), call. = FALSE)
  }
  out
}

.gaze_surv_analysis_rows <- function(data) {
  validate_gaze_survival_data(data)
  if (any(is.na(data$event_observed))) {
    stop("Non-analyzable rows are present. Resolve/review them explicitly before model fitting; they are not censored trials.", call. = FALSE)
  }
  if ("analysis_eligible" %in% names(data) && any(!as.logical(data$analysis_eligible))) {
    stop("analysis_eligible=FALSE rows are present. Explicitly resolve or exclude them before model fitting.", call. = FALSE)
  }
  data
}

#' Summarize gaze censoring
#' @export
summarise_gaze_censoring <- function(data, by = NULL) {
  validate_gaze_survival_data(data, raise_on_error = FALSE)
  groups <- if (is.null(by)) character() else as.character(by)
  missing_groups <- setdiff(groups, names(data))
  if (length(missing_groups)) stop("Unknown grouping column(s): ", paste(missing_groups, collapse = ", "), call. = FALSE)
  split_data <- if (!length(groups)) {
    list(all = data)
  } else {
    split(data, interaction(data[groups], drop = TRUE, lex.order = TRUE), drop = TRUE)
  }
  ans <- lapply(split_data, function(z) {
    obs <- suppressWarnings(as.numeric(z$event_observed))
    analyzable <- !is.na(obs) & obs %in% c(0, 1)
    base <- if (length(groups)) as.list(z[1L, groups, drop = FALSE]) else list()
    c(
      base,
      list(
        n_trials = nrow(z),
        n_analyzable = sum(analyzable),
        n_observed_events = sum(obs == 1, na.rm = TRUE),
        n_censored = sum(obs == 0, na.rm = TRUE),
        n_review_required = sum(is.na(obs)),
        censoring_fraction = if (sum(analyzable)) sum(obs == 0, na.rm = TRUE) / sum(analyzable) else NA_real_
      )
    )
  })
  out <- do.call(rbind, lapply(ans, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  out
}

#' Kaplan-Meier gaze survival
#' @export
estimate_gaze_survival <- function(data, group = NULL, conf_level = .95) {
  .gaze_surv_require("survival", "for survival estimation")
  d <- .gaze_surv_analysis_rows(data)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || conf_level <= 0 || conf_level >= 1) stop("`conf_level` must be between 0 and 1.", call. = FALSE)
  if (!is.null(group) && !group %in% names(d)) stop("Unknown group column.", call. = FALSE)
  f <- if (is.null(group)) {
    survival::Surv(analysis_time, event_observed) ~ 1
  } else {
    stats::as.formula(paste0("survival::Surv(analysis_time, event_observed) ~ `", group, "`"))
  }
  fit <- survival::survfit(f, data = d, conf.int = conf_level)
  s <- summary(fit)
  out <- data.frame(
    time = s$time,
    n_risk = s$n.risk,
    n_events = s$n.event,
    n_censored = s$n.censor,
    survival = s$surv,
    std_error = s$std.err,
    lower = s$lower,
    upper = s$upper,
    stringsAsFactors = FALSE
  )
  out$group <- if (is.null(s$strata)) "all" else sub("^[^=]+=", "", as.character(s$strata))
  attr(out, "fit") <- fit
  attr(out, "provenance") <- .gaze_surv_provenance(d, estimator = "Kaplan-Meier")
  out
}

.gaze_surv_fit_with_warning_guard <- function(expr, label) {
  caught <- character()
  fit <- withCallingHandlers(
    expr,
    warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  danger <- grepl("converg|infinite|failed|singular", caught, ignore.case = TRUE)
  if (any(danger)) stop(label, " convergence/estimation warning: ", paste(caught[danger], collapse = " | "), call. = FALSE)
  if (any(!danger)) for (msg in caught[!danger]) warning(msg, call. = FALSE)
  fit
}

#' Fit Cox gaze-latency model
#' @export
fit_gaze_cox_model <- function(data, formula, ties = c("breslow", "efron", "exact"), cluster = NULL) {
  .gaze_surv_require("survival", "for Cox regression")
  d <- .gaze_surv_analysis_rows(data)
  ties <- match.arg(ties)
  rhs <- .gaze_surv_rhs(formula)
  if (!is.null(cluster) && !cluster %in% names(d)) stop("Unknown cluster column.", call. = FALSE)
  f <- stats::as.formula(paste0("survival::Surv(analysis_time, event_observed) ~ ", rhs))
  fit <- .gaze_surv_fit_with_warning_guard(
    survival::coxph(
      f,
      data = d,
      ties = ties,
      x = TRUE,
      model = TRUE,
      cluster = if (is.null(cluster)) NULL else d[[cluster]]
    ),
    "Cox"
  )
  if (any(!is.finite(stats::coef(fit)))) stop("Cox fit contains non-finite coefficients and is not treated as valid.", call. = FALSE)
  structure(
    list(
      model_family = "cox",
      backend = "survival::coxph",
      fit = fit,
      data = d,
      formula = formula,
      participant_col = if (!is.null(cluster)) cluster else "participant_id",
      repeated_structure = if (!is.null(cluster)) paste0("cluster_robust:", cluster) else NULL,
      provenance = .gaze_surv_provenance(d, as.character(formula), paste0("Cox PH (", ties, ")"))
    ),
    class = "eye_gaze_survival_model"
  )
}

#' Fit repeated-participant Cox model
#'
#' `structure = "cluster_robust"` fits a marginal Cox model with participant-
#' clustered sandwich uncertainty. `structure = "frailty"` uses `coxme::coxme`
#' to fit a Gaussian participant random intercept. These are distinct estimators
#' and are never silently substituted.
#' @export
fit_gaze_mixed_cox_model <- function(data, formula, participant_col = "participant_id", structure = NULL, ties = c("breslow","efron","exact")) {
  .gaze_surv_require("survival", "for repeated-participant Cox regression")
  d <- .gaze_surv_analysis_rows(data)
  if (is.null(structure)) stop("`structure` must be specified explicitly as 'cluster_robust' or 'frailty'.", call.=FALSE)
  structure <- match.arg(structure, c("cluster_robust", "frailty"))
  ties <- match.arg(ties)
  rhs <- .gaze_surv_rhs(formula)
  if (!participant_col %in% names(d)) stop("Unknown participant column.", call.=FALSE)

  if (structure == "cluster_robust") {
    f <- stats::as.formula(paste0(
      "survival::Surv(analysis_time, event_observed) ~ ", rhs,
      " + cluster(`", participant_col, "`)"
    ))
    fit <- survival::coxph(f, data=d, ties=ties, x=TRUE, model=TRUE)
    backend <- "survival::coxph"
    repeated <- paste0("cluster_robust:", participant_col)
  } else {
    .gaze_surv_require("coxme", "for Gaussian participant-frailty Cox regression")
    if (ties == "exact") stop("`coxme` supports Breslow/Efron ties, not exact ties; choose ties='breslow' or ties='efron'.", call.=FALSE)
    f <- stats::as.formula(paste0(
      "survival::Surv(analysis_time, event_observed) ~ ", rhs,
      " + (1 | `", participant_col, "`)"
    ))
    fit <- coxme::coxme(f, data=d, ties=ties)
    backend <- "coxme::coxme"
    repeated <- paste0("gaussian_frailty:", participant_col)
  }

  structure(list(
    model_family=if (structure == "frailty") "cox_frailty" else "cox_repeated",
    backend=backend, fit=fit, data=d, formula=formula, participant_col=participant_col,
    repeated_structure=repeated,
    provenance=.gaze_surv_provenance(d, as.character(formula), paste0("Cox ", structure))
  ), class="eye_gaze_survival_model")
}

#' Fit Weibull or log-normal AFT gaze-latency model
#' @export
fit_gaze_aft_model <- function(data, formula, distribution = NULL, ...) {
  .gaze_surv_require("survival", "for AFT regression")
  d <- .gaze_surv_analysis_rows(data)
  if (is.null(distribution)) stop("`distribution` must be specified explicitly as 'weibull' or 'lognormal'.", call. = FALSE)
  distribution <- match.arg(distribution, c("weibull", "lognormal"))
  if (any(d$analysis_time <= 0)) {
    stop(
      "AFT models require strictly positive analysis_time; zero-time events must be resolved ",
      "or shifted by a pre-specified measurement-resolution rule.",
      call. = FALSE
    )
  }
  rhs <- .gaze_surv_rhs(formula)
  f <- stats::as.formula(paste0("survival::Surv(analysis_time, event_observed) ~ ", rhs))
  fit <- .gaze_surv_fit_with_warning_guard(
    survival::survreg(f, data = d, dist = distribution, ...),
    "AFT"
  )
  if (!isTRUE(fit$converged)) stop("AFT convergence failure; result is not treated as valid.", call. = FALSE)
  structure(
    list(
      model_family = paste0("aft_", distribution),
      backend = "survival::survreg",
      fit = fit,
      data = d,
      formula = formula,
      participant_col = "participant_id",
      repeated_structure = NULL,
      provenance = .gaze_surv_provenance(d, as.character(formula), paste0(distribution, " AFT"))
    ),
    class = "eye_gaze_survival_model"
  )
}

#' Tidy gaze-survival model effects
#' @export
tidy_gaze_survival_model <- function(model, conf_level=.95) {
  if (!inherits(model, "eye_gaze_survival_model")) stop("Expected an eye_gaze_survival_model.", call.=FALSE)
  fit <- model$fit
  z <- stats::qnorm(1 - (1-conf_level)/2)
  if (grepl("^cox", model$model_family)) {
    b <- if (identical(model$backend, "coxme::coxme")) coxme::fixef(fit) else stats::coef(fit)
    se <- sqrt(diag(stats::vcov(fit)))
    se <- se[seq_along(b)]
    stat <- b/se
    pv <- 2*stats::pnorm(abs(stat), lower.tail=FALSE)
    data.frame(
      term=names(b), estimate_log_scale=as.numeric(b), std_error=as.numeric(se),
      hazard_ratio=exp(b), conf_low=exp(b-z*se), conf_high=exp(b+z*se),
      statistic=stat, p_value=pv, effect_measure="hazard_ratio",
      row.names=NULL, check.names=FALSE
    )
  } else {
    b <- stats::coef(fit)
    se <- sqrt(diag(stats::vcov(fit)))[seq_along(b)]
    stat <- b/se
    pv <- 2*stats::pnorm(abs(stat), lower.tail=FALSE)
    data.frame(
      term=names(b), estimate_log_scale=as.numeric(b), std_error=as.numeric(se),
      time_ratio=exp(b), conf_low=exp(b-z*se), conf_high=exp(b+z*se),
      statistic=stat, p_value=pv, effect_measure="time_ratio",
      row.names=NULL, check.names=FALSE
    )
  }
}

#' Proportional-hazards diagnostics
#' @export
check_gaze_proportional_hazards <- function(model, transform="km") {
  .gaze_surv_require("survival", "for Cox diagnostics")
  if (!inherits(model, "eye_gaze_survival_model") || !grepl("^cox", model$model_family)) stop("PH diagnostics require a Cox gaze-survival model.", call.=FALSE)
  if (identical(model$backend, "coxme::coxme")) {
    stop("PH diagnostics for coxme frailty fits are not available through survival::cox.zph; inspect the corresponding marginal Cox model for proportional-hazards diagnostics.", call.=FALSE)
  }
  z <- survival::cox.zph(model$fit, transform=transform)
  out <- data.frame(
    term=rownames(z$table),
    rho=if("rho" %in% colnames(z$table)) z$table[,"rho"] else NA_real_,
    chisq=z$table[,"chisq"],
    p_value=z$table[,"p"],
    row.names=NULL, check.names=FALSE
  )
  attr(out, "method") <- "survival::cox.zph"
  out
}

#' Compare gaze-survival models without choosing a winner
#' @export
compare_gaze_survival_models <- function(...) {
  models <- list(...)
  if (!length(models)) stop("Supply one or more models.", call.=FALSE)
  rows <- lapply(seq_along(models), function(i) {
    m <- models[[i]]
    if(!inherits(m,"eye_gaze_survival_model")) stop("All inputs must be gaze-survival models.", call.=FALSE)
    ll_obj <- stats::logLik(m$fit)
    ll <- as.numeric(ll_obj)
    k <- attr(ll_obj,"df")
    n <- tryCatch(stats::nobs(m$fit), error=function(e) nrow(m$data))
    basis <- if (grepl("^cox", m$model_family)) "cox_partial_likelihood" else "full_likelihood"
    data.frame(
      model=paste0("model_",i), family=m$model_family, backend=m$backend,
      logLik=ll, AIC=-2*ll+2*k, BIC=-2*ll+log(n)*k, n=n,
      likelihood_basis=basis, stringsAsFactors=FALSE
    )
  })
  out <- do.call(rbind, rows)
  comparable <- length(unique(out$likelihood_basis)) == 1L && length(unique(out$n)) == 1L
  out$information_criteria_comparable <- comparable
  if (!comparable) warning(
    "Information criteria are not directly comparable across Cox partial-likelihood and AFT full-likelihood models or across different analysis-row counts. Use diagnostics and estimand-specific interpretation instead of ranking by AIC/BIC.",
    call.=FALSE
  )
  out
}

#' Predict gaze-survival probabilities
#' @export
predict_gaze_survival <- function(model, newdata, times) {
  .gaze_surv_require("survival", "for survival prediction")
  if (!inherits(model, "eye_gaze_survival_model")) stop("Expected an eye_gaze_survival_model.", call. = FALSE)
  if (!is.data.frame(newdata) || !nrow(newdata)) stop("`newdata` must be a non-empty data.frame.", call. = FALSE)
  if (!is.numeric(times) || !length(times) || any(!is.finite(times)) || any(times < 0)) stop("Prediction times must be finite and non-negative.", call. = FALSE)
  if (identical(model$backend, "coxme::coxme")) {
    stop(
      "Direct survival-probability prediction for the latent-frailty `coxme` fit is not exposed by this helper. ",
      "Use the explicitly requested marginal Cox or AFT model for survival-probability curves.",
      call. = FALSE
    )
  }
  if (grepl("^cox", model$model_family)) {
    sf <- survival::survfit(model$fit, newdata = newdata)
    rows <- vector("list", nrow(newdata))
    for (i in seq_len(nrow(newdata))) {
      s <- summary(sf[i], times = times, extend = TRUE)
      rows[[i]] <- data.frame(row = i - 1L, time = s$time, survival = s$surv)
    }
    return(do.call(rbind, rows))
  }

  dist <- if (model$model_family == "aft_weibull") "weibull" else "lognormal"
  lp <- as.numeric(stats::predict(model$fit, newdata = newdata, type = "lp"))
  sc <- model$fit$scale
  rows <- list()
  k <- 0L
  for (i in seq_along(lp)) {
    for (tt in times) {
      k <- k + 1L
      sv <- if (tt <= 0) {
        1
      } else if (identical(dist, "weibull")) {
        exp(-exp((log(tt) - lp[i]) / sc))
      } else {
        stats::pnorm((log(tt) - lp[i]) / sc, lower.tail = FALSE)
      }
      rows[[k]] <- data.frame(row = i - 1L, time = tt, survival = sv)
    }
  }
  do.call(rbind, rows)
}

#' Estimate gaze-latency quantiles
#' @export
estimate_gaze_latency_quantiles <- function(object, probs = c(.25, .5, .75), group = NULL, newdata = NULL) {
  if (any(probs <= 0 | probs >= 1)) stop("`probs` must lie strictly between 0 and 1.", call. = FALSE)
  if (is.data.frame(object)) {
    km <- estimate_gaze_survival(object, group = group)
    spl <- split(km, km$group, drop = TRUE)
    return(do.call(rbind, lapply(names(spl), function(g) {
      do.call(rbind, lapply(probs, function(p) {
        q <- spl[[g]][spl[[g]]$survival <= 1 - p, , drop = FALSE]
        data.frame(group = g, prob = p, quantile = if (nrow(q)) q$time[1L] else NA_real_)
      }))
    })))
  }
  if (!inherits(object, "eye_gaze_survival_model")) stop("Expected survival data or a fitted gaze-survival model.", call. = FALSE)
  if (identical(object$backend, "coxme::coxme")) stop("Latency-quantile prediction is not exposed for the latent-frailty `coxme` helper; use KM, marginal Cox, or AFT quantiles explicitly.", call. = FALSE)
  if (is.null(newdata)) newdata <- object$data[1L, , drop = FALSE]
  if (grepl("^aft", object$model_family)) {
    q <- stats::predict(object$fit, newdata = newdata, type = "quantile", p = probs)
    if (is.null(dim(q))) q <- matrix(q, nrow = nrow(newdata), byrow = TRUE)
    out <- expand.grid(row = seq_len(nrow(newdata)) - 1L, prob = probs)
    out$quantile <- as.vector(t(q))
    return(out)
  }
  pred <- predict_gaze_survival(object, newdata, sort(unique(object$data$analysis_time)))
  do.call(rbind, lapply(unique(pred$row), function(i) {
    do.call(rbind, lapply(probs, function(p) {
      z <- pred[pred$row == i & pred$survival <= 1 - p, , drop = FALSE]
      data.frame(row = i, prob = p, quantile = if (nrow(z)) z$time[1L] else NA_real_)
    }))
  }))
}

#' Plot gaze-survival curve
#' @export
plot_gaze_survival_curve <- function(data, group = NULL, ...) {
  km <- estimate_gaze_survival(data, group = group)
  spl <- split(km, km$group, drop = TRUE)
  lty <- seq_along(spl)
  graphics::plot(
    NA, xlim = range(c(0, km$time)), ylim = c(0, 1),
    xlab = "Latency", ylab = "P(target event not yet observed)", ...
  )
  for (i in seq_along(spl)) {
    z <- spl[[i]]
    graphics::lines(c(0, z$time), c(1, z$survival), type = "s", lty = lty[i], lwd = 2)
  }
  if (length(spl) > 1L) {
    graphics::legend("topright", legend = names(spl), lty = lty, lwd = 2, bty = "n")
  }
  invisible(km)
}

#' Plot gaze cumulative incidence (1-KM)
#' @export
plot_gaze_cumulative_incidence <- function(data, group = NULL, ...) {
  km <- estimate_gaze_survival(data, group = group)
  spl <- split(km, km$group, drop = TRUE)
  lty <- seq_along(spl)
  graphics::plot(
    NA, xlim = range(c(0, km$time)), ylim = c(0, 1),
    xlab = "Latency", ylab = "Cumulative target-event incidence (1-KM)", ...
  )
  for (i in seq_along(spl)) {
    z <- spl[[i]]
    graphics::lines(c(0, z$time), c(0, 1 - z$survival), type = "s", lty = lty[i], lwd = 2)
  }
  if (length(spl) > 1L) {
    graphics::legend("bottomright", legend = names(spl), lty = lty, lwd = 2, bty = "n")
  }
  invisible(km)
}

#' Plot descriptive gaze hazard increments
#' @export
plot_gaze_hazard <- function(data, group = NULL, ...) {
  d <- .gaze_surv_analysis_rows(data)
  if (is.null(group)) d$.group <- "all" else {
    if (!group %in% names(d)) stop("Unknown group column.", call. = FALSE)
    d$.group <- d[[group]]
  }
  spl <- split(d, d$.group, drop = TRUE)
  all_t <- sort(unique(d$analysis_time[d$event_observed == 1]))
  if (!length(all_t)) stop("No observed events are available for a hazard plot.", call. = FALSE)
  graphics::plot(
    NA, xlim = range(all_t), ylim = c(0, 1),
    xlab = "Latency", ylab = "Nelson-Aalen hazard increment", ...
  )
  lty <- seq_along(spl)
  for (i in seq_along(spl)) {
    z <- spl[[i]]
    tt <- sort(unique(z$analysis_time[z$event_observed == 1]))
    if (!length(tt)) next
    inc <- vapply(
      tt,
      function(t) sum(z$analysis_time == t & z$event_observed == 1) / sum(z$analysis_time >= t),
      numeric(1L)
    )
    graphics::lines(tt, inc, type = "s", lty = lty[i], lwd = 2)
  }
  if (length(spl) > 1L) {
    graphics::legend("topright", legend = names(spl), lty = lty, lwd = 2, bty = "n")
  }
  invisible(NULL)
}

#' Plot Cox diagnostics
#' @export
plot_gaze_cox_diagnostics <- function(model, ...) {
  .gaze_surv_require("survival", "for Cox diagnostics")
  if (!inherits(model, "eye_gaze_survival_model") || !grepl("^cox", model$model_family)) stop("Cox diagnostics require a Cox model.", call. = FALSE)
  if (identical(model$backend, "coxme::coxme")) stop("Plot PH diagnostics from the corresponding marginal/cluster-robust Cox model; this helper does not silently substitute a diagnostic for a coxme frailty fit.", call. = FALSE)
  z <- survival::cox.zph(model$fit)
  graphics::plot(z, ...)
  invisible(z)
}

#' Compare named gaze-survival sensitivity specifications
#'
#' Fits only the explicitly requested model families across separately prepared
#' survival tables. No estimator or sensitivity branch is selected silently.
#' @export
compare_gaze_survival_specifications <- function(
    specifications,
    formula,
    model_families,
    participant_col = "participant_id",
    ties = "breslow") {
  if (!is.list(specifications) || !length(specifications)) stop("`specifications` must be a non-empty named list of survival tables.", call. = FALSE)
  if (is.null(names(specifications)) || any(!nzchar(names(specifications)))) stop("Every sensitivity specification must have a name.", call. = FALSE)
  allowed <- c("cox", "cox_cluster_robust", "cox_frailty", "aft_weibull", "aft_lognormal")
  if (!length(model_families) || any(!model_families %in% allowed)) stop("`model_families` must explicitly contain supported estimator names.", call. = FALSE)

  rows <- list()
  k <- 0L
  for (spec_name in names(specifications)) {
    d <- specifications[[spec_name]]
    if (!is.data.frame(d)) stop("Each sensitivity specification must be a data.frame.", call. = FALSE)
    cs <- summarise_gaze_censoring(d)[1L, , drop = FALSE]
    for (family in model_families) {
      fit <- switch(
        family,
        cox = fit_gaze_cox_model(d, formula, ties = ties),
        cox_cluster_robust = fit_gaze_mixed_cox_model(d, formula, participant_col = participant_col, structure = "cluster_robust", ties = ties),
        cox_frailty = fit_gaze_mixed_cox_model(d, formula, participant_col = participant_col, structure = "frailty", ties = ties),
        aft_weibull = fit_gaze_aft_model(d, formula, distribution = "weibull"),
        aft_lognormal = fit_gaze_aft_model(d, formula, distribution = "lognormal")
      )
      tidy <- tidy_gaze_survival_model(fit)
      tidy$specification <- spec_name
      tidy$model_family <- fit$model_family
      tidy$n_trials <- as.integer(cs$n_trials)
      tidy$n_observed_events <- as.integer(cs$n_observed_events)
      tidy$n_censored <- as.integer(cs$n_censored)
      tidy$censoring_fraction <- as.numeric(cs$censoring_fraction)
      for (field in intersect(c("event_detector", "aoi_specification", "quality_rules", "preprocessing_specification", "time_origin"), names(d))) {
        tidy[[field]] <- paste(sort(unique(as.character(stats::na.omit(d[[field]])))), collapse = " | ")
      }
      k <- k + 1L
      rows[[k]] <- tidy
    }
  }
  out <- do.call(rbind, rows)
  out <- out[, c("specification", "model_family", setdiff(names(out), c("specification", "model_family"))), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Concise gaze-survival reporting bundle
#' @export
report_gaze_survival_model <- function(model, conf_level = .95) {
  if (!inherits(model, "eye_gaze_survival_model")) stop("Expected an eye_gaze_survival_model.", call. = FALSE)
  d <- model$data
  cs <- summarise_gaze_censoring(d)[1L, , drop = FALSE]
  diagnostic <- NULL
  if (grepl("^cox", model$model_family)) {
    if (identical(model$backend, "coxme::coxme")) {
      diagnostic <- "PH diagnostics require the corresponding marginal Cox model; cox.zph is not applied to coxme frailty fits."
    } else {
      ph <- check_gaze_proportional_hazards(model)
      diagnostic <- if (any(ph$p_value < .05, na.rm = TRUE)) "flagged PH diagnostic" else "no PH diagnostic flag at alpha=.05"
    }
  }
  list(
    N_participants = length(unique(d$participant_id)),
    N_trials = nrow(d),
    N_observed_events = as.integer(cs$n_observed_events),
    N_censored_trials = as.integer(cs$n_censored),
    censoring_percentage = 100 * as.numeric(cs$censoring_fraction),
    model_family = model$model_family,
    backend = model$backend,
    effect_measure = if (grepl("^cox", model$model_family)) "hazard ratio" else "time ratio",
    effects = tidy_gaze_survival_model(model, conf_level),
    random_or_frailty_structure = model$repeated_structure,
    diagnostic_result = diagnostic,
    provenance = model$provenance
  )
}
#' Synthetic gaze-survival example data
#' @export
simulate_gaze_survival_example <- function(
    kind = c("disclosure", "verification"),
    seed = 20260918,
    n_participants = 36L,
    trials_per_participant = 3L) {
  kind <- match.arg(kind)
  inputs <- simulate_gaze_survival_inputs(
    kind,
    seed = seed,
    n_participants = n_participants,
    trials_per_participant = trials_per_participant
  )
  target <- if (kind == "disclosure") "disclosure" else "source_evidence"
  event_type <- if (kind == "disclosure") "first_fixation" else "first_aoi_entry"
  prepare_gaze_survival_data(
    inputs$trials,
    inputs$events,
    target_aoi = target,
    event_type = event_type,
    condition_col = "condition_id",
    observation_end_reason_col = "observation_end_reason",
    source_data = paste0("synthetic_", kind, "_trial_event_inputs"),
    preprocessing_specification = "synthetic_truth_no_filtering",
    event_detector = "synthetic_truth",
    aoi_specification = paste0("fixed synthetic AOI:", target),
    quality_rules = list(valid_fraction_min = .90, synthetic_truth = TRUE)
  )
}