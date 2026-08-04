simulate_eye_dataset <- function(
    n_person = 30,
    n_item = 10,
    sampling_rate = 60,
    trial_duration = 2,
    samples_per_trial = NULL,
    include_pupil = TRUE,
    include_biometrics = TRUE,
    missing_gaze = 0.05,
    missing_pupil = 0.08,
    seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n_person <- as.integer(n_person); n_item <- as.integer(n_item)
  if (n_person < 2L || n_item < 2L) .eye_stop("Simulation requires at least two persons and two items.")
  persons <- paste0("P", sprintf("%03d", seq_len(n_person)))
  items <- paste0("I", sprintf("%03d", seq_len(n_item)))
  ability <- stats::rnorm(n_person)
  speed <- -0.3 * ability + sqrt(1 - 0.3^2) * stats::rnorm(n_person)
  difficulty <- stats::rnorm(n_item)
  discrimination <- stats::rlnorm(n_item, 0, 0.2)
  time_intensity <- stats::rnorm(n_item, log(trial_duration), 0.2)
  recordings <- data.frame(
    recording_id = paste0("rec_", persons), participant_id = persons, session_id = "S001",
    vendor = "simulated", vendor_family = "eyeprocess", device_model = "synthetic_tracker",
    firmware_version = "1", software_name = "eyeprocess", software_version = "development",
    experiment_type = "simulated item assessment", nominal_sampling_rate = sampling_rate,
    screen_width_px = 1920, screen_height_px = 1080, recording_start = NA_character_,
    source_timezone = "UTC", source_file_set = "<simulated>", stringsAsFactors = FALSE
  )
  coord <- new_coordinate_space("coord_sim_norm", "display_normalized_top_left", width = 1, height = 1, reference_object = "simulated_display")
  streams <- do.call(rbind, lapply(recordings$recording_id, function(rec) {
    types <- "gaze_combined"
    rates <- sampling_rate
    units <- NA_character_
    coords <- "coord_sim_norm"
    if (include_pupil) {
      types <- c(types, "pupil_left", "pupil_right")
      rates <- c(rates, sampling_rate, sampling_rate)
      units <- c(units, "millimetres", "millimetres")
      coords <- c(coords, NA_character_, NA_character_)
    }
    if (include_biometrics) {
      types <- c(types, "eda", "heart_rate")
      rates <- c(rates, 10, 1)
      units <- c(units, "microsiemens", "beats_per_minute")
      coords <- c(coords, NA_character_, NA_character_)
    }
    data.frame(
      stream_id = paste0(rec, "_", ifelse(types == "gaze_combined", "gaze", types)), recording_id = rec,
      stream_type = types, source_device = "synthetic_tracker", source_clock = "simulation",
      sampling_type = "sampled", nominal_rate_hz = rates, observed_rate_hz = rates,
      timestamp_unit = "seconds", value_unit = units, coordinate_space_id = coords,
      processing_level = "simulated_raw", stringsAsFactors = FALSE
    )
  }))
  intervals <- list(); responses <- list(); events <- list(); gaze <- list(); eyes <- list(); bio <- list()
  ik <- rk <- ek <- gk <- pk <- bk <- 0L
  for (p in seq_len(n_person)) {
    rec <- paste0("rec_", persons[p]); current <- 0
    for (j in seq_len(n_item)) {
      trial <- paste0(rec, "_trial_", sprintf("%03d", j))
      eta <- discrimination[j] * (ability[p] - difficulty[j])
      prob <- stats::plogis(eta)
      score <- stats::rbinom(1, 1, prob)
      rt <- exp(time_intensity[j] - 0.25 * speed[p] + stats::rnorm(1, 0, 0.12))
      duration <- max(trial_duration, rt + 0.2)
      start <- current; end <- start + duration; current <- end + 0.5
      ik <- ik + 1L
      intervals[[ik]] <- data.frame(
        interval_id = paste0(trial, "_interval"), recording_id = rec, interval_type = "trial",
        start_time = start, end_time = end, trial_id = trial, participant_id = persons[p],
        item_id = items[j], stimulus_id = paste0("stim_", items[j]), condition_id = ifelse(j %% 2, "A", "B"),
        parent_interval_id = NA_character_, valid_interval = TRUE, stringsAsFactors = FALSE
      )
      rk <- rk + 1L
      responses[[rk]] <- data.frame(
        response_id = paste0(trial, "_response"), recording_id = rec, participant_id = persons[p],
        trial_id = trial, item_id = items[j], response = as.character(score), score = score,
        response_time = rt, response_timestamp = start + rt, response_type = "binary", valid_response = TRUE,
        stringsAsFactors = FALSE
      )
      for (type in c("TRIAL_START", "TRIAL_END", "RESPONSE")) {
        ek <- ek + 1L
        etime <- switch(type, TRIAL_START = start, TRIAL_END = end, RESPONSE = start + rt)
        events[[ek]] <- data.frame(
          event_id = paste0(trial, "_", tolower(type)), recording_id = rec,
          timestamp_native = etime, timestamp_seconds = etime, event_type = tolower(type),
          event_name = type, event_value = trial, duration = NA_real_, source = "simulation",
          native_record = NA_character_, trial_id = trial, stimulus_id = paste0("stim_", items[j]),
          stringsAsFactors = FALSE
        )
      }
      n <- if (is.null(samples_per_trial)) max(5L, round(duration * sampling_rate)) else max(5L, as.integer(samples_per_trial))
      tt <- seq(start, end, length.out = n)
      rel <- (tt - start) / duration
      # Visual trajectory shifts from prompt to options and evidence.
      state <- ifelse(rel < 0.35, "prompt", ifelse(rel < 0.75, "options", "evidence"))
      state[sample.int(n, max(1L, floor(n * (0.05 + 0.08 * (1 - score)))))] <- "prompt"
      centers <- list(prompt = c(0.25, 0.35), options = c(0.70, 0.55), evidence = c(0.50, 0.82))
      xy <- t(vapply(state, function(s) centers[[s]], numeric(2)))
      difficulty_noise <- 0.02 + 0.015 * stats::plogis(difficulty[j] - ability[p])
      gx <- xy[, 1L] + stats::rnorm(n, 0, difficulty_noise)
      gy <- xy[, 2L] + stats::rnorm(n, 0, difficulty_noise)
      valid <- stats::runif(n) > missing_gaze
      gx[!valid] <- NA; gy[!valid] <- NA
      gk <- gk + 1L
      gaze[[gk]] <- data.frame(
        recording_id = rec, stream_id = paste0(rec, "_gaze"),
        sample_id = paste0(trial, "_sample_", sprintf("%05d", seq_len(n))),
        timestamp_native = tt, timestamp_seconds = tt, gaze_x = gx, gaze_y = gy, gaze_z = NA_real_,
        azimuth_deg = NA_real_, elevation_deg = NA_real_, valid = valid, confidence = as.numeric(valid),
        fixation_id_source = NA_character_, blink_id_source = NA_character_, trial_id = trial,
        stimulus_id = paste0("stim_", items[j]), coordinate_space_id = "coord_sim_norm",
        true_aoi = state, stringsAsFactors = FALSE
      )
      if (include_pupil) {
        load <- stats::plogis(difficulty[j] - ability[p])
        for (eye in c("left", "right")) {
          pupil <- 3.4 + 0.15 * load + 0.25 * (rel^2 * exp(-4 * rel)) * 10 + stats::rnorm(n, 0, 0.035)
          miss <- stats::runif(n) < missing_pupil
          pupil[miss] <- NA
          pk <- pk + 1L
          eyes[[pk]] <- data.frame(
            recording_id = rec, sample_id = paste0(trial, "_eye_", eye, "_", sprintf("%05d", seq_len(n))),
            timestamp_native = tt, timestamp_seconds = tt, eye = eye,
            pupil_diameter = pupil, pupil_unit = "millimetres", pupil_valid = !miss,
            eye_openness = ifelse(miss, 0, 1), gaze_origin_x = NA_real_, gaze_origin_y = NA_real_, gaze_origin_z = NA_real_,
            gaze_origin_valid = NA, corneal_reflection_x = NA_real_, corneal_reflection_y = NA_real_,
            detector_method = "simulation", confidence = ifelse(miss, 0, 1), trial_id = trial,
            stimulus_id = paste0("stim_", items[j]), stringsAsFactors = FALSE
          )
        }
      }
      if (include_biometrics) {
        for (channel in c("eda", "heart_rate")) {
          rate <- if (channel == "eda") 10 else 1
          tb <- seq(start, end, by = 1 / rate)
          value <- if (channel == "eda") 2 + 0.25 * stats::plogis(difficulty[j] - ability[p]) + 0.08 * sin((tb - start) * 2 * pi) + stats::rnorm(length(tb), 0, 0.03) else 70 + 3 * stats::plogis(difficulty[j] - ability[p]) + stats::rnorm(length(tb), 0, 0.8)
          bk <- bk + 1L
          bio[[bk]] <- data.frame(
            recording_id = rec, stream_id = paste0(rec, "_", channel), timestamp_native = tb,
            timestamp_seconds = tb, channel = channel, value = value,
            unit = if (channel == "eda") "microsiemens" else "beats_per_minute",
            valid = TRUE, processing_level = "simulated_raw", source_device = "synthetic_biometrics",
            trial_id = trial, stimulus_id = paste0("stim_", items[j]), stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  out <- new_eye_dataset(
    recordings = recordings, streams = streams,
    gaze_samples = do.call(.bind_rows_base, gaze),
    eye_samples = if (length(eyes)) do.call(.bind_rows_base, eyes) else NULL,
    events = do.call(.bind_rows_base, events), intervals = do.call(.bind_rows_base, intervals),
    responses = do.call(.bind_rows_base, responses), coordinate_spaces = coord,
    biometrics = if (length(bio)) do.call(.bind_rows_base, bio) else NULL,
    vendor_metadata = list(simulation_truth = list(
      ability = setNames(ability, persons), speed = setNames(speed, persons),
      difficulty = setNames(difficulty, items), discrimination = setNames(discrimination, items),
      time_intensity = setNames(time_intensity, items)
    )), validate = FALSE
  )
  out <- register_aois(out,
    new_aoi("prompt", "Prompt", shape = "rectangle", x = 0.05, y = 0.10, width = 0.40, height = 0.50, coordinate_space_id = "coord_sim_norm"),
    new_aoi("options", "Options", shape = "rectangle", x = 0.50, y = 0.25, width = 0.45, height = 0.50, coordinate_space_id = "coord_sim_norm"),
    new_aoi("evidence", "Evidence", shape = "rectangle", x = 0.20, y = 0.68, width = 0.60, height = 0.28, coordinate_space_id = "coord_sim_norm")
  )
  out <- assign_aois(out)
  out <- add_provenance(out, "simulate_eye_dataset", "dataset", paste0(
    "n_person=", n_person, ";n_item=", n_item, ";sampling_rate=", sampling_rate,
    ";samples_per_trial=", samples_per_trial %||% "rate_derived"
  ))
  attr(out, "validation") <- validate_eye_dataset(out)
  out
}

simulate_process_irt <- function(
    n_person = 200,
    n_item = 20,
    gaze_effect = 0.4,
    pupil_effect = 0.3,
    ability_speed_correlation = -0.3,
    missing_process = 0.1,
    seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  persons <- paste0("P", seq_len(n_person)); items <- paste0("I", seq_len(n_item))
  Sigma <- matrix(c(1, ability_speed_correlation, ability_speed_correlation, 1), 2)
  z <- matrix(stats::rnorm(n_person * 2), ncol = 2) %*% chol(Sigma)
  theta <- z[, 1L]; speed <- z[, 2L]
  b <- stats::rnorm(n_item); a <- stats::rlnorm(n_item, 0, 0.15); intensity <- stats::rnorm(n_item, 0.5, 0.2)
  grid <- expand.grid(participant_id = persons, item_id = items, stringsAsFactors = FALSE)
  pi <- match(grid$participant_id, persons); ii <- match(grid$item_id, items)
  shared <- stats::rnorm(nrow(grid))
  gaze <- gaze_effect * shared + stats::rnorm(nrow(grid), sd = sqrt(max(0.01, 1 - gaze_effect^2)))
  pupil <- pupil_effect * shared + stats::rnorm(nrow(grid), sd = sqrt(max(0.01, 1 - pupil_effect^2)))
  eta <- a[ii] * (theta[pi] - b[ii]) + 0.25 * shared
  grid$score <- stats::rbinom(nrow(grid), 1, stats::plogis(eta))
  grid$response_time <- exp(intensity[ii] - 0.3 * speed[pi] + 0.15 * shared + stats::rnorm(nrow(grid), 0, 0.2))
  grid$gaze_process <- gaze; grid$pupil_process <- pupil; grid$shared_process <- shared
  miss <- stats::runif(nrow(grid)) < missing_process
  grid$gaze_process[miss] <- NA; grid$pupil_process[miss] <- NA
  list(
    data = grid,
    truth = list(theta = setNames(theta, persons), speed = setNames(speed, persons),
      difficulty = setNames(b, items), discrimination = setNames(a, items),
      intensity = setNames(intensity, items), gaze_effect = gaze_effect, pupil_effect = pupil_effect)
  )
}

parameter_recovery <- function(
    simulator,
    estimator,
    extractor,
    truth_extractor,
    replications = 100,
    seed = 1,
    ...) {
  if (!is.function(simulator) || !is.function(estimator) || !is.function(extractor) || !is.function(truth_extractor)) {
    .eye_stop("Simulator, estimator, extractor, and truth_extractor must be functions.")
  }
  set.seed(seed)
  results <- vector("list", replications)
  for (r in seq_len(replications)) {
    sim <- simulator(...)
    fit <- tryCatch(estimator(sim), error = function(e) e)
    if (inherits(fit, "error")) {
      results[[r]] <- data.frame(replication = r, parameter = NA_character_, estimate = NA_real_, truth = NA_real_, error = fit$message)
      next
    }
    est <- extractor(fit); truth <- truth_extractor(sim)
    common <- intersect(names(est), names(truth))
    results[[r]] <- data.frame(
      replication = r, parameter = common, estimate = as.numeric(est[common]),
      truth = as.numeric(truth[common]), error = NA_character_, stringsAsFactors = FALSE
    )
  }
  out <- do.call(.bind_rows_base, results)
  if (nrow(out)) {
    out$bias <- out$estimate - out$truth
    out$squared_error <- out$bias^2
  }
  structure(out, class = c("eye_parameter_recovery", "data.frame"))
}

summary.eye_parameter_recovery <- function(object, ...) {
  d <- object[is.finite(object$estimate) & is.finite(object$truth), ]
  if (!nrow(d)) return(data.frame())
  stats::aggregate(cbind(bias, squared_error) ~ parameter, d, function(z) c(mean = mean(z), sd = stats::sd(z)))
}

plot.eye_parameter_recovery <- function(x, ...) {
  d <- x[is.finite(x$estimate) & is.finite(x$truth), ]
  if (!nrow(d)) return(.plot_empty("No successful recovery estimates.", "Parameter recovery"))
  graphics::plot(d$truth, d$estimate, xlab = "True value", ylab = "Estimate", main = "Parameter recovery", ...)
  graphics::abline(0, 1, lty = 2)
  invisible(d)
}

power_process_simulation <- function(
    n_person,
    n_item,
    effect,
    replications = 200,
    alpha = 0.05,
    seed = 1) {
  set.seed(seed)
  scenarios <- expand.grid(n_person = n_person, n_item = n_item, effect = effect)
  results <- vector("list", nrow(scenarios))
  for (s in seq_len(nrow(scenarios))) {
    detected <- logical(replications)
    estimates <- numeric(replications)
    for (r in seq_len(replications)) {
      sim <- simulate_process_irt(scenarios$n_person[s], scenarios$n_item[s], gaze_effect = scenarios$effect[s])$data
      fit <- stats::glm(score ~ gaze_process + participant_id + item_id, family = stats::binomial(), data = sim)
      co <- summary(fit)$coefficients
      detected[r] <- "gaze_process" %in% rownames(co) && co["gaze_process", 4L] < alpha
      estimates[r] <- if ("gaze_process" %in% rownames(co)) co["gaze_process", 1L] else NA_real_
    }
    results[[s]] <- data.frame(
      n_person = scenarios$n_person[s], n_item = scenarios$n_item[s], effect = scenarios$effect[s],
      power = mean(detected), mean_estimate = mean(estimates, na.rm = TRUE),
      replications = replications, alpha = alpha, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, results)
}
