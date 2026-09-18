# Event-detector multiverse and inference robustness -------------------------
# Vendor-neutral core. Vendor-specific convenience belongs in adapter packages.

.edm_stop <- function(...) stop(..., call. = FALSE)
.edm_warn <- function(...) warning(..., call. = FALSE)
.edm_scalar_chr <- function(x, name) {
  x <- as.character(x)[1L]
  if (is.na(x) || !nzchar(trimws(x))) .edm_stop("`", name, "` must be a non-empty scalar string.")
  trimws(x)
}
.edm_positive <- function(x, name, allow_null = TRUE) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    if (allow_null) return(NULL)
    .edm_stop("`", name, "` is required.")
  }
  x <- as.numeric(x)[1L]
  if (!is.finite(x) || x <= 0) .edm_stop("`", name, "` must be finite and > 0.")
  x
}
.edm_hash <- function(x) {
  if (exists("object_hash", mode = "function")) return(object_hash(x))
  raw <- serialize(x, NULL, version = 2)
  tf <- tempfile(); on.exit(unlink(tf), add = TRUE)
  writeBin(raw, tf)
  unname(tools::md5sum(tf))
}
.edm_rbind_fill <- function(xs) {
  xs <- Filter(function(x) is.data.frame(x) && nrow(x), xs)
  if (!length(xs)) return(data.frame())
  nms <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    miss <- setdiff(nms, names(x)); for (nm in miss) x[[nm]] <- NA
    x[, nms, drop = FALSE]
  })
  out <- do.call(rbind, xs); rownames(out) <- NULL; out
}
.edm_frame_hash <- function(x) if (is.null(x) || !is.data.frame(x) || !nrow(x)) NA_character_ else .edm_hash(x)
.edm_dataset_hash <- function(x) {
  pieces <- lapply(c("recordings", "gaze_samples", "intervals", "aoi_definitions", "aoi_geometry"), function(nm) x[[nm]])
  .edm_hash(pieces)
}
.edm_lineage <- function(x, spec = NULL) {
  aoi <- .edm_rbind_fill(list(x$aoi_definitions, x$aoi_geometry))
  list(
    source_data_hash = .edm_dataset_hash(x),
    preprocessing_provenance_hash = .edm_frame_hash(x$provenance),
    aoi_spec_hash = .edm_frame_hash(aoi),
    quality_spec_hash = .edm_frame_hash(x$quality),
    detector_spec_hash = if (is.null(spec)) NA_character_ else spec$detector_spec_hash,
    detector_implementation = if (is.null(spec)) NA_character_ else spec$implementation,
    detector_implementation_version = if (is.null(spec)) NA_character_ else spec$implementation_version,
    software = "eyeprocess",
    software_version = tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_)
  )
}

#' Define an event-detector specification
#'
#' Thresholds are intentionally not supplied as universal defaults.
#' @export
define_event_detector_spec <- function(
    detector_id,
    algorithm,
    velocity_threshold = NULL,
    dispersion_threshold = NULL,
    minimum_duration_ms = NULL,
    maximum_gap_ms = NULL,
    merge_rule = "none",
    sampling_rate = NULL,
    smoothing = NULL,
    filter = NULL,
    coordinate_unit = "degrees",
    implementation = NULL,
    implementation_version = NULL,
    parameters = list(),
    callback = NULL) {
  detector_id <- .edm_scalar_chr(detector_id, "detector_id")
  algorithm <- tolower(gsub("-", "_", .edm_scalar_chr(algorithm, "algorithm")))
  if (algorithm == "i_vt") algorithm <- "ivt"
  if (algorithm == "i_dt") algorithm <- "idt"
  if (is.null(implementation)) implementation <- if (algorithm == "remodnav") "REMoDNaV" else "eyeprocess"
  if (!is.list(parameters)) .edm_stop("`parameters` must be a named list.")
  if (length(parameters) && (is.null(names(parameters)) || any(!nzchar(names(parameters))))) .edm_stop("`parameters` must be named.")
  core <- list(
    detector_id = detector_id,
    algorithm = algorithm,
    velocity_threshold = if (is.null(velocity_threshold)) NULL else as.numeric(velocity_threshold)[1L],
    dispersion_threshold = if (is.null(dispersion_threshold)) NULL else as.numeric(dispersion_threshold)[1L],
    minimum_duration_ms = if (is.null(minimum_duration_ms)) NULL else as.numeric(minimum_duration_ms)[1L],
    maximum_gap_ms = if (is.null(maximum_gap_ms)) NULL else as.numeric(maximum_gap_ms)[1L],
    merge_rule = .edm_scalar_chr(merge_rule, "merge_rule"),
    sampling_rate = if (is.null(sampling_rate)) NULL else as.numeric(sampling_rate)[1L],
    smoothing = if (is.null(smoothing)) NULL else as.character(smoothing)[1L],
    filter = if (is.null(filter)) NULL else as.character(filter)[1L],
    coordinate_unit = tolower(.edm_scalar_chr(coordinate_unit, "coordinate_unit")),
    implementation = .edm_scalar_chr(implementation, "implementation"),
    implementation_version = if (is.null(implementation_version)) NA_character_ else as.character(implementation_version)[1L],
    parameters = parameters,
    callback = callback
  )
  fingerprint_input <- core; fingerprint_input$callback <- NULL
  core$detector_spec_hash <- .edm_hash(fingerprint_input)
  class(core) <- "eye_event_detector_spec"
  validate_event_detector_spec(core)
  core
}

#' Validate an event-detector specification
#' @export
validate_event_detector_spec <- function(spec) {
  if (!inherits(spec, "eye_event_detector_spec")) .edm_stop("`spec` must be created by define_event_detector_spec().")
  algorithms <- c("ivt", "idt", "adaptive_velocity", "remodnav", "external", "vendor")
  if (!spec$algorithm %in% algorithms) .edm_stop("Unsupported detector algorithm.")
  if (!spec$coordinate_unit %in% c("degrees", "pixels", "normalized")) .edm_stop("`coordinate_unit` must be degrees, pixels, or normalized.")
  if (spec$algorithm %in% c("ivt", "idt", "adaptive_velocity", "remodnav")) {
    .edm_positive(spec$sampling_rate, "sampling_rate", FALSE)
    .edm_positive(spec$minimum_duration_ms, "minimum_duration_ms", FALSE)
  }
  if (spec$algorithm == "ivt") .edm_positive(spec$velocity_threshold, "velocity_threshold", FALSE)
  if (spec$algorithm == "idt") .edm_positive(spec$dispersion_threshold, "dispersion_threshold", FALSE)
  if (!is.null(spec$maximum_gap_ms)) .edm_positive(spec$maximum_gap_ms, "maximum_gap_ms", FALSE)
  if (spec$algorithm == "adaptive_velocity") {
    .edm_positive(spec$parameters$noise_factor, "parameters$noise_factor", FALSE)
    .edm_positive(spec$parameters$minimum_velocity_threshold, "parameters$minimum_velocity_threshold", FALSE)
  }
  if (spec$algorithm == "remodnav" && spec$coordinate_unit == "pixels") .edm_positive(spec$parameters$px2deg, "parameters$px2deg", FALSE)
  if (spec$algorithm == "external" && !is.function(spec$callback)) .edm_stop("External detector specs require a callable `callback`.")
  if (spec$algorithm == "vendor" && is.function(spec$callback)) .edm_stop("Vendor-event specs do not use `callback`.")
  invisible(TRUE)
}

#' Create a deterministic detector multiverse
#' @export
create_detector_multiverse <- function(specs = NULL, base_spec = NULL, parameter_grid = NULL,
                                       id_template = "%s_%03d", label = "event_detector_multiverse") {
  assembled <- if (is.null(specs)) list() else as.list(specs)
  if (!is.null(parameter_grid)) {
    if (!inherits(base_spec, "eye_event_detector_spec")) .edm_stop("`base_spec` is required with parameter_grid.")
    if (!is.list(parameter_grid) || !length(parameter_grid) || is.null(names(parameter_grid))) .edm_stop("`parameter_grid` must be a named non-empty list.")
    if (any(lengths(parameter_grid) == 0L)) .edm_stop("Every detector-grid dimension must contain at least one value.")
    grid <- expand.grid(parameter_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(grid))) {
      args <- unclass(base_spec); args$detector_spec_hash <- NULL; args$callback <- base_spec$callback
      for (nm in names(grid)) {
        val <- grid[[nm]][[i]]
        if (startsWith(nm, "parameters.")) args$parameters[[sub("^parameters\\.", "", nm)]] <- val
        else if (nm %in% names(args)) args[[nm]] <- val
        else args$parameters[[nm]] <- val
      }
      args$detector_id <- sprintf(id_template, base_spec$detector_id, i)
      assembled[[length(assembled) + 1L]] <- do.call(define_event_detector_spec, args)
    }
  }
  if (!length(assembled)) .edm_stop("Supply at least one detector specification.")
  invisible(lapply(assembled, validate_event_detector_spec))
  ids <- vapply(assembled, `[[`, character(1), "detector_id")
  if (anyDuplicated(ids)) .edm_stop("Detector ids must be unique within a multiverse.")
  ord <- order(ids, vapply(assembled, `[[`, character(1), "detector_spec_hash"))
  assembled <- assembled[ord]
  manifest <- .edm_rbind_fill(lapply(assembled, function(s) data.frame(
    detector_id = s$detector_id, algorithm = s$algorithm,
    velocity_threshold = if (is.null(s$velocity_threshold)) NA_real_ else s$velocity_threshold,
    dispersion_threshold = if (is.null(s$dispersion_threshold)) NA_real_ else s$dispersion_threshold,
    minimum_duration_ms = if (is.null(s$minimum_duration_ms)) NA_real_ else s$minimum_duration_ms,
    maximum_gap_ms = if (is.null(s$maximum_gap_ms)) NA_real_ else s$maximum_gap_ms,
    merge_rule = s$merge_rule, sampling_rate = if (is.null(s$sampling_rate)) NA_real_ else s$sampling_rate,
    smoothing = if (is.null(s$smoothing)) NA_character_ else s$smoothing,
    filter = if (is.null(s$filter)) NA_character_ else s$filter,
    coordinate_unit = s$coordinate_unit, implementation = s$implementation,
    implementation_version = s$implementation_version,
    detector_spec_hash = s$detector_spec_hash,
    stringsAsFactors = FALSE
  )))
  structure(list(specs = assembled, label = .edm_scalar_chr(label, "label"), manifest = manifest), class = "eye_detector_multiverse")
}

.edm_clean_branch <- function(x) {
  out <- x
  if (nrow(out$episodes)) out$episodes <- out$episodes[!out$episodes$episode_type %in% c("fixation", "saccade", "pursuit", "pso"), , drop = FALSE]
  out$episodes <- standardize_eye_table(out$episodes, "episodes")
  out
}
.edm_sampling_warnings <- function(x, spec, tolerance_fraction = .10) {
  if (!nrow(x$gaze_samples)) return(character())
  groups <- split(x$gaze_samples, interaction(x$gaze_samples$recording_id, x$gaze_samples$trial_id, drop = TRUE))
  rates <- vapply(groups, function(z) {
    t <- sort(as.numeric(z$timestamp_seconds)); dt <- diff(t); dt <- dt[is.finite(dt) & dt > 0]
    if (!length(dt)) NA_real_ else 1 / stats::median(dt)
  }, numeric(1))
  rates <- rates[is.finite(rates)]
  if (!length(rates)) return("Sampling rate could not be verified from timestamps.")
  empirical <- stats::median(rates)
  if (abs(empirical - spec$sampling_rate) / spec$sampling_rate > tolerance_fraction)
    sprintf("Empirical sampling rate (%.3f Hz) differs from the detector specification (%.3f Hz).", empirical, spec$sampling_rate)
  else character()
}
.edm_attach_detector <- function(events, spec, source) {
  events <- standardize_eye_table(events, "episodes")
  if (!nrow(events)) {
    for (nm in c("detector_id", "detector_algorithm", "detector_spec_hash", "detector_implementation", "detector_implementation_version",
                 "source_data_hash", "preprocessing_provenance_hash", "aoi_spec_hash", "quality_spec_hash", "software", "software_version")) events[[nm]] <- character()
    return(events)
  }
  lin <- .edm_lineage(source, spec)
  events$detector_id <- spec$detector_id
  events$detector_algorithm <- spec$algorithm
  for (nm in names(lin)) events[[nm]] <- lin[[nm]]
  events
}

.edm_detect_adaptive <- function(x, spec) {
  out <- .edm_clean_branch(x); rows <- list(); k <- 0L
  groups <- split(out$gaze_samples, interaction(out$gaze_samples$recording_id, out$gaze_samples$trial_id, drop = TRUE))
  for (z in groups) {
    z <- z[order(z$timestamp_seconds), , drop = FALSE]
    if (nrow(z) < 3L) next
    t <- as.numeric(z$timestamp_seconds); gx <- as.numeric(z$gaze_x); gy <- as.numeric(z$gaze_y)
    valid <- as.logical(z$valid); valid[is.na(valid)] <- FALSE
    dt <- c(NA_real_, diff(t)); vel <- c(NA_real_, sqrt(diff(gx)^2 + diff(gy)^2) / diff(t))
    usable <- vel[is.finite(vel) & valid]
    if (length(usable) < 3L) next
    center <- stats::median(usable); mad0 <- stats::median(abs(usable - center)); robust_sigma <- 1.4826 * mad0
    threshold <- max(spec$parameters$minimum_velocity_threshold, center + spec$parameters$noise_factor * robust_sigma)
    is_fix <- is.finite(vel) & valid & vel <= threshold
    if (length(is_fix)) is_fix[1L] <- if (length(is_fix) > 1L) is_fix[2L] else FALSE
    gap_limit <- if (is.null(spec$maximum_gap_ms)) 1000 / spec$sampling_rate * 2.5 else spec$maximum_gap_ms
    run <- integer(nrow(z)); cur <- 0L
    for (i in seq_len(nrow(z))) {
      if (i == 1L || !is_fix[i] || !is_fix[i - 1L] || (is.finite(dt[i]) && dt[i] * 1000 > gap_limit)) cur <- cur + 1L
      run[i] <- cur
    }
    for (r in unique(run[is_fix])) {
      pos <- which(run == r & is_fix); if (!length(pos)) next
      dur <- (max(t[pos]) - min(t[pos])) * 1000; if (dur < spec$minimum_duration_ms) next
      k <- k + 1L
      rows[[k]] <- data.frame(
        episode_id = sprintf("%s_adaptive_fix_%07d", z$recording_id[1L], k), recording_id = z$recording_id[1L],
        episode_type = "fixation", eye = "combined", start_time = min(t[pos]), end_time = max(t[pos]), duration_ms = dur,
        start_x = gx[min(pos)], start_y = gy[min(pos)], end_x = gx[max(pos)], end_y = gy[max(pos)],
        centroid_x = mean(gx[pos], na.rm = TRUE), centroid_y = mean(gy[pos], na.rm = TRUE), amplitude = NA_real_,
        peak_velocity = if (any(is.finite(vel[pos]))) max(vel[pos], na.rm = TRUE) else NA_real_,
        dispersion = diff(range(gx[pos], na.rm = TRUE)) + diff(range(gy[pos], na.rm = TRUE)),
        coordinate_space_id = z$coordinate_space_id[1L], source_algorithm = "adaptive velocity (robust-MAD reference)",
        source_parameters = sprintf("noise_factor=%g;minimum_velocity_threshold=%g;adaptive_threshold=%g", spec$parameters$noise_factor, spec$parameters$minimum_velocity_threshold, threshold),
        derived_by = "eyeprocess", trial_id = z$trial_id[1L], stimulus_id = z$stimulus_id[1L], aoi_id = NA_character_, stringsAsFactors = FALSE
      )
    }
  }
  detected <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("episodes")
  out$episodes <- standardize_eye_table(.bind_rows_base(out$episodes, detected), "episodes")
  add_provenance(out, "detect_fixations_adaptive_velocity", "episodes", paste0("detector_id=", spec$detector_id, ";spec_hash=", spec$detector_spec_hash, ";n=", nrow(detected)))
}

.edm_remodnav_version <- function(command) {
  tryCatch(paste(system2(command, "--version", stdout = TRUE, stderr = TRUE), collapse = " "), error = function(e) NA_character_)
}
.edm_run_remodnav <- function(x, spec) {
  command <- if (is.null(spec$parameters$command)) "remodnav" else as.character(spec$parameters$command)[1L]
  if (!nzchar(Sys.which(command))) .edm_stop("REMoDNaV executable was not found. Install REMoDNaV or supply parameters$command; no surrogate detector is substituted.")
  if (spec$coordinate_unit == "normalized") .edm_stop("REMoDNaV requires degree coordinates or pixel coordinates with explicit px2deg; convert normalized coordinates first.")
  px2deg <- if (spec$coordinate_unit == "degrees") 1 else spec$parameters$px2deg
  .edm_positive(px2deg, "px2deg", FALSE)
  out <- .edm_clean_branch(x); rows <- list(); k <- 0L
  groups <- split(out$gaze_samples, interaction(out$gaze_samples$recording_id, out$gaze_samples$trial_id, drop = TRUE))
  cli_map <- c(noise_factor = "--noise-factor", velthresh_startvelocity = "--velthresh-startvelocity", min_intersaccade_duration = "--min-intersaccade-duration",
               min_saccade_duration = "--min-saccade-duration", min_pursuit_duration = "--min-pursuit-duration", pursuit_velthresh = "--pursuit-velthresh",
               max_initial_saccade_freq = "--max-initial-saccade-freq", saccade_context_window_length = "--saccade-context-window-length", max_pso_duration = "--max-pso-duration",
               lowpass_cutoff_freq = "--lowpass-cutoff-freq", min_blink_duration = "--min-blink-duration", dilate_nan = "--dilate-nan",
               median_filter_length = "--median-filter-length", savgol_length = "--savgol-length", savgol_polyord = "--savgol-polyord", max_vel = "--max-vel")
  for (z in groups) {
    z <- z[order(z$timestamp_seconds), , drop = FALSE]; if (nrow(z) < 3L) next
    inp <- tempfile(fileext = ".tsv"); outp <- tempfile(fileext = ".tsv"); on.exit(unlink(c(inp, outp)), add = TRUE)
    gx <- as.numeric(z$gaze_x); gy <- as.numeric(z$gaze_y); valid <- as.logical(z$valid); valid[is.na(valid)] <- FALSE
    gx[!valid] <- NA_real_; gy[!valid] <- NA_real_
    utils::write.table(data.frame(x = gx, y = gy), inp, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, na = "nan")
    args <- c(shQuote(inp), shQuote(outp), format(px2deg, scientific = FALSE), format(spec$sampling_rate, scientific = FALSE),
              "--min-fixation-duration", format(spec$minimum_duration_ms / 1000, scientific = FALSE))
    for (nm in intersect(names(spec$parameters), names(cli_map))) args <- c(args, cli_map[[nm]], as.character(spec$parameters[[nm]]))
    status <- system2(command, args = args, stdout = TRUE, stderr = TRUE)
    if (!file.exists(outp)) .edm_stop("REMoDNaV failed for trial ", z$trial_id[1L], ": ", paste(status, collapse = " | "))
    ev <- utils::read.delim(outp, stringsAsFactors = FALSE, check.names = FALSE)
    if (!nrow(ev)) next
    label_map <- c(FIXA = "fixation", SACC = "saccade", ISAC = "saccade", PURS = "pursuit", HPSO = "pso", IHPS = "pso", LPSO = "pso", ILPS = "pso")
    ev <- ev[ev$label %in% names(label_map), , drop = FALSE]
    t0 <- min(as.numeric(z$timestamp_seconds), na.rm = TRUE)
    for (i in seq_len(nrow(ev))) {
      k <- k + 1L; st <- t0 + as.numeric(ev$onset[i]); en <- st + as.numeric(ev$duration[i])
      rows[[k]] <- data.frame(
        episode_id = sprintf("%s_remodnav_%07d", z$recording_id[1L], k), recording_id = z$recording_id[1L], episode_type = unname(label_map[ev$label[i]]), eye = "combined",
        start_time = st, end_time = en, duration_ms = (en - st) * 1000, start_x = ev$start_x[i], start_y = ev$start_y[i], end_x = ev$end_x[i], end_y = ev$end_y[i],
        centroid_x = mean(c(ev$start_x[i], ev$end_x[i]), na.rm = TRUE), centroid_y = mean(c(ev$start_y[i], ev$end_y[i]), na.rm = TRUE),
        amplitude = ev$amp[i], peak_velocity = ev$peak_vel[i], dispersion = NA_real_, coordinate_space_id = z$coordinate_space_id[1L],
        source_algorithm = "REMoDNaV", source_parameters = paste(names(spec$parameters), unlist(spec$parameters), sep = "=", collapse = ";"), derived_by = "external",
        trial_id = z$trial_id[1L], stimulus_id = z$stimulus_id[1L], aoi_id = NA_character_, stringsAsFactors = FALSE
      )
    }
  }
  detected <- if (length(rows)) do.call(.bind_rows_base, rows) else empty_eye_table("episodes")
  out$episodes <- standardize_eye_table(.bind_rows_base(out$episodes, detected), "episodes")
  add_provenance(out, "detect_events_remodnav", "episodes", paste0("detector_id=", spec$detector_id, ";spec_hash=", spec$detector_spec_hash, ";remodnav_version=", .edm_remodnav_version(command), ";n=", nrow(detected)))
}

#' Import external detector events into the canonical event schema
#' @export
import_external_detector_events <- function(events, spec, dataset = NULL) {
  validate_event_detector_spec(spec)
  if (!is.data.frame(events)) .edm_stop("External detector output must be a data frame.")
  d <- events
  if ("onset" %in% names(d) && !"start_time" %in% names(d)) names(d)[names(d) == "onset"] <- "start_time"
  if ("label" %in% names(d) && !"episode_type" %in% names(d)) names(d)[names(d) == "label"] <- "episode_type"
  if (!"end_time" %in% names(d) && all(c("start_time", "duration") %in% names(d))) d$end_time <- as.numeric(d$start_time) + as.numeric(d$duration)
  req <- c("recording_id", "episode_type", "start_time", "end_time"); miss <- setdiff(req, names(d)); if (length(miss)) .edm_stop("External detector events are missing: ", paste(miss, collapse = ", "))
  labels <- c(FIXA = "fixation", FIX = "fixation", SACC = "saccade", ISAC = "saccade", PURS = "pursuit", PUR = "pursuit", HPSO = "pso", IHPS = "pso", LPSO = "pso", ILPS = "pso")
  raw <- as.character(d$episode_type); d$episode_type <- ifelse(raw %in% names(labels), unname(labels[raw]), tolower(raw))
  st <- as.numeric(d$start_time); en <- as.numeric(d$end_time); if (any(!is.finite(st) | !is.finite(en) | en < st)) .edm_stop("External detector event times must be finite with end_time >= start_time.")
  if (!"duration_ms" %in% names(d)) d$duration_ms <- (en - st) * 1000
  if (!"episode_id" %in% names(d)) d$episode_id <- sprintf("%s_external_%07d", spec$detector_id, seq_len(nrow(d)))
  if (!"eye" %in% names(d)) d$eye <- "combined"
  if (!"source_algorithm" %in% names(d)) d$source_algorithm <- spec$implementation
  if (!"source_parameters" %in% names(d)) d$source_parameters <- paste(names(spec$parameters), unlist(spec$parameters), sep = "=", collapse = ";")
  if (!"derived_by" %in% names(d)) d$derived_by <- "external"
  if (!"trial_id" %in% names(d)) d$trial_id <- NA_character_
  d <- standardize_eye_table(d, "episodes")
  if (!is.null(dataset)) {
    lin <- .edm_lineage(dataset, spec); for (nm in names(lin)) d[[nm]] <- lin[[nm]]
  }
  d$detector_id <- spec$detector_id; d$detector_algorithm <- spec$algorithm; d$detector_spec_hash <- spec$detector_spec_hash
  d$detector_implementation <- spec$implementation; d$detector_implementation_version <- spec$implementation_version
  d
}

#' Detect events with one explicit specification
#' @export
detect_events_with_spec <- function(x, spec) {
  .assert_eye_dataset(x); validate_event_detector_spec(spec)
  branch <- .edm_clean_branch(x); sampling_warnings <- if (spec$algorithm %in% c("ivt", "idt", "adaptive_velocity", "remodnav")) .edm_sampling_warnings(branch, spec) else character()
  if (length(sampling_warnings)) .edm_warn(paste(sampling_warnings, collapse = " | "))
  if (spec$algorithm == "ivt") {
    branch <- detect_fixations_ivt(branch, velocity_threshold = spec$velocity_threshold, minimum_duration_ms = spec$minimum_duration_ms,
                                   maximum_gap_ms = if (is.null(spec$maximum_gap_ms)) 75 else spec$maximum_gap_ms, coordinate_units = spec$coordinate_unit, overwrite = FALSE)
    if (isTRUE(spec$parameters$include_saccades)) branch <- detect_saccades(branch, velocity_threshold = spec$velocity_threshold,
      minimum_duration_ms = if (is.null(spec$parameters$minimum_saccade_duration_ms)) 10 else spec$parameters$minimum_saccade_duration_ms, overwrite = FALSE)
  } else if (spec$algorithm == "idt") {
    branch <- detect_fixations_idt(branch, dispersion_threshold = spec$dispersion_threshold, minimum_duration_ms = spec$minimum_duration_ms,
                                   coordinate_units = spec$coordinate_unit, overwrite = FALSE)
  } else if (spec$algorithm == "adaptive_velocity") branch <- .edm_detect_adaptive(branch, spec)
  else if (spec$algorithm == "remodnav") branch <- .edm_run_remodnav(branch, spec)
  else if (spec$algorithm == "external") {
    ext <- spec$callback(data = branch, spec = spec); if (inherits(ext, "eye_dataset")) ext <- ext$episodes
    ext <- import_external_detector_events(ext, spec, dataset = branch)
    branch$episodes <- standardize_eye_table(.bind_rows_base(branch$episodes, ext), "episodes")
    branch <- add_provenance(branch, "detect_events_external", "episodes", paste0("detector_id=", spec$detector_id, ";spec_hash=", spec$detector_spec_hash, ";n=", nrow(ext)))
  } else if (spec$algorithm == "vendor") {
    vend <- x$episodes; by <- if (is.null(spec$parameters$derived_by)) "vendor" else spec$parameters$derived_by
    vend <- vend[vend$derived_by == by, , drop = FALSE]; vend <- .edm_attach_detector(vend, spec, x)
    branch$episodes <- standardize_eye_table(.bind_rows_base(branch$episodes, vend), "episodes")
  }
  rel <- branch$episodes
  if (spec$algorithm != "vendor") rel <- rel[rel$episode_type %in% c("fixation", "saccade", "pursuit", "pso"), , drop = FALSE]
  rel <- .edm_attach_detector(rel, spec, x)
  if (nrow(rel)) {
    idx <- match(branch$episodes$episode_id, rel$episode_id)
    extras <- setdiff(names(rel), names(branch$episodes)); for (nm in extras) branch$episodes[[nm]] <- NA
    prov_cols <- unique(c(grep("^detector_", names(rel), value = TRUE), c("source_data_hash", "preprocessing_provenance_hash", "aoi_spec_hash", "quality_spec_hash", "software", "software_version")))
    for (nm in intersect(prov_cols, names(rel))) branch$episodes[[nm]] <- rel[[nm]][idx]
  }
  add_provenance(branch, "detect_events_with_spec", "episodes", paste0("detector_id=", spec$detector_id, ";algorithm=", spec$algorithm, ";spec_hash=", spec$detector_spec_hash),
                 warnings = if (length(sampling_warnings)) paste(sampling_warnings, collapse = " | ") else NA_character_)
}

#' Run all detector branches independently
#' @export
run_detector_multiverse <- function(x, multiverse, continue_on_error = TRUE) {
  .assert_eye_dataset(x)
  if (!inherits(multiverse, "eye_detector_multiverse")) multiverse <- create_detector_multiverse(multiverse)
  branches <- list(); events <- list(); statuses <- list(); failures <- list(); warns <- list()
  for (spec in multiverse$specs) {
    captured <- character()
    value <- withCallingHandlers(tryCatch(detect_events_with_spec(x, spec), error = identity), warning = function(w) { captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning") })
    if (inherits(value, "error")) {
      failures[[length(failures) + 1L]] <- data.frame(detector_id = spec$detector_id, detector_spec_hash = spec$detector_spec_hash, stage = "detection", error_type = class(value)[1L], error = conditionMessage(value), stringsAsFactors = FALSE)
      statuses[[length(statuses) + 1L]] <- data.frame(detector_id = spec$detector_id, detector_spec_hash = spec$detector_spec_hash, status = "failed", n_events = NA_real_, n_fixations = NA_real_, stringsAsFactors = FALSE)
      if (!continue_on_error) stop(value)
      next
    }
    branches[[spec$detector_id]] <- value
    ev <- value$episodes; if ("detector_id" %in% names(ev)) ev <- ev[ev$detector_id == spec$detector_id, , drop = FALSE] else ev <- ev[FALSE, , drop = FALSE]
    events[[length(events) + 1L]] <- ev
    statuses[[length(statuses) + 1L]] <- data.frame(detector_id = spec$detector_id, detector_spec_hash = spec$detector_spec_hash, status = "ok", n_events = nrow(ev), n_fixations = sum(ev$episode_type == "fixation"), stringsAsFactors = FALSE)
    if (length(captured)) for (w in captured) warns[[length(warns) + 1L]] <- data.frame(detector_id = spec$detector_id, stage = "detection", warning = w, stringsAsFactors = FALSE)
  }
  structure(list(multiverse = multiverse, branches = branches, events = .edm_rbind_fill(events), status = .edm_rbind_fill(statuses), failures = .edm_rbind_fill(failures), warnings = .edm_rbind_fill(warns), features = data.frame(), source_fingerprint = .edm_dataset_hash(x)), class = "eye_detector_multiverse_result")
}

.edm_iou <- function(a1, a2, b1, b2) { inter <- max(0, min(a2, b2) - max(a1, b1)); union <- max(a2, b2) - min(a1, b1); if (union > 0) inter / union else as.numeric(a1 == b1 && a2 == b2) }

#' Match detected events using one-to-one temporal overlap
#' @export
match_detected_events <- function(reference, candidate, event_type = "fixation", onset_tolerance_ms = 75, minimum_overlap = .10) {
  if (!is.data.frame(reference) || !is.data.frame(candidate)) .edm_stop("reference and candidate must be data frames.")
  req <- c("recording_id", "episode_type", "start_time", "end_time"); if (length(setdiff(req, names(reference))) || length(setdiff(req, names(candidate)))) .edm_stop("Event tables are missing required fields.")
  if (!is.finite(onset_tolerance_ms) || onset_tolerance_ms < 0 || !is.finite(minimum_overlap) || minimum_overlap < 0 || minimum_overlap > 1) .edm_stop("Invalid event-matching tolerance.")
  ref <- reference[reference$episode_type == event_type, , drop = FALSE]; cand <- candidate[candidate$episode_type == event_type, , drop = FALSE]
  pairs <- list(); k <- 0L
  for (i in seq_len(nrow(ref))) for (j in seq_len(nrow(cand))) {
    if (as.character(ref$recording_id[i]) != as.character(cand$recording_id[j])) next
    if ("trial_id" %in% names(ref) && "trial_id" %in% names(cand) && !is.na(ref$trial_id[i]) && !is.na(cand$trial_id[j]) && as.character(ref$trial_id[i]) != as.character(cand$trial_id[j])) next
    iou <- .edm_iou(ref$start_time[i], ref$end_time[i], cand$start_time[j], cand$end_time[j]); onset <- abs(ref$start_time[i] - cand$start_time[j]) * 1000
    if (iou < minimum_overlap && onset > onset_tolerance_ms) next
    k <- k + 1L; pairs[[k]] <- data.frame(iou = iou, onset = onset, i = i, j = j)
  }
  if (!length(pairs)) return(data.frame(reference_index = integer(), candidate_index = integer(), recording_id = character(), trial_id = character(), event_type = character(), overlap_iou = numeric(), onset_difference_ms = numeric(), offset_difference_ms = numeric(), duration_difference_ms = numeric()))
  p <- do.call(rbind, pairs); p <- p[order(-p$iou, p$onset), , drop = FALSE]; used_i <- integer(); used_j <- integer(); out <- list()
  for (r in seq_len(nrow(p))) {
    i <- p$i[r]; j <- p$j[r]; if (i %in% used_i || j %in% used_j) next; used_i <- c(used_i, i); used_j <- c(used_j, j)
    out[[length(out) + 1L]] <- data.frame(reference_index = i, candidate_index = j, recording_id = ref$recording_id[i], trial_id = if ("trial_id" %in% names(ref)) ref$trial_id[i] else NA_character_, event_type = event_type,
      overlap_iou = p$iou[r], onset_difference_ms = p$onset[r], offset_difference_ms = abs(ref$end_time[i] - cand$end_time[j]) * 1000,
      duration_difference_ms = ((cand$end_time[j] - cand$start_time[j]) - (ref$end_time[i] - ref$start_time[i])) * 1000, stringsAsFactors = FALSE)
  }
  .edm_rbind_fill(out)
}

#' Compare two event catalogues
#' @export
compare_event_catalogues <- function(reference, candidate, event_type = "fixation", onset_tolerance_ms = 75, minimum_overlap = .10) {
  m <- match_detected_events(reference, candidate, event_type, onset_tolerance_ms, minimum_overlap)
  nr <- sum(reference$episode_type == event_type); nc <- sum(candidate$episode_type == event_type); nm <- nrow(m)
  precision <- if (nc) nm / nc else NA_real_; recall <- if (nr) nm / nr else NA_real_; f1 <- if (is.finite(precision) && is.finite(recall) && precision + recall > 0) 2 * precision * recall / (precision + recall) else NA_real_
  data.frame(event_type = event_type, reference_events = nr, candidate_events = nc, matched_events = nm, matched_event_precision = precision, matched_event_recall = recall, f1 = f1,
    mean_event_overlap = if (nm) mean(m$overlap_iou) else NA_real_, median_event_overlap = if (nm) stats::median(m$overlap_iou) else NA_real_,
    mean_onset_difference_ms = if (nm) mean(m$onset_difference_ms) else NA_real_, mean_offset_difference_ms = if (nm) mean(m$offset_difference_ms) else NA_real_,
    mean_duration_difference_ms = if (nm) mean(m$duration_difference_ms) else NA_real_, stringsAsFactors = FALSE)
}

#' Estimate pairwise detector agreement
#' @export
estimate_detector_agreement <- function(x, event_type = "fixation", onset_tolerance_ms = 75, minimum_overlap = .10) {
  events <- if (inherits(x, "eye_detector_multiverse_result")) x$events else x
  if (!is.data.frame(events) || !"detector_id" %in% names(events)) .edm_stop("Detector-labelled event data are required.")
  ids <- sort(unique(as.character(events$detector_id))); if (length(ids) < 2L) return(data.frame())
  cmb <- utils::combn(ids, 2L); rows <- list()
  for (i in seq_len(ncol(cmb))) {
    a <- cmb[1L, i]; b <- cmb[2L, i]
    q <- compare_event_catalogues(events[events$detector_id == a, , drop = FALSE], events[events$detector_id == b, , drop = FALSE], event_type, onset_tolerance_ms, minimum_overlap)
    q$detector_a <- a; q$detector_b <- b; rows[[i]] <- q
  }
  .edm_rbind_fill(rows)
}

#' Summarise detector events
#' @export
summarise_detector_events <- function(x) {
  events <- if (inherits(x, "eye_detector_multiverse_result")) x$events else x
  if (!nrow(events)) return(data.frame()); if (!"detector_id" %in% names(events)) .edm_stop("events must include detector_id.")
  groups <- split(events, events$detector_id); .edm_rbind_fill(lapply(names(groups), function(id) {
    z <- groups[[id]]; f <- z[z$episode_type == "fixation", , drop = FALSE]; dur <- as.numeric(f$duration_ms)
    data.frame(detector_id = id, number_of_events = nrow(z), number_of_fixations = nrow(f), number_of_saccades = sum(z$episode_type == "saccade"),
      mean_fixation_duration_ms = if (nrow(f)) mean(dur, na.rm = TRUE) else NA_real_, median_fixation_duration_ms = if (nrow(f)) stats::median(dur, na.rm = TRUE) else NA_real_,
      total_fixation_duration_ms = if (nrow(f)) sum(dur, na.rm = TRUE) else 0, stringsAsFactors = FALSE)
  }))
}

#' Summarise detector disagreement
#' @export
summarise_detector_disagreement <- function(x, event_type = "fixation") {
  a <- estimate_detector_agreement(x, event_type); if (!nrow(a)) return(a)
  a$unmatched_reference <- a$reference_events - a$matched_events; a$unmatched_candidate <- a$candidate_events - a$matched_events; a$event_count_difference <- a$candidate_events - a$reference_events; a
}

.edm_assign_episode_aois <- function(branch, overlap = "error") {
  if (!overlap %in% c("error", "first", "smallest", "all")) .edm_stop("overlap must be error, first, smallest, or all.")
  if (!nrow(branch$aoi_definitions) || !nrow(branch$aoi_geometry)) .edm_stop("No AOIs are registered.")
  out <- branch; d <- out$episodes; if (!nrow(d)) return(out); assigned <- rep(NA_character_, nrow(d))
  for (r in seq_len(nrow(d))) {
    if (!d$episode_type[r] %in% c("fixation", "pursuit") || !is.finite(as.numeric(d$centroid_x[r]))) { assigned[r] <- d$aoi_id[r]; next }
    hits <- list()
    for (i in seq_len(nrow(out$aoi_definitions))) {
      def <- out$aoi_definitions[i, , drop = FALSE]
      if (!is.na(def$stimulus_id) && nzchar(def$stimulus_id) && as.character(d$stimulus_id[r]) != as.character(def$stimulus_id)) next
      geoms <- out$aoi_geometry[out$aoi_geometry$aoi_id == def$aoi_id, , drop = FALSE]
      for (g in seq_len(nrow(geoms))) {
        geom <- geoms[g, , drop = FALSE]; if (as.character(d$coordinate_space_id[r]) != as.character(geom$coordinate_space_id)) next
        hit <- .aoi_contains(as.numeric(d$centroid_x[r]), as.numeric(d$centroid_y[r]), as.numeric(d$start_time[r]), def, geom)
        if (isTRUE(hit)) hits[[length(hits) + 1L]] <- list(id = as.character(def$aoi_id), area = as.numeric(geom$width) * as.numeric(geom$height), order = i)
      }
    }
    if (length(hits)) hits <- hits[!duplicated(vapply(hits, `[[`, character(1), "id"))]
    if (length(hits) > 1L && overlap == "error") .edm_stop("Ambiguous AOI assignment for episode ", d$episode_id[r], ": ", paste(vapply(hits, `[[`, character(1), "id"), collapse = ", "), ". Choose an overlap rule explicitly.")
    if (!length(hits)) assigned[r] <- NA_character_
    else if (overlap == "all") assigned[r] <- paste(vapply(hits, `[[`, character(1), "id"), collapse = "|")
    else if (overlap == "smallest") assigned[r] <- hits[[order(vapply(hits, `[[`, numeric(1), "area"), vapply(hits, `[[`, numeric(1), "order"))[1L]]]$id
    else assigned[r] <- hits[[1L]]$id
  }
  d$aoi_id <- assigned; out$episodes <- d; add_provenance(out, "propagate_detector_to_aoi", "episodes", paste0("overlap=", overlap))
}

#' Propagate detector branches to AOI assignment
#' @export
propagate_detector_to_aoi <- function(x, overlap = "error", continue_on_error = TRUE) {
  if (!inherits(x, "eye_detector_multiverse_result")) .edm_stop("x must be an eye_detector_multiverse_result.")
  branches <- list(); events <- list(); failures <- if (nrow(x$failures)) split(x$failures, seq_len(nrow(x$failures))) else list()
  for (spec in x$multiverse$specs) {
    if (is.null(x$branches[[spec$detector_id]])) next
    value <- tryCatch(.edm_assign_episode_aois(x$branches[[spec$detector_id]], overlap), error = identity)
    if (inherits(value, "error")) {
      failures[[length(failures) + 1L]] <- data.frame(detector_id = spec$detector_id, detector_spec_hash = spec$detector_spec_hash, stage = "aoi_assignment", error_type = class(value)[1L], error = conditionMessage(value), stringsAsFactors = FALSE)
      if (!continue_on_error) stop(value); next
    }
    branches[[spec$detector_id]] <- value; ev <- value$episodes; if ("detector_id" %in% names(ev)) ev <- ev[ev$detector_id == spec$detector_id, , drop = FALSE]; events[[length(events) + 1L]] <- ev
  }
  x$branches <- branches; x$events <- .edm_rbind_fill(events); x$failures <- .edm_rbind_fill(failures); x
}

.edm_trial_features <- function(branch, spec) {
  trials <- branch$intervals[branch$intervals$interval_type == "trial", , drop = FALSE]
  if (!nrow(trials)) .edm_stop("Explicit trial intervals are required for detector-to-feature propagation.")
  if (any(is.na(trials$trial_id)) || anyDuplicated(paste(trials$recording_id, trials$trial_id, sep = "\r"))) .edm_stop("Trial intervals must have non-missing unique recording_id/trial_id pairs.")
  if (!nrow(branch$aoi_definitions)) .edm_stop("AOI definitions are required before feature propagation.")
  fix <- branch$episodes[branch$episodes$episode_type == "fixation", , drop = FALSE]; if ("detector_id" %in% names(fix)) fix <- fix[fix$detector_id == spec$detector_id, , drop = FALSE]
  lin <- .edm_lineage(branch, spec); rows <- list(); k <- 0L
  for (i in seq_len(nrow(trials))) {
    tr <- trials[i, , drop = FALSE]; g <- branch$gaze_samples[branch$gaze_samples$recording_id == tr$recording_id & branch$gaze_samples$trial_id == tr$trial_id, , drop = FALSE]
    n_samples <- nrow(g); valid <- if (n_samples) as.logical(g$valid) & is.finite(as.numeric(g$gaze_x)) & is.finite(as.numeric(g$gaze_y)) else logical(); valid[is.na(valid)] <- FALSE
    vf <- if (n_samples) mean(valid) else NA_real_; observed <- n_samples > 0L && is.finite(vf) && vf > 0
    tf <- fix[fix$recording_id == tr$recording_id & fix$trial_id == tr$trial_id, , drop = FALSE]; tf <- tf[order(tf$start_time), , drop = FALSE]
    seq <- as.character(tf$aoi_id[!is.na(tf$aoi_id)]); collapsed <- if (length(seq)) seq[c(TRUE, seq[-1L] != seq[-length(seq)])] else character()
    for (a in seq_len(nrow(branch$aoi_definitions))) {
      id <- as.character(branch$aoi_definitions$aoi_id[a]); target <- tf[as.character(tf$aoi_id) == id & !is.na(tf$aoi_id), , drop = FALSE]
      count <- if (observed) nrow(target) else NA_real_; dwell <- if (!observed) NA_real_ else if (nrow(target)) sum(as.numeric(target$duration_ms), na.rm = TRUE) else 0
      mean_dur <- if (nrow(target)) mean(as.numeric(target$duration_ms), na.rm = TRUE) else NA_real_; latency <- if (nrow(target)) (min(target$start_time) - tr$start_time) * 1000 else NA_real_
      entries <- if (length(collapsed)) sum(collapsed == id & c(TRUE, collapsed[-length(collapsed)] != id)) else 0L
      from <- if (length(collapsed) > 1L) sum(head(collapsed, -1L) == id & tail(collapsed, -1L) != id) else 0L
      to <- if (length(collapsed) > 1L) sum(tail(collapsed, -1L) == id & head(collapsed, -1L) != id) else 0L
      k <- k + 1L; row <- data.frame(detector_id = spec$detector_id, detector_algorithm = spec$algorithm, detector_spec_hash = spec$detector_spec_hash,
        recording_id = tr$recording_id, participant_id = tr$participant_id, trial_id = tr$trial_id, item_id = tr$item_id, stimulus_id = tr$stimulus_id, condition_id = tr$condition_id, aoi_id = id,
        trial_duration_ms = (tr$end_time - tr$start_time) * 1000, n_gaze_samples = n_samples, valid_data_fraction = vf,
        fixation_count = count, dwell_time_ms = dwell, mean_fixation_duration_ms = mean_dur, first_fixation_latency_ms = latency, ttff_ms = latency,
        ttff_event_observed = if (observed) nrow(target) > 0L else NA, ttff_censor_time_ms = if (observed) (tr$end_time - tr$start_time) * 1000 else NA_real_,
        revisits = if (observed) max(entries - 1L, 0L) else NA_real_, transition_count_from_aoi = if (observed) from else NA_real_, transition_count_to_aoi = if (observed) to else NA_real_,
        scanpath_sequence = paste(collapsed, collapse = " > "), pupil_within_fixation_mean = NA_real_, feature_review_required = is.finite(vf) && vf < .5, stringsAsFactors = FALSE)
      for (nm in names(lin)) row[[nm]] <- lin[[nm]]; rows[[k]] <- row
    }
  }
  .edm_rbind_fill(rows)
}

#' Propagate detector branches to derived AOI features
#' @export
propagate_detector_to_features <- function(x, continue_on_error = TRUE) {
  if (!inherits(x, "eye_detector_multiverse_result")) .edm_stop("x must be an eye_detector_multiverse_result.")
  frames <- list(); failures <- if (nrow(x$failures)) split(x$failures, seq_len(nrow(x$failures))) else list()
  for (spec in x$multiverse$specs) {
    branch <- x$branches[[spec$detector_id]]; if (is.null(branch)) next
    value <- tryCatch(.edm_trial_features(branch, spec), error = identity)
    if (inherits(value, "error")) { failures[[length(failures) + 1L]] <- data.frame(detector_id = spec$detector_id, detector_spec_hash = spec$detector_spec_hash, stage = "feature_propagation", error_type = class(value)[1L], error = conditionMessage(value), stringsAsFactors = FALSE); if (!continue_on_error) stop(value); next }
    frames[[length(frames) + 1L]] <- value
  }
  x$features <- .edm_rbind_fill(frames); x$failures <- .edm_rbind_fill(failures); x
}

.edm_tidy_lm <- function(fit, converged = TRUE) {
  sm <- summary(fit)$coefficients; ci <- stats::confint(fit)
  data.frame(term = rownames(sm), estimate = sm[, 1L], SE = sm[, 2L], CI_lower = ci[rownames(sm), 1L], CI_upper = ci[rownames(sm), 2L], p = sm[, ncol(sm)], converged = converged, N = stats::nobs(fit), stringsAsFactors = FALSE)
}
.edm_tidy_lmer <- function(fit, converged) {
  sm <- summary(fit)$coefficients; terms <- rownames(sm); ci <- suppressMessages(stats::confint(fit, parm = terms, method = "Wald"))
  data.frame(term = terms, estimate = sm[, "Estimate"], SE = sm[, "Std. Error"], CI_lower = ci[terms, 1L], CI_upper = ci[terms, 2L], p = NA_real_, converged = converged, N = stats::nobs(fit), stringsAsFactors = FALSE)
}

#' Run identical statistical inference across detector branches
#' @export
run_detector_inference_multiverse <- function(x, model_spec, model_callback = NULL, minimum_valid_fraction = NULL) {
  if (!inherits(x, "eye_detector_multiverse_result") || !nrow(x$features)) .edm_stop("Propagated detector features are required.")
  if (!is.list(model_spec)) .edm_stop("model_spec must be a named list.")
  engine <- as.character(model_spec$engine)[1L]; if (!engine %in% c("stats_lm", "lme4_lmer", "callback")) .edm_stop("Choose an explicit model engine: stats_lm, lme4_lmer, or callback.")
  if (engine == "callback" && !is.function(model_callback)) .edm_stop("engine='callback' requires model_callback.")
  formula <- model_spec$formula; if (is.null(formula)) .edm_stop("model_spec$formula is required."); if (is.character(formula)) formula <- stats::as.formula(formula)
  outcome <- if (!is.null(model_spec$outcome)) as.character(model_spec$outcome)[1L] else all.vars(formula)[1L]
  if (!outcome %in% names(x$features)) .edm_stop("The declared outcome is not present in propagated features.")
  rows <- list(); failures <- list(); warns <- list()
  for (spec in x$multiverse$specs) {
    d <- x$features[x$features$detector_id == spec$detector_id, , drop = FALSE]
    if (!is.null(model_spec$aoi_id)) d <- d[as.character(d$aoi_id) == as.character(model_spec$aoi_id), , drop = FALSE]
    if (!is.null(minimum_valid_fraction)) { if (!is.finite(minimum_valid_fraction) || minimum_valid_fraction < 0 || minimum_valid_fraction > 1) .edm_stop("minimum_valid_fraction must lie in [0,1]."); d <- d[is.finite(d$valid_data_fraction) & d$valid_data_fraction >= minimum_valid_fraction, , drop = FALSE] }
    d <- d[is.finite(as.numeric(d[[outcome]])), , drop = FALSE]; if (!nrow(d)) { failures[[length(failures) + 1L]] <- data.frame(detector_id = spec$detector_id, stage = "model", error_type = "NoModelData", error = "No finite model rows remained for this detector."); next }
    captured <- character()
    fitres <- withCallingHandlers(tryCatch({
      if (engine == "stats_lm") { fit <- stats::lm(formula, data = d, na.action = stats::na.fail); list(tidy = .edm_tidy_lm(fit), converged = TRUE) }
      else if (engine == "lme4_lmer") {
        if (!requireNamespace("lme4", quietly = TRUE)) .edm_stop("lme4_lmer requires the optional lme4 package; no surrogate estimator is substituted.")
        fit <- lme4::lmer(formula, data = d, REML = isTRUE(model_spec$reml), na.action = stats::na.fail, control = lme4::lmerControl(optimizer = if (is.null(model_spec$optimizer)) "nloptwrap" else model_spec$optimizer))
        msg <- fit@optinfo$conv$lme4$messages; converged <- is.null(msg) && is.null(fit@optinfo$conv$opt) || identical(fit@optinfo$conv$opt, 0L)
        list(tidy = .edm_tidy_lmer(fit, converged), converged = converged)
      } else { tab <- model_callback(d, model_spec); req <- c("term", "estimate", "SE", "CI_lower", "CI_upper", "p", "converged", "N"); if (!is.data.frame(tab) || length(setdiff(req, names(tab)))) .edm_stop("model_callback must return the documented tidy coefficient contract."); list(tidy = tab, converged = all(tab$converged %in% TRUE)) }
    }, error = identity), warning = function(w) { captured <<- c(captured, conditionMessage(w)); invokeRestart("muffleWarning") })
    if (inherits(fitres, "error")) { failures[[length(failures) + 1L]] <- data.frame(detector_id = spec$detector_id, stage = "model", error_type = class(fitres)[1L], error = conditionMessage(fitres), stringsAsFactors = FALSE); next }
    tab <- fitres$tidy; if (!isTRUE(fitres$converged)) captured <- c(captured, "Model did not converge; estimates are retained for diagnosis but excluded from stability summaries.")
    tab$detector_id <- spec$detector_id; tab$detector_algorithm <- spec$algorithm; tab$detector_spec_hash <- spec$detector_spec_hash; tab$parameter_spec <- spec$detector_spec_hash
    tab$model_engine <- engine; tab$model_formula <- paste(deparse(formula), collapse = " "); tab$model_spec_hash <- .edm_hash(model_spec); tab$feature_fingerprint <- .edm_hash(d); tab$software <- "eyeprocess"; tab$software_version <- tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_); tab$warnings <- if (length(captured)) paste(captured, collapse = " | ") else NA_character_
    rows[[length(rows) + 1L]] <- tab; if (length(captured)) for (w in captured) warns[[length(warns) + 1L]] <- data.frame(detector_id = spec$detector_id, stage = "model", warning = w, stringsAsFactors = FALSE)
  }
  structure(list(multiverse = x$multiverse, coefficients = .edm_rbind_fill(rows), failures = .edm_rbind_fill(failures), warnings = .edm_rbind_fill(warns), model_spec = model_spec, feature_fingerprint = .edm_hash(x$features)), class = "eye_detector_inference_result")
}

#' Assess detector-level inference stability
#' @export
assess_detector_inference_stability <- function(x, term, substantive_threshold = NULL, direction = c("above", "below", "absolute")) {
  if (!inherits(x, "eye_detector_inference_result")) .edm_stop("x must be an eye_detector_inference_result."); direction <- match.arg(direction)
  d <- x$coefficients[as.character(x$coefficients$term) == as.character(term), , drop = FALSE]; all_n <- nrow(d)
  if (!all_n) return(data.frame(term = term, specifications = 0L, converged_specifications = 0L, convergence_rate = NA_real_, median_estimate = NA_real_, estimate_min = NA_real_, estimate_max = NA_real_, estimate_range = NA_real_, same_sign_proportion = NA_real_, ci_overlap = NA, ci_overlap_lower = NA_real_, ci_overlap_upper = NA_real_, substantive_conclusion_stability = NA_real_))
  d <- d[d$converged %in% TRUE & is.finite(as.numeric(d$estimate)), , drop = FALSE]
  if (!nrow(d)) return(data.frame(term = term, specifications = all_n, converged_specifications = 0L, convergence_rate = 0, median_estimate = NA_real_, estimate_min = NA_real_, estimate_max = NA_real_, estimate_range = NA_real_, same_sign_proportion = NA_real_, ci_overlap = NA, ci_overlap_lower = NA_real_, ci_overlap_upper = NA_real_, substantive_conclusion_stability = NA_real_))
  e <- as.numeric(d$estimate); nz <- e[e != 0]; same <- if (!length(nz)) NA_real_ else max(mean(nz > 0), mean(nz < 0)); ok <- is.finite(d$CI_lower) & is.finite(d$CI_upper)
  lo <- if (any(ok)) max(d$CI_lower[ok]) else NA_real_; hi <- if (any(ok)) min(d$CI_upper[ok]) else NA_real_; cio <- if (any(ok)) lo <= hi else NA
  subst <- NA_real_; if (!is.null(substantive_threshold)) { dec <- switch(direction, above = e >= substantive_threshold, below = e <= substantive_threshold, absolute = abs(e) >= abs(substantive_threshold)); subst <- max(mean(dec), mean(!dec)) }
  data.frame(term = term, specifications = all_n, converged_specifications = nrow(d), convergence_rate = nrow(d) / all_n, median_estimate = stats::median(e), estimate_min = min(e), estimate_max = max(e), estimate_range = diff(range(e)), same_sign_proportion = same, ci_overlap = cio, ci_overlap_lower = lo, ci_overlap_upper = hi, substantive_conclusion_stability = subst, stringsAsFactors = FALSE)
}

.edm_feature_sensitivity <- function(features) {
  metrics <- intersect(c("dwell_time_ms", "fixation_count", "mean_fixation_duration_ms", "ttff_ms", "revisits", "transition_count_from_aoi", "transition_count_to_aoi", "pupil_within_fixation_mean"), names(features)); rows <- list()
  for (m in metrics) {
    groups <- split(features, interaction(features$recording_id, features$trial_id, features$aoi_id, drop = TRUE)); ranges <- vapply(groups, function(z) { v <- as.numeric(z[[m]]); v <- v[is.finite(v)]; if (length(v) >= 2L) diff(range(v)) else NA_real_ }, numeric(1)); ranges <- ranges[is.finite(ranges)]
    rows[[length(rows) + 1L]] <- data.frame(feature = m, units_with_multiple_detectors = length(ranges), median_detector_range = if (length(ranges)) stats::median(ranges) else NA_real_, max_detector_range = if (length(ranges)) max(ranges) else NA_real_, stringsAsFactors = FALSE)
  }
  .edm_rbind_fill(rows)
}

#' Summarise detector robustness
#' @export
summarise_detector_robustness <- function(x, inference = NULL, term = NULL, substantive_threshold = NULL, direction = "above") {
  out <- list(event_summary = summarise_detector_events(x), event_agreement = if (nrow(x$events)) estimate_detector_agreement(x) else data.frame(), feature_sensitivity = .edm_feature_sensitivity(x$features), failures = x$failures)
  if (!is.null(inference)) { if (is.null(term)) .edm_stop("term is required when inference is supplied."); out$inference_stability <- assess_detector_inference_stability(inference, term, substantive_threshold, direction); out$model_failures <- inference$failures }
  else { out$inference_stability <- data.frame(); out$model_failures <- data.frame() }
  out
}

#' Plot event timelines by detector
#' @export
plot_detector_event_timeline <- function(x, trial_id = NULL, event_type = "fixation", ...) {
  ev <- if (inherits(x, "eye_detector_multiverse_result")) x$events else x; ev <- ev[ev$episode_type == event_type, , drop = FALSE]; if (!is.null(trial_id)) ev <- ev[as.character(ev$trial_id) == as.character(trial_id), , drop = FALSE]
  ids <- sort(unique(as.character(ev$detector_id))); graphics::plot(NA, xlim = if (nrow(ev)) range(c(ev$start_time, ev$end_time), finite = TRUE) else c(0, 1), ylim = c(.5, max(1, length(ids)) + .5), yaxt = "n", xlab = "Time (s)", ylab = "Detector", main = paste(tools::toTitleCase(event_type), "event timeline"), ...); graphics::axis(2, at = seq_along(ids), labels = ids, las = 1)
  for (i in seq_along(ids)) { z <- ev[as.character(ev$detector_id) == ids[i], , drop = FALSE]; if (nrow(z)) graphics::segments(z$start_time, i, z$end_time, i, lwd = 4) }; invisible(ev)
}

#' Plot pairwise detector agreement
#' @export
plot_detector_agreement <- function(x, metric = "mean_event_overlap", ...) {
  a <- estimate_detector_agreement(x); if (nrow(a) && !metric %in% names(a)) .edm_stop("Unknown agreement metric."); ids <- sort(unique(c(as.character(a$detector_a), as.character(a$detector_b)))); mat <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids)); diag(mat) <- 1
  if (nrow(a)) for (i in seq_len(nrow(a))) mat[a$detector_a[i], a$detector_b[i]] <- mat[a$detector_b[i], a$detector_a[i]] <- a[[metric]][i]
  graphics::image(seq_along(ids), seq_along(ids), mat, axes = FALSE, xlab = "Detector", ylab = "Detector", main = paste("Detector agreement:", metric), ...); graphics::axis(1, seq_along(ids), ids, las = 2); graphics::axis(2, seq_along(ids), ids, las = 2); invisible(mat)
}

