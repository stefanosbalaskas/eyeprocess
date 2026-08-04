build_trials <- function(
    x,
    start_events = c("TRIAL_START", "TRIALID", "START_TRIAL"),
    end_events = c("TRIAL_END", "TRIAL_RESULT", "END_TRIAL"),
    event_field = c("event_name", "event_value"),
    trial_id_pattern = NULL,
    close_open = c("recording_end", "next_start", "drop"),
    overwrite = FALSE) {
  .assert_eye_dataset(x)
  close_open <- match.arg(close_open)
  event_field <- match.arg(event_field)
  if (!nrow(x$events)) .eye_stop("No events are available for trial reconstruction.")
  ev <- x$events[order(x$events$recording_id, x$events$timestamp_seconds), , drop = FALSE]
  val <- as.character(ev[[event_field]])
  match_patterns <- function(patterns, values) {
    if (!length(patterns)) return(rep(FALSE, length(values)))
    Reduce(`|`, lapply(patterns, function(p) {
      values %in% p | grepl(p, values, ignore.case = TRUE, perl = TRUE)
    }))
  }
  starts <- which(match_patterns(start_events, val))
  ends <- which(match_patterns(end_events, val))
  intervals <- list(); k <- 0L
  for (rec in unique(ev$recording_id)) {
    idx <- which(ev$recording_id == rec)
    sidx <- intersect(idx, starts)
    eidx <- intersect(idx, ends)
    if (!length(sidx)) next
    for (j in seq_along(sidx)) {
      s <- sidx[j]
      candidates <- eidx[eidx > s]
      next_start_time <- if (j < length(sidx)) ev$timestamp_seconds[sidx[j + 1L]] else Inf
      candidates <- candidates[ev$timestamp_seconds[candidates] <= next_start_time]
      if (length(candidates)) {
        e <- candidates[1L]
        end_time <- ev$timestamp_seconds[e]
      } else if (close_open == "next_start" && is.finite(next_start_time)) {
        end_time <- next_start_time
      } else if (close_open == "recording_end") {
        all_times <- c(
          x$gaze_samples$timestamp_seconds[x$gaze_samples$recording_id == rec],
          x$eye_samples$timestamp_seconds[x$eye_samples$recording_id == rec],
          x$biometrics$timestamp_seconds[x$biometrics$recording_id == rec]
        )
        end_time <- suppressWarnings(max(all_times, na.rm = TRUE))
        if (!is.finite(end_time)) end_time <- NA_real_
      } else {
        next
      }
      raw_id <- ev$event_value[s]
      if (is.na(raw_id) || !nzchar(raw_id)) raw_id <- ev$event_name[s]
      if (!is.null(trial_id_pattern)) {
        m <- regexec(trial_id_pattern, raw_id, perl = TRUE)
        hit <- regmatches(raw_id, m)[[1L]]
        raw_id <- if (length(hit) > 1L) hit[2L] else raw_id
      }
      k <- k + 1L
      trial_id <- if (!is.na(raw_id) && nzchar(raw_id) && !raw_id %in% start_events) raw_id else paste0(rec, "_trial_", sprintf("%05d", j))
      intervals[[k]] <- data.frame(
        interval_id = paste0(rec, "_interval_trial_", sprintf("%05d", j)),
        recording_id = rec, interval_type = "trial",
        start_time = ev$timestamp_seconds[s], end_time = end_time,
        trial_id = trial_id,
        participant_id = .first_nonmissing(x$recordings$participant_id[x$recordings$recording_id == rec]),
        item_id = NA_character_, stimulus_id = ev$stimulus_id[s], condition_id = NA_character_,
        parent_interval_id = NA_character_, valid_interval = is.finite(end_time) && end_time >= ev$timestamp_seconds[s],
        stringsAsFactors = FALSE
      )
    }
  }
  built <- if (length(intervals)) do.call(.bind_rows_base, intervals) else empty_eye_table("intervals")
  if (!nrow(built)) .eye_stop("No trial intervals could be reconstructed from the selected events.")
  if (isTRUE(overwrite)) x$intervals <- x$intervals[x$intervals$interval_type != "trial", , drop = FALSE]
  x$intervals <- standardize_eye_table(.bind_rows_base(x$intervals, built), "intervals")
  x <- assign_trials(x)
  add_provenance(x, "build_trials", "intervals", paste0(nrow(built), " trials; close_open=", close_open))
}

build_stimulus_intervals <- function(x, source = c("gaze_samples", "events"), overwrite = FALSE) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  out <- list(); k <- 0L
  if (source == "gaze_samples") {
    d <- x$gaze_samples
    if (!nrow(d) || all(is.na(d$stimulus_id))) .eye_stop("No stimulus ids are available in gaze samples.")
    for (rec in unique(d$recording_id)) {
      z <- d[d$recording_id == rec & !is.na(d$stimulus_id), , drop = FALSE]
      z <- z[order(z$timestamp_seconds), ]
      if (!nrow(z)) next
      run <- cumsum(c(TRUE, z$stimulus_id[-1L] != z$stimulus_id[-nrow(z)]))
      for (r in unique(run)) {
        q <- z[run == r, ]
        k <- k + 1L
        out[[k]] <- data.frame(
          interval_id = paste0(rec, "_stim_", sprintf("%05d", k)), recording_id = rec,
          interval_type = "stimulus", start_time = min(q$timestamp_seconds, na.rm = TRUE),
          end_time = max(q$timestamp_seconds, na.rm = TRUE), trial_id = .mode_value(q$trial_id),
          participant_id = .first_nonmissing(x$recordings$participant_id[x$recordings$recording_id == rec]),
          item_id = NA_character_, stimulus_id = q$stimulus_id[1L], condition_id = NA_character_,
          parent_interval_id = NA_character_, valid_interval = TRUE, stringsAsFactors = FALSE
        )
      }
    }
  } else {
    ev <- x$events[x$events$event_type == "media_change" | x$events$event_name == "MEDIA_START", ]
    if (!nrow(ev)) .eye_stop("No stimulus/media events found.")
    for (rec in unique(ev$recording_id)) {
      z <- ev[ev$recording_id == rec, ]; z <- z[order(z$timestamp_seconds), ]
      for (j in seq_len(nrow(z))) {
        k <- k + 1L
        end <- if (j < nrow(z)) z$timestamp_seconds[j + 1L] else max(c(
          x$gaze_samples$timestamp_seconds[x$gaze_samples$recording_id == rec],
          x$events$timestamp_seconds[x$events$recording_id == rec]
        ), na.rm = TRUE)
        out[[k]] <- data.frame(
          interval_id = paste0(rec, "_stim_", sprintf("%05d", k)), recording_id = rec,
          interval_type = "stimulus", start_time = z$timestamp_seconds[j], end_time = end,
          trial_id = z$trial_id[j], participant_id = .first_nonmissing(x$recordings$participant_id[x$recordings$recording_id == rec]),
          item_id = NA_character_, stimulus_id = z$event_value[j], condition_id = NA_character_,
          parent_interval_id = NA_character_, valid_interval = is.finite(end), stringsAsFactors = FALSE
        )
      }
    }
  }
  built <- do.call(.bind_rows_base, out)
  if (isTRUE(overwrite)) x$intervals <- x$intervals[x$intervals$interval_type != "stimulus", , drop = FALSE]
  x$intervals <- standardize_eye_table(.bind_rows_base(x$intervals, built), "intervals")
  x <- assign_trials(x)
  add_provenance(x, "build_stimulus_intervals", "intervals", paste0(nrow(built), " intervals"))
}

.find_interval_id <- function(time, recording, intervals, field) {
  out <- rep(NA_character_, length(time))
  if (!nrow(intervals)) return(out)
  for (rec in unique(recording)) {
    ridx <- which(recording == rec)
    ints <- intervals[intervals$recording_id == rec & is.finite(intervals$start_time) & is.finite(intervals$end_time), ]
    if (!nrow(ints)) next
    for (j in seq_len(nrow(ints))) {
      hit <- ridx[time[ridx] >= ints$start_time[j] & time[ridx] <= ints$end_time[j] & is.na(out[ridx])]
      if (length(hit)) out[hit] <- as.character(ints[[field]][j])
    }
  }
  out
}

assign_trials <- function(x, interval_type = "trial", overwrite = FALSE) {
  .assert_eye_dataset(x)
  ints <- x$intervals[x$intervals$interval_type == interval_type, , drop = FALSE]
  if (!nrow(ints)) return(x)
  components <- c("gaze_samples", "eye_samples", "events", "biometrics")
  for (nm in components) {
    d <- x[[nm]]
    if (!nrow(d) || !all(c("recording_id", "timestamp_seconds", "trial_id") %in% names(d))) next
    assigned <- .find_interval_id(d$timestamp_seconds, d$recording_id, ints, "trial_id")
    if (overwrite) d$trial_id <- assigned else d$trial_id[is.na(d$trial_id) | !nzchar(d$trial_id)] <- assigned[is.na(d$trial_id) | !nzchar(d$trial_id)]
    x[[nm]] <- d
  }
  if (nrow(x$episodes)) {
    d <- x$episodes
    assigned <- .find_interval_id(d$start_time, d$recording_id, ints, "trial_id")
    if (overwrite) d$trial_id <- assigned else d$trial_id[is.na(d$trial_id) | !nzchar(d$trial_id)] <- assigned[is.na(d$trial_id) | !nzchar(d$trial_id)]
    x$episodes <- d
  }
  add_provenance(x, "assign_trials", "dataset", paste0("interval_type=", interval_type, ";overwrite=", overwrite))
}

add_responses <- function(x, responses, overwrite = FALSE) {
  .assert_eye_dataset(x); .assert_data_frame(responses, "responses")
  required <- c("participant_id", "item_id", "response")
  .assert_columns(responses, required, "responses")
  if (!"response_id" %in% names(responses)) responses$response_id <- .unique_id("response_", nrow(responses))
  if (!"recording_id" %in% names(responses)) responses$recording_id <- NA_character_
  if (!"trial_id" %in% names(responses)) responses$trial_id <- NA_character_
  if (!"score" %in% names(responses)) responses$score <- NA_real_
  if (!"response_time" %in% names(responses)) responses$response_time <- NA_real_
  if (!"response_timestamp" %in% names(responses)) responses$response_timestamp <- NA_real_
  if (!"response_type" %in% names(responses)) responses$response_type <- "observed"
  if (!"valid_response" %in% names(responses)) responses$valid_response <- TRUE
  responses <- standardize_eye_table(responses, "responses")
  if (overwrite) {
    key_new <- paste(responses$participant_id, responses$item_id, responses$trial_id, sep = "\r")
    key_old <- paste(x$responses$participant_id, x$responses$item_id, x$responses$trial_id, sep = "\r")
    x$responses <- x$responses[!key_old %in% key_new, , drop = FALSE]
  }
  x$responses <- standardize_eye_table(.bind_rows_base(x$responses, responses), "responses")
  add_provenance(x, "add_responses", "responses", paste0(nrow(responses), " responses"))
}

build_item_responses <- function(x, score_key = NULL, response_type = "observed") {
  .assert_eye_dataset(x)
  trials <- x$intervals[x$intervals$interval_type == "trial", , drop = FALSE]
  if (!nrow(trials)) .eye_stop("Trial intervals are required.")
  existing <- x$responses
  if (nrow(existing)) return(x)
  response <- rep(NA_character_, nrow(trials)); score <- rep(NA_real_, nrow(trials))
  if (!is.null(score_key)) {
    if (is.null(names(score_key))) .eye_stop("`score_key` must be named by item id.")
    score <- as.numeric(response == unname(score_key[trials$item_id]))
  }
  responses <- data.frame(
    response_id = paste0(trials$recording_id, "_response_", sprintf("%05d", seq_len(nrow(trials)))),
    recording_id = trials$recording_id, participant_id = trials$participant_id,
    trial_id = trials$trial_id, item_id = trials$item_id, response = response,
    score = score, response_time = trials$end_time - trials$start_time,
    response_timestamp = trials$end_time, response_type = response_type,
    valid_response = TRUE, stringsAsFactors = FALSE
  )
  add_responses(x, responses)
}

# AOIs --------------------------------------------------------------------

new_aoi <- function(
    aoi_id,
    aoi_name = aoi_id,
    stimulus_id = NA_character_,
    shape = c("rectangle", "circle", "polygon"),
    x = NA_real_,
    y = NA_real_,
    width = NA_real_,
    height = NA_real_,
    polygon = NULL,
    coordinate_space_id = "coord_display_normalized_top_left",
    valid_from = -Inf,
    valid_to = Inf,
    frame_id = NA_character_,
    visible = TRUE,
    parent_aoi_id = NA_character_,
    source = "user") {
  shape <- match.arg(shape)
  if (shape == "polygon") {
    if (!is.matrix(polygon) || ncol(polygon) != 2L) .eye_stop("Polygon AOIs require a two-column coordinate matrix.")
  }
  structure(list(
    definition = data.frame(
      aoi_id = as.character(aoi_id), aoi_name = as.character(aoi_name),
      stimulus_id = as.character(stimulus_id), shape_type = shape,
      coordinate_space_id = as.character(coordinate_space_id),
      parent_aoi_id = as.character(parent_aoi_id), source = as.character(source),
      stringsAsFactors = FALSE
    ),
    geometry = data.frame(
      aoi_id = as.character(aoi_id), valid_from = as.numeric(valid_from), valid_to = as.numeric(valid_to),
      frame_id = as.character(frame_id), x = as.numeric(x), y = as.numeric(y),
      width = as.numeric(width), height = as.numeric(height),
      polygon = I(list(polygon)), visible = isTRUE(visible),
      coordinate_space_id = as.character(coordinate_space_id), stringsAsFactors = FALSE
    )
  ), class = "eye_aoi")
}

print.eye_aoi <- function(x, ...) {
  cat("<eye_aoi ", x$definition$aoi_id, ": ", x$definition$shape_type, ">\n", sep = "")
  invisible(x)
}

register_aois <- function(x, ..., overwrite = FALSE) {
  .assert_eye_dataset(x)
  aois <- list(...)
  if (length(aois) == 1L && is.list(aois[[1L]]) && !inherits(aois[[1L]], "eye_aoi")) aois <- aois[[1L]]
  if (!length(aois) || !all(vapply(aois, inherits, logical(1), "eye_aoi"))) .eye_stop("Supply one or more `new_aoi()` objects.")
  defs <- do.call(.bind_rows_base, lapply(aois, `[[`, "definition"))
  geoms <- do.call(.bind_rows_base, lapply(aois, `[[`, "geometry"))
  if (overwrite) {
    ids <- defs$aoi_id
    x$aoi_definitions <- x$aoi_definitions[!x$aoi_definitions$aoi_id %in% ids, , drop = FALSE]
    x$aoi_geometry <- x$aoi_geometry[!x$aoi_geometry$aoi_id %in% ids, , drop = FALSE]
  } else if (any(defs$aoi_id %in% x$aoi_definitions$aoi_id)) {
    .eye_stop("AOI id already exists; use `overwrite = TRUE`.")
  }
  x$aoi_definitions <- standardize_eye_table(.bind_rows_base(x$aoi_definitions, defs), "aoi_definitions")
  x$aoi_geometry <- standardize_eye_table(.bind_rows_base(x$aoi_geometry, geoms), "aoi_geometry")
  add_provenance(x, "register_aois", "aoi_definitions", paste(defs$aoi_id, collapse = ","))
}

.point_in_polygon <- function(px, py, polygon) {
  if (is.null(polygon) || !is.matrix(polygon) || nrow(polygon) < 3L) return(rep(FALSE, length(px)))
  n <- nrow(polygon); inside <- rep(FALSE, length(px)); j <- n
  for (i in seq_len(n)) {
    xi <- polygon[i, 1L]; yi <- polygon[i, 2L]
    xj <- polygon[j, 1L]; yj <- polygon[j, 2L]
    hit <- ((yi > py) != (yj > py)) & (px < (xj - xi) * (py - yi) / ((yj - yi) + .Machine$double.eps) + xi)
    inside <- xor(inside, hit)
    j <- i
  }
  inside
}

.aoi_contains <- function(x, y, time, def, geom) {
  active <- time >= geom$valid_from & time <= geom$valid_to & isTRUE(geom$visible)
  if (!any(active)) return(rep(FALSE, length(x)))
  shape <- def$shape_type
  if (shape == "rectangle") {
    active & x >= geom$x & x <= geom$x + geom$width & y >= geom$y & y <= geom$y + geom$height
  } else if (shape == "circle") {
    active & ((x - geom$x)^2 / (geom$width / 2)^2 + (y - geom$y)^2 / (geom$height / 2)^2 <= 1)
  } else if (shape == "polygon") {
    active & .point_in_polygon(x, y, geom$polygon[[1L]])
  } else rep(FALSE, length(x))
}

assign_aois <- function(
    x,
    component = c("gaze_samples", "episodes"),
    overlap = c("first", "all", "smallest"),
    overwrite = TRUE) {
  .assert_eye_dataset(x)
  component <- match.arg(component)
  overlap <- match.arg(overlap)
  if (!nrow(x$aoi_definitions) || !nrow(x$aoi_geometry)) .eye_stop("No AOIs are registered.")
  if (component == "gaze_samples") {
    d <- x$gaze_samples
    if (!nrow(d)) return(x)
    assignments <- vector("list", nrow(d))
    for (i in seq_len(nrow(x$aoi_definitions))) {
      def <- x$aoi_definitions[i, ]
      geoms <- x$aoi_geometry[x$aoi_geometry$aoi_id == def$aoi_id, , drop = FALSE]
      for (g in seq_len(nrow(geoms))) {
        geom <- geoms[g, ]
        compatible <- d$coordinate_space_id == geom$coordinate_space_id
        stimulus_ok <- is.na(def$stimulus_id) | !nzchar(def$stimulus_id) | d$stimulus_id == def$stimulus_id
        hit <- compatible & stimulus_ok & .aoi_contains(d$gaze_x, d$gaze_y, d$timestamp_seconds, def, geom)
        idx <- which(hit)
        for (j in idx) assignments[[j]] <- c(assignments[[j]], def$aoi_id)
      }
    }
    if (overlap == "smallest") {
      area_map <- setNames(rep(Inf, nrow(x$aoi_definitions)), x$aoi_definitions$aoi_id)
      for (id in names(area_map)) {
        g <- x$aoi_geometry[x$aoi_geometry$aoi_id == id, ]
        area_map[id] <- min(g$width * g$height, na.rm = TRUE)
      }
      assigned <- vapply(assignments, function(a) if (!length(a)) NA_character_ else a[which.min(area_map[a])], character(1))
    } else if (overlap == "all") {
      assigned <- vapply(assignments, function(a) if (!length(a)) NA_character_ else paste(unique(a), collapse = "|"), character(1))
    } else {
      assigned <- vapply(assignments, function(a) if (!length(a)) NA_character_ else a[1L], character(1))
    }
    if (!"aoi_id" %in% names(d)) d$aoi_id <- NA_character_
    if (overwrite) d$aoi_id <- assigned else d$aoi_id[is.na(d$aoi_id)] <- assigned[is.na(d$aoi_id)]
    x$gaze_samples <- d
  } else {
    d <- x$episodes
    if (!nrow(d)) return(x)
    assignments <- rep(NA_character_, nrow(d))
    for (i in seq_len(nrow(x$aoi_definitions))) {
      def <- x$aoi_definitions[i, ]; geoms <- x$aoi_geometry[x$aoi_geometry$aoi_id == def$aoi_id, ]
      for (g in seq_len(nrow(geoms))) {
        geom <- geoms[g, ]
        hit <- d$coordinate_space_id == geom$coordinate_space_id & .aoi_contains(d$centroid_x, d$centroid_y, d$start_time, def, geom)
        assignments[hit & is.na(assignments)] <- def$aoi_id
      }
    }
    if (overwrite) d$aoi_id <- assignments else d$aoi_id[is.na(d$aoi_id)] <- assignments[is.na(d$aoi_id)]
    x$episodes <- d
  }
  add_provenance(x, "assign_aois", component, paste0("overlap=", overlap))
}

build_aoi_visits <- function(x, gap_tolerance_ms = 75, minimum_duration_ms = 0, source = c("gaze_samples", "episodes")) {
  .assert_eye_dataset(x)
  source <- match.arg(source)
  visits <- list(); k <- 0L
  if (source == "gaze_samples") {
    d <- x$gaze_samples
    if (!"aoi_id" %in% names(d)) .eye_stop("Assign AOIs to gaze samples first.")
    d <- d[!is.na(d$aoi_id), ]; d <- d[order(d$recording_id, d$trial_id, d$timestamp_seconds), ]
    groups <- split(d, interaction(d$recording_id, d$trial_id, drop = TRUE))
    for (z in groups) {
      if (!nrow(z)) next
      dt <- c(Inf, diff(z$timestamp_seconds) * 1000)
      new_run <- c(TRUE, z$aoi_id[-1L] != z$aoi_id[-nrow(z)] | dt[-1L] > gap_tolerance_ms)
      run <- cumsum(new_run)
      for (r in unique(run)) {
        q <- z[run == r, ]; dur <- (max(q$timestamp_seconds) - min(q$timestamp_seconds)) * 1000
        if (dur < minimum_duration_ms) next
        k <- k + 1L
        visits[[k]] <- data.frame(
          episode_id = paste0(q$recording_id[1L], "_visit_", sprintf("%07d", k)),
          recording_id = q$recording_id[1L], episode_type = "aoi_visit", eye = "combined",
          start_time = min(q$timestamp_seconds), end_time = max(q$timestamp_seconds), duration_ms = dur,
          start_x = q$gaze_x[1L], start_y = q$gaze_y[1L], end_x = q$gaze_x[nrow(q)], end_y = q$gaze_y[nrow(q)],
          centroid_x = mean(q$gaze_x, na.rm = TRUE), centroid_y = mean(q$gaze_y, na.rm = TRUE),
          amplitude = NA_real_, peak_velocity = NA_real_, dispersion = NA_real_,
          coordinate_space_id = q$coordinate_space_id[1L], source_algorithm = "AOI run-length encoding",
          source_parameters = paste0("gap_tolerance_ms=", gap_tolerance_ms), derived_by = "eyeprocess",
          trial_id = q$trial_id[1L], stimulus_id = q$stimulus_id[1L], aoi_id = q$aoi_id[1L], stringsAsFactors = FALSE
        )
      }
    }
  } else {
    d <- x$episodes[x$episodes$episode_type == "fixation" & !is.na(x$episodes$aoi_id), ]
    if (!nrow(d)) return(x)
    d <- d[order(d$recording_id, d$trial_id, d$start_time), ]
    groups <- split(d, interaction(d$recording_id, d$trial_id, drop = TRUE))
    for (z in groups) {
      run <- cumsum(c(TRUE, z$aoi_id[-1L] != z$aoi_id[-nrow(z)] | (z$start_time[-1L] - z$end_time[-nrow(z)]) * 1000 > gap_tolerance_ms))
      for (r in unique(run)) {
        q <- z[run == r, ]; dur <- (max(q$end_time) - min(q$start_time)) * 1000
        if (dur < minimum_duration_ms) next
        k <- k + 1L
        visits[[k]] <- transform(q[1L, ],
          episode_id = paste0(q$recording_id[1L], "_visit_", sprintf("%07d", k)),
          episode_type = "aoi_visit", start_time = min(q$start_time), end_time = max(q$end_time),
          duration_ms = dur, centroid_x = mean(q$centroid_x, na.rm = TRUE), centroid_y = mean(q$centroid_y, na.rm = TRUE),
          source_algorithm = "Fixation AOI aggregation", source_parameters = paste0("gap_tolerance_ms=", gap_tolerance_ms), derived_by = "eyeprocess")
      }
    }
  }
  if (length(visits)) x$episodes <- standardize_eye_table(.bind_rows_base(x$episodes, do.call(.bind_rows_base, visits)), "episodes")
  add_provenance(x, "build_aoi_visits", "episodes", paste0(length(visits), " visits"))
}
