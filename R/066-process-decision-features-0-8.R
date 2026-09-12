# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Response-process feature families useful for pre-action and leakage-safe
# predictive/measurement representations. These are proxies, not fitted DDM/GLAM
# parameters or causal mechanisms.

#' Build pre-action process features
#'
#' @param data Sample-level data.
#' @param by Grouping columns.
#' @param time,response_time Time and response-event columns.
#' @param windows_ms Positive look-back windows before action.
#' @param aoi,pupil,blink Optional feature columns.
#' @export
preaction_process_features <- function(
    data, by = c("person_id", "trial_id"), time = "time_ms", response_time = "response_time_ms",
    windows_ms = c(500, 1000, 2000), aoi = "aoi", pupil = "pupil_bc", blink = "blink") {
  data <- .ep08_as_df(data)
  .ep08_req_cols(data, c(by, time, response_time))
  windows_ms <- unique(.ep08_num(windows_ms))
  windows_ms <- windows_ms[is.finite(windows_ms) & windows_ms > 0]
  if (!length(windows_ms)) stop("windows_ms must contain at least one positive finite window.", call. = FALSE)
  global_aois <- if (aoi %in% names(data)) {
    sort(unique(as.character(data[[aoi]])[!is.na(data[[aoi]]) & nzchar(as.character(data[[aoi]]))]))
  } else character()
  groups <- .ep08_split_rows(data, by)
  rows <- list(); k <- 0L
  for (idx in groups) {
    z <- data[idx, , drop = FALSE]
    tt <- .ep08_num(z[[time]])
    rt <- .ep08_first_finite(z[[response_time]])
    if (!is.finite(rt)) next
    rel <- tt - rt
    for (w in windows_ms) {
      keep <- is.finite(rel) & rel >= -w & rel <= 0
      zz <- z[keep, , drop = FALSE]
      rr <- rel[keep]
      if (nrow(zz) < 3L) next
      av <- if (aoi %in% names(zz)) as.character(zz[[aoi]]) else rep(NA_character_, nrow(zz))
      pv <- if (pupil %in% names(zz)) .ep08_num(zz[[pupil]]) else rep(NA_real_, nrow(zz))
      bv <- if (blink %in% names(zz)) as.numeric(.ep08_bool(zz[[blink]])) else rep(NA_real_, nrow(zz))
      base <- .ep08_group_values(data, idx, by)
      row <- cbind(base, data.frame(
        pre_window_ms = w,
        n_samples = nrow(zz),
        aoi_entropy = .ep08_entropy_factor(av),
        aoi_switch_count = .ep08_switch_count(av),
        pupil_mean = .ep08_mean(pv),
        pupil_slope = .ep08_slope(pv, rr),
        blink_prop = .ep08_mean(bv),
        stringsAsFactors = FALSE
      ))
      for (lv in global_aois) row[[paste0("aoi_prop__", make.names(lv))]] <- if (any(!is.na(av))) mean(av == lv, na.rm = TRUE) else NA_real_
      k <- k + 1L; rows[[k]] <- row
    }
  }
  tab <- if (length(rows)) do.call(rbind, rows) else data.frame()
  structure(list(data = tab, windows_ms = windows_ms, by = by,
                 status = "preaction_process_representation",
                 caveat = "Pre-action features describe observed process dynamics; they are not evidence of latent intention by themselves."),
            class = "eye_preaction_process_features")
}

#' Compute aDDM/GLAM-inspired gaze-evidence proxy features
#'
#' @param data Sample-level data.
#' @param by Grouping columns.
#' @param time,aoi Columns.
#' @param target_aoi,distractor_aoi,action_aoi AOI labels.
#' @export
addm_glam_proxy_features <- function(
    data, by = c("person_id", "trial_id"), time = "time_ms", aoi = "aoi",
    target_aoi = "target", distractor_aoi = "distractor", action_aoi = "button") {
  data <- .ep08_as_df(data); .ep08_req_cols(data, c(by, time, aoi))
  groups <- .ep08_split_rows(data, by)
  rows <- lapply(groups, function(idx) {
    z <- data[idx, , drop = FALSE]
    av <- as.character(z[[aoi]]); tt <- .ep08_num(z[[time]])
    evidence <- ifelse(av == target_aoi, 1, ifelse(av == distractor_aoi, -1, 0))
    action_evidence <- ifelse(av == action_aoi, 1,
                              ifelse(av %in% c(target_aoi, distractor_aoi), -1, 0))
    target_prop <- .ep08_mean(as.numeric(av == target_aoi))
    distractor_prop <- .ep08_mean(as.numeric(av == distractor_aoi))
    action_prop <- .ep08_mean(as.numeric(av == action_aoi))
    eps <- 1e-6
    split_point <- stats::median(tt, na.rm = TRUE)
    early <- evidence[tt < split_point]; late <- evidence[tt >= split_point]
    cbind(.ep08_group_values(data, idx, by), data.frame(
      target_minus_distractor_prop = target_prop - distractor_prop,
      evidence_slope = .ep08_slope(evidence, tt),
      action_evidence_slope = .ep08_slope(action_evidence, tt),
      late_minus_early_evidence = .ep08_mean(late) - .ep08_mean(early),
      relative_target_attention = target_prop / (target_prop + distractor_prop + eps),
      relative_action_attention = action_prop / (action_prop + target_prop + distractor_prop + eps),
      gaze_discount_proxy = distractor_prop / (target_prop + distractor_prop + eps),
      choice_caution_proxy = .ep08_switch_count(av) / (abs(.ep08_mean(evidence)) + .01),
      stringsAsFactors = FALSE
    ))
  })
  tab <- do.call(rbind, rows); rownames(tab) <- NULL
  structure(list(features = tab, by = by,
                 status = "decision_process_proxy_features",
                 caveat = paste(
                   "These are aDDM/GLAM-inspired descriptive proxies, not fitted drift rate, gaze discount,",
                   "decision threshold, or causal attention parameters."
                 )), class = "eye_decision_process_proxy")
}

#' Registry of process-feature families and interpretation guardrails
#' @export
process_feature_family_registry <- function() {
  data.frame(
    pattern = c(
      "transition|switch|entropy|scanpath|sequence", "aoi|target|distractor|button|text",
      "pupil|phasic|tonic|ripa|frequency", "gaze|fixation|saccade|dwell|ttff",
      "valid|trackloss|missing|confidence|quality", "rt|response_time|trial_order",
      "condition|task|stimulus|layout|luminance"
    ),
    family = c(
      "Scanpath organization", "AOI attention", "Pupil dynamics", "Gaze dynamics",
      "Data quality", "Timing", "Design/context"
    ),
    warning = c(
      "Correlated with dwell and task structure.",
      "May reflect task design or option relevance.",
      "May reflect luminance, arousal, effort, fatigue, or motor preparation.",
      "May reflect viewing constraints and layout as well as processing.",
      "Must not be interpreted psychologically.",
      "May reflect task design, motor timing, or speededness.",
      "Design/context variables can dominate prediction and must be audited separately."
    ), stringsAsFactors = FALSE
  )
}

#' Assign features to conservative process-feature families
#' @param feature_names Feature names.
#' @param registry Feature-family registry.
#' @export
assign_process_feature_family <- function(feature_names, registry = process_feature_family_registry()) {
  vapply(as.character(feature_names), function(f) {
    hit <- which(vapply(registry$pattern, function(p) grepl(p, f, ignore.case = TRUE), logical(1)))
    if (!length(hit)) "Other" else registry$family[hit[1L]]
  }, character(1))
}

#' Summarize process-feature stability across repeated analyses
#'
#' @param data Long table containing feature names and ranks/importance values.
#' @param feature Feature column.
#' @param split Split/resample column.
#' @param importance Importance column, where larger is better.
#' @param top_n Number of top features counted per split.
#' @export
process_feature_stability <- function(data, feature = "feature", split = "split",
                                      importance = "importance", top_n = 20L) {
  data <- .ep08_as_df(data); .ep08_req_cols(data, c(feature, split, importance))
  top_n <- as.integer(top_n)
  if (top_n < 1L) stop("top_n must be at least 1.", call. = FALSE)
  d <- data[, c(feature, split, importance), drop = FALSE]
  names(d) <- c("feature", "split", "importance")
  d$importance <- .ep08_num(d$importance)
  d <- d[!is.na(d$feature) & !is.na(d$split) & is.finite(d$importance), , drop = FALSE]
  groups <- base::split(d, d$split, drop = TRUE)
  if (!length(groups)) stop("No complete split/importance rows are available.", call. = FALSE)
  allf <- unique(d$feature)
  out <- data.frame(
    feature = allf,
    top_n_selection_rate = vapply(allf, function(f) mean(vapply(groups, function(z) {
      zz <- z[order(z$importance, decreasing = TRUE), , drop = FALSE]
      f %in% head(zz$feature, as.integer(top_n))
    }, logical(1))), numeric(1)),
    mean_importance = vapply(allf, function(f) .ep08_mean(d$importance[d$feature == f]), numeric(1)),
    stringsAsFactors = FALSE
  )
  out$feature_family <- assign_process_feature_family(out$feature)
  out[order(out$top_n_selection_rate, out$mean_importance, decreasing = TRUE), , drop = FALSE]
}

#' Plot preaction process features diagnostics
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param feature Process feature to evaluate or display.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_preaction_process_features <- function(x, feature = "pupil_mean", ...) {
  d <- x$data
  if (!nrow(d) || !feature %in% names(d)) return(.ep08_plot_empty("Pre-action process features"))
  agg <- stats::aggregate(d[[feature]], by = list(window_ms = d$pre_window_ms), FUN = .ep08_mean)
  names(agg)[2L] <- "value"
  graphics::plot(agg$window_ms, agg$value, type = "b", xlab = "Look-back window (ms)",
                 ylab = feature, main = "Pre-action process feature by horizon", ...)
  invisible(agg)
}

#' Plot decision process proxy diagnostics
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
plot.eye_decision_process_proxy <- function(x, ...) {
  d <- x$features
  vars <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], x$by)
  if (!length(vars)) return(.ep08_plot_empty("Decision-process proxies"))
  vals <- vapply(vars, function(v) .ep08_mean(d[[v]]), numeric(1))
  graphics::barplot(vals, las = 2, ylab = "Mean proxy value",
                    main = "aDDM/GLAM-inspired process proxies", ...)
  invisible(d)
}
