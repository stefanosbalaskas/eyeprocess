#' Compare nominal and perturbed AOI assignments
#' @export
compare_aoi_assignments <- function(baseline, perturbed, ids = seq_along(baseline)) {
  if (length(baseline) != length(perturbed) || length(ids) != length(baseline)) .aoi_stop("Assignments and IDs must have equal length.")
  missing <- is.na(baseline) | is.na(perturbed); comparable <- !missing
  unchanged <- comparable & baseline == perturbed; newly <- comparable & baseline == .aoi_outside & perturbed != .aoi_outside
  lost <- comparable & baseline != .aoi_outside & perturbed == .aoi_outside; reassigned <- comparable & !(unchanged | newly | lost)
  detail <- data.frame(observation_id = ids, baseline_aoi = baseline, perturbed_aoi = perturbed, unchanged = unchanged,
                       newly_assigned = newly, lost_assignment = lost, reassigned = reassigned,
                       missing_comparison = missing, stringsAsFactors = FALSE)
  n <- sum(comparable)
  summary <- data.frame(n_total = length(baseline), n_comparable = n,
                        proportion_unchanged = if (n) mean(unchanged[comparable]) else NA_real_,
                        proportion_newly_assigned = if (n) mean(newly[comparable]) else NA_real_,
                        proportion_lost = if (n) mean(lost[comparable]) else NA_real_,
                        proportion_reassigned = if (n) mean(reassigned[comparable]) else NA_real_,
                        proportion_missing_comparison = if (length(baseline)) mean(missing) else NA_real_)
  .aoi_result("eye_aoi_assignment_comparison", detail = detail, summary = summary,
              reassignment_matrix = table(detail$baseline_aoi[comparable], detail$perturbed_aoi[comparable], useNA = "no"))
}

#' Estimate AOI assignment stability
#' @export
estimate_aoi_assignment_stability <- function(comparisons, metadata = NULL, group_cols = NULL) {
  rows <- lapply(names(comparisons), function(pid) {
    d <- comparisons[[pid]]$detail; d$perturbation_id <- pid; d
  })
  source <- do.call(rbind, rows); if (is.null(source) || !nrow(source)) .aoi_stop("No assignment comparisons are available.")
  if (!is.null(metadata)) {
    if (!is.data.frame(metadata)) .aoi_stop("`metadata` must be a data frame.")
    if (!"observation_id" %in% names(metadata)) metadata$observation_id <- seq_len(nrow(metadata))
    if (nrow(metadata) != length(unique(source$observation_id))) .aoi_stop("`metadata` must contain one row per observation.")
    source <- merge(source, metadata, by = "observation_id", all.x = TRUE, sort = FALSE)
  }
  summarise <- function(d, keys) {
    sk <- if (!length(keys)) factor(rep("all", nrow(d))) else interaction(d[, keys, drop = FALSE], drop = TRUE, lex.order = TRUE)
    do.call(rbind, lapply(split(d, sk), function(z) {
      comparable <- !z$missing_comparison; n <- sum(comparable)
      key_values <- if (length(keys)) z[1L, keys, drop = FALSE] else data.frame()
      cbind(key_values, data.frame(n_total = nrow(z), n_comparable = n,
        proportion_unchanged = if (n) mean(z$unchanged[comparable]) else NA_real_,
        proportion_newly_assigned = if (n) mean(z$newly_assigned[comparable]) else NA_real_,
        proportion_lost = if (n) mean(z$lost_assignment[comparable]) else NA_real_,
        proportion_reassigned = if (n) mean(z$reassigned[comparable]) else NA_real_))
    }))
  }
  overall <- summarise(source, "perturbation_id"); group_summaries <- list()
  if (!is.null(group_cols)) for (col in group_cols) {
    if (!col %in% names(source)) .aoi_stop("Grouping column `", col, "` is unavailable.")
    group_summaries[[col]] <- summarise(source, c("perturbation_id", col))
  }
  tmp <- source; tmp$aoi <- tmp$baseline_aoi
  .aoi_result("eye_aoi_assignment_stability", detail = source, overall = overall, group_summaries = group_summaries,
              aoi_level = summarise(tmp, c("perturbation_id", "aoi")),
              caveat = "Stability frequencies describe sensitivity to the specified perturbations; they are not probabilities that AOI assignments are scientifically true.")
}

#' Estimate empirical AOI assignment frequencies
#' @export
estimate_fixation_assignment_probability <- function(assignments, ids = NULL, include_baseline = TRUE) {
  if (!is.list(assignments) || !length(assignments)) .aoi_stop("`assignments` must be a named list of vectors.")
  lengths <- unique(lengths(assignments)); if (length(lengths) != 1L) .aoi_stop("All assignment vectors must have equal length.")
  n <- lengths[1L]; if (is.null(ids)) ids <- seq_len(n); rows <- list()
  for (pid in names(assignments)) {
    if (!include_baseline && pid == "baseline") next
    rows[[length(rows) + 1L]] <- data.frame(perturbation_id = pid, observation_id = ids, aoi = assignments[[pid]], stringsAsFactors = FALSE)
  }
  long <- do.call(rbind, rows); long <- long[!is.na(long$aoi), , drop = FALSE]
  if (!nrow(long)) return(data.frame(observation_id = numeric(), aoi = character(), assignment_count = integer(), n_perturbations = integer(), assignment_frequency = numeric()))
  counts <- aggregate(rep(1L, nrow(long)), by = list(observation_id = long$observation_id, aoi = long$aoi), FUN = sum); names(counts)[3L] <- "assignment_count"
  den <- aggregate(long$perturbation_id, by = list(observation_id = long$observation_id), FUN = function(x) length(unique(x))); names(den)[2L] <- "n_perturbations"
  out <- merge(counts, den, by = "observation_id", sort = FALSE); out$assignment_frequency <- out$assignment_count / out$n_perturbations
  attr(out, "caveat") <- "Assignment frequency is descriptive, not a posterior probability of true AOI membership."; out
}

#' Recompute AOI features under one assignment
#' @export
recompute_aoi_features <- function(data, assignments, participant_col = NULL, trial_col = NULL,
                                   duration_col = NULL, time_col = NULL, perturbation_id = NULL) {
  if (!is.data.frame(data) || length(assignments) != nrow(data)) .aoi_stop("Assignments must contain one value per data row.")
  d <- data; d$aoi_assignment <- assignments; group_cols <- unlist(Filter(Negate(is.null), list(participant_col, trial_col)), use.names = FALSE)
  if (!all(group_cols %in% names(d))) .aoi_stop("One or more grouping columns are absent.")
  if (!is.null(duration_col) && !duration_col %in% names(d)) .aoi_stop("`duration_col` is absent.")
  if (!is.null(time_col) && !time_col %in% names(d)) .aoi_stop("`time_col` is absent.")
  if (is.null(duration_col)) .aoi_warn("No `duration_col` supplied; dwell is returned as NA rather than inferred.")
  d <- d[!is.na(d$aoi_assignment) & !d$aoi_assignment %in% c(.aoi_outside, .aoi_ambiguous), , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  keys <- c(group_cols, "aoi_assignment"); parts <- split(d, interaction(d[, keys, drop = FALSE], drop = TRUE, lex.order = TRUE))
  do.call(rbind, lapply(parts, function(z) {
    base <- if (length(group_cols)) z[1L, group_cols, drop = FALSE] else data.frame()
    dwell <- if (is.null(duration_col)) NA_real_ else { v <- suppressWarnings(as.numeric(z[[duration_col]])); if (any(is.finite(v))) sum(v[is.finite(v)]) else NA_real_ }
    first <- if (is.null(time_col)) min(as.numeric(rownames(z))) else { v <- suppressWarnings(as.numeric(z[[time_col]])); if (any(is.finite(v))) min(v[is.finite(v)]) else NA_real_ }
    cbind(base, data.frame(aoi = z$aoi_assignment[1L], fixation_count = nrow(z), dwell = dwell, first_fixation = first,
                           inspected = TRUE, perturbation_id = perturbation_id, stringsAsFactors = FALSE))
  }))
}

.aoi_validate_model_table <- function(x, pid) {
  if (!is.data.frame(x)) .aoi_stop("Model callback result must be a data frame.")
  required <- c("term", "estimate", "SE", "CI_low", "CI_high", "p_value", "model_converged", "N")
  missing <- setdiff(required, names(x)); if (length(missing)) .aoi_stop("Model callback result is missing: ", paste(missing, collapse = ", "))
  x$perturbation_id <- pid; est <- suppressWarnings(as.numeric(x$estimate))
  x$direction <- ifelse(est > 0, "positive", ifelse(est < 0, "negative", "zero"))
  x[, c("perturbation_id", required, "direction", setdiff(names(x), c("perturbation_id", required, "direction"))), drop = FALSE]
}

#' Run AOI perturbation sensitivity analysis
#' @export
run_aoi_sensitivity_analysis <- function(
    data, aois, grid, x_col, y_col, observation_id_col = NULL, participant_col = NULL,
    trial_col = NULL, duration_col = NULL, time_col = NULL, overlap_policy = "ambiguous",
    model_callback = NULL, preprocessing_specification = NULL, event_detector = NULL,
    quality_rules = NULL, model_specification = NULL) {
  if (!is.data.frame(data)) .aoi_stop("`data` must be a data frame.")
  geometry <- validate_aoi_geometry(aois)$geometry; ids <- if (is.null(observation_id_col)) seq_len(nrow(data)) else data[[observation_id_col]]
  if (anyDuplicated(ids)) .aoi_stop("Observation IDs must be unique.")
  grid_result <- apply_aoi_perturbation_grid(geometry, grid); completed <- grid_result$audit$perturbation_id[grid_result$audit$status == "completed"]
  if (!"baseline" %in% completed) .aoi_stop("Sensitivity analysis requires a successful `baseline` branch.")
  assignments <- list(); features <- list(); models <- list()
  failures <- grid_result$audit[grid_result$audit$status == "failed", c("perturbation_id", "message"), drop = FALSE]
  if (nrow(failures)) failures$stage <- "geometry" else failures <- data.frame(perturbation_id = character(), message = character(), stage = character())
  specs <- setNames(grid$specifications, vapply(grid$specifications, function(x) x$perturbation_id, character(1)))
  for (pid in completed) {
    assigned <- .aoi_assign_points(data, grid_result$geometries[[pid]], x_col, y_col, overlap_policy); assignments[[pid]] <- assigned
    feat <- recompute_aoi_features(data, assigned, participant_col, trial_col, duration_col, time_col, pid); features[[pid]] <- feat
    if (!is.null(model_callback)) {
      assigned_data <- data; assigned_data$aoi_assignment <- assigned; assigned_data$perturbation_id <- pid
      fit <- tryCatch(.aoi_validate_model_table(model_callback(feat, assigned_data, specs[[pid]]), pid), error = identity)
      if (inherits(fit, "error")) failures <- rbind(failures, data.frame(perturbation_id = pid, message = conditionMessage(fit), stage = "model")) else models[[length(models) + 1L]] <- fit
    }
  }
  comparisons <- lapply(assignments, function(x) compare_aoi_assignments(assignments$baseline, x, ids))
  metadata_cols <- unlist(Filter(Negate(is.null), list(participant_col, trial_col)), use.names = FALSE); metadata <- data.frame(observation_id = ids)
  for (nm in metadata_cols) metadata[[nm]] <- data[[nm]]
  stability <- estimate_aoi_assignment_stability(comparisons, metadata, metadata_cols)
  .aoi_result("eye_aoi_sensitivity", nominal_aois = geometry, grid = grid, grid_result = grid_result,
              assignments = assignments, comparisons = comparisons, stability = stability,
              assignment_probability = estimate_fixation_assignment_probability(assignments, ids),
              features = features, models = if (length(models)) do.call(rbind, models) else data.frame(),
              failures = failures,
              provenance = list(source_data_hash = .aoi_hash(data), aoi_specification_hash = .aoi_hash(geometry),
                                preprocessing_specification = preprocessing_specification, event_detector = event_detector,
                                quality_rules = quality_rules, model_specification = model_specification,
                                overlap_policy = overlap_policy, software = list(package = "eyeprocess")),
              caveat = "Robustness proportions summarize the declared perturbation set; they are not probabilities that the substantive conclusion is true.")
}

#' Assess coefficient stability across AOI perturbations
#' @export
assess_aoi_inference_stability <- function(x, term = NULL) {
  d <- x$models; if (is.null(d) || !nrow(d)) return(data.frame())
  if (!is.null(term)) { d <- d[as.character(d$term) == as.character(term), , drop = FALSE]; if (!nrow(d)) .aoi_stop("No model rows found for term `", term, "`.") }
  out <- do.call(rbind, lapply(split(d, d$term), function(z) {
    converged <- !is.na(z$model_converged) & as.logical(z$model_converged); usable <- z[converged, , drop = FALSE]
    base <- usable[usable$perturbation_id == "baseline", , drop = FALSE]; baseline_sign <- if (nrow(base)) sign(as.numeric(base$estimate[1L])) else NA_real_
    est <- suppressWarnings(as.numeric(usable$estimate)); ciw <- suppressWarnings(as.numeric(usable$CI_high) - as.numeric(usable$CI_low))
    data.frame(term = as.character(z$term[1L]), n_models = nrow(z), n_converged = sum(converged),
      convergence_proportion = mean(converged),
      same_sign_proportion = if (is.finite(baseline_sign) && length(est)) mean(sign(est) == baseline_sign) else NA_real_,
      median_estimate = if (length(est)) stats::median(est, na.rm = TRUE) else NA_real_,
      min_estimate = if (length(est)) min(est, na.rm = TRUE) else NA_real_,
      max_estimate = if (length(est)) max(est, na.rm = TRUE) else NA_real_,
      median_CI_width = if (length(ciw)) stats::median(ciw, na.rm = TRUE) else NA_real_)
  }))
  attr(out, "caveat") <- "Same-sign and convergence proportions are descriptive sensitivity summaries, not probabilities that an effect is true."; out
}

#' Summarise AOI sensitivity analysis
#' @export
summarise_aoi_sensitivity <- function(x) {
  .aoi_result("eye_aoi_sensitivity_summary", assignment_stability = x$stability$overall,
              inference_stability = assess_aoi_inference_stability(x), perturbation_audit = x$grid_result$audit,
              failures = x$failures, n_planned = nrow(x$grid_result$audit),
              n_completed = sum(x$grid_result$audit$status == "completed"),
              n_geometry_failed = sum(x$grid_result$audit$status == "failed"),
              n_model_failures = sum(x$failures$stage == "model"), caveat = x$caveat)
}

#' Report AOI sensitivity analysis
#' @export
report_aoi_sensitivity <- function(x) {
  s <- summarise_aoi_sensitivity(x); m <- if (nrow(s$assignment_stability)) stats::median(s$assignment_stability$proportion_unchanged, na.rm = TRUE) else NA_real_
  paste(c("## AOI perturbation sensitivity analysis", "",
          sprintf("Planned perturbations: %d; completed geometry branches: %d; geometry failures: %d; model callback failures: %d.",
                  s$n_planned, s$n_completed, s$n_geometry_failed, s$n_model_failures),
          if (is.finite(m)) sprintf("Median unchanged AOI assignment across completed perturbations: %.3f.", m) else "Assignment stability could not be summarized.",
          "", "Interpretation: these quantities describe robustness to the declared AOI perturbations. They are not probabilities that the scientific conclusion is true."),
        collapse = "\n")
}

