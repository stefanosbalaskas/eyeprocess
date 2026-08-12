# eyeprocess 0.9 Milestone #2: paper-ready validation tables and evidence atlas

.ep09m2_round_numeric <- function(x, digits = 4L) {
  out <- x
  is_num <- vapply(out, is.numeric, logical(1))
  out[is_num] <- lapply(out[is_num], round, digits = digits)
  out
}

#' Build a paper-ready parameter-recovery table
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param digits Number of decimal digits used for presentation.
#' @export
eyeprocess_recovery_evidence_table <- function(x, digits = 4L) {
  tab <- if (inherits(x, "eye_irt_recovery_result")) eyeprocess_irt_recovery_summary(x) else .ep09m2_as_df(x, "x")
  needed <- intersect(c("scenario_id", "parameter", "n", "bias", "rmse", "mae", "estimate_sd", "coverage", "failure_rate"), names(tab))
  if (!length(needed)) stop("No recognized recovery columns were found.", call. = FALSE)
  .ep09m2_round_numeric(tab[, needed, drop = FALSE], digits)
}

#' Build a paper-ready SBC table
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param digits Number of decimal digits used for presentation.
#' @export
eyeprocess_sbc_evidence_table <- function(x, digits = 4L) {
  if (inherits(x, "eye_sbc_diagnostics")) {
    tab <- data.frame(n_ranks = length(x$ranks), n_draws = x$n_draws,
                      ecdf_max_deviation = sbc_ecdf_deviation(x), stringsAsFactors = FALSE)
  } else if (inherits(x, "eye_irt_sbc_evidence")) {
    tab <- data.frame(n_ranks = x$n, n_draws = x$n_draws, ecdf_max_deviation = x$ecdf_deviation, stringsAsFactors = FALSE)
    if (!is.null(x$coverage)) tab$coverage <- x$coverage
    if (!is.null(x$nominal_coverage)) tab$nominal_coverage <- x$nominal_coverage
    if (!is.null(x$coverage_error)) tab$coverage_error <- x$coverage_error
  } else tab <- .ep09m2_as_df(x, "x")
  .ep09m2_round_numeric(tab, digits)
}

#' Build a paper-ready stress-test table
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param digits Number of decimal digits used for presentation.
#' @export
eyeprocess_stress_evidence_table <- function(x, digits = 4L) {
  tab <- if (inherits(x, "eye_stress_test_summary")) x$table else .ep09m2_as_df(x, "x")
  .ep09m2_round_numeric(tab, digits)
}

#' Build a paper-ready reliability table
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param digits Number of decimal digits used for presentation.
#' @export
eyeprocess_reliability_evidence_table <- function(x, digits = 4L) {
  if (inherits(x, "eye_process_reliability_profile")) {
    tab <- data.frame(measure = x$measure, icc_a1 = .ep09m2_or(x$icc$icc_a1, NA_real_), stringsAsFactors = FALSE)
    if (!is.null(x$temporal)) tab$temporal_pairs <- nrow(x$temporal)
  } else tab <- .ep09m2_as_df(x, "x")
  .ep09m2_round_numeric(tab, digits)
}

#' Build a paper-ready negative-control table
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param digits Number of decimal digits used for presentation.
#' @export
eyeprocess_negative_control_evidence_table <- function(x, digits = 4L) {
  tab <- if (inherits(x, "eye_process_negative_controls")) summarise_process_negative_controls(x) else .ep09m2_as_df(x, "x")
  .ep09m2_round_numeric(tab, digits)
}

#' Build a paper-ready IRT information/precision table
#' @param items Item-parameter data frame or item collection.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param digits Number of decimal digits used for presentation.
#' @export
eyeprocess_irt_precision_evidence_table <- function(items, theta = seq(-3, 3, by = 0.5), digits = 4L) {
  tab <- eyeprocess_irt_test_information(theta, items)
  .ep09m2_round_numeric(as.data.frame(tab), digits)
}

#' Build an external-engine capability and availability table
#' @export
eyeprocess_irt_engine_evidence_table <- function() {
  x <- as.data.frame(eyeprocess_irt_engine_registry())
  x$policy <- ifelse(x$available, "exact engine available", "gated; no substitute estimator")
  x
}

#' Create an index over frozen validation evidence artifacts
#' @param root Root directory for evidence indexing.
#' @param recursive Whether evidence files are indexed recursively.
#' @export
eyeprocess_validation_evidence_index <- function(root, recursive = TRUE) {
  root <- normalizePath(root, mustWork = TRUE)
  paths <- list.files(root, recursive = recursive, full.names = TRUE, all.files = FALSE)
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  rel <- substring(normalizePath(paths, winslash = "/", mustWork = TRUE), nchar(normalizePath(root, winslash = "/")) + 2L)
  ext <- tolower(tools::file_ext(rel))
  role <- ifelse(grepl("figure|plot", rel, ignore.case = TRUE), "figure",
                 ifelse(grepl("table|summary|result", rel, ignore.case = TRUE), "table",
                        ifelse(grepl("manifest|hash|provenance", rel, ignore.case = TRUE), "provenance", "artifact")))
  data.frame(path = rel, extension = ext, role = role,
             bytes = unname(file.info(paths)$size), hash = unname(tools::md5sum(paths)), stringsAsFactors = FALSE)
}

#' Assemble a validation evidence atlas
#'
#' The atlas is an organizational object. It preserves links between claims,
#' software-validation results, figures, tables, and provenance; it does not
#' upgrade software-validation evidence into construct-validity evidence.
#' @param claims Claim-evidence mapping table.
#' @param recovery Parameter-recovery evidence object or table.
#' @param sbc Simulation-based-calibration evidence object or table.
#' @param stress Measurement-stress evidence object or table.
#' @param reliability Reliability or repeatability evidence object or table.
#' @param negative_controls Negative-control evidence object or table.
#' @param irt IRT-specific evidence object or table.
#' @param provenance Provenance metadata or provenance object.
#' @param artifacts Artifact table or file-index information.
#' @export
eyeprocess_validation_evidence_atlas <- function(claims, recovery = NULL, sbc = NULL, stress = NULL,
                                                 reliability = NULL, negative_controls = NULL,
                                                 irt = NULL, provenance = NULL, artifacts = NULL) {
  claims <- .ep09m2_as_df(claims, "claims")
  .ep09m2_req_cols(claims, c("claim_id", "claim", "status"), "claims")
  components <- list(recovery = recovery, sbc = sbc, stress = stress, reliability = reliability,
                     negative_controls = negative_controls, irt = irt, provenance = provenance, artifacts = artifacts)
  present <- !vapply(components, is.null, logical(1))
  structure(list(
    claims = claims,
    components = components,
    component_status = data.frame(component = names(components), present = unname(present), stringsAsFactors = FALSE),
    coverage = mean(present),
    hash = .ep09m2_hash(list(claims = claims, components = components)),
    guardrail = "The atlas indexes software-validation evidence and does not establish substantive construct validity."
  ), class = "eye_validation_evidence_atlas")
}

#' Summarise gaps in a validation evidence atlas
#' @param atlas Validation-evidence atlas object.
#' @export
eyeprocess_validation_atlas_gaps <- function(atlas) {
  if (!inherits(atlas, "eye_validation_evidence_atlas")) stop("atlas must be created by eyeprocess_validation_evidence_atlas().", call. = FALSE)
  missing_components <- atlas$component_status$component[!atlas$component_status$present]
  unresolved_claims <- atlas$claims[!atlas$claims$status %in% c("supported", "qualified", "demonstrated"), , drop = FALSE]
  list(missing_components = missing_components, unresolved_claims = unresolved_claims,
       complete = !length(missing_components) && !nrow(unresolved_claims))
}

#' Freeze a validation atlas with a reproducibility fingerprint
#' @param atlas Validation-evidence atlas object.
#' @param metadata Named metadata to store with the frozen object.
#' @export
freeze_eyeprocess_validation_atlas <- function(atlas, metadata = list()) {
  if (!inherits(atlas, "eye_validation_evidence_atlas")) stop("atlas must be an eye_validation_evidence_atlas.", call. = FALSE)
  version <- tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_)
  payload <- list(atlas = atlas, metadata = metadata, version = version)
  structure(list(payload = payload, hash = .ep09m2_hash(payload), frozen = TRUE), class = "eye_validation_atlas_freeze")
}

#' Verify a frozen validation atlas
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
verify_eyeprocess_validation_atlas <- function(x) {
  if (!inherits(x, "eye_validation_atlas_freeze")) stop("x must be an eye_validation_atlas_freeze.", call. = FALSE)
  identical(x$hash, .ep09m2_hash(x$payload))
}

#' Write a compact Markdown validation report
#' @param atlas Validation-evidence atlas object.
#' @param path File path for reading or writing.
#' @param title Report title.
#' @export
write_eyeprocess_validation_report <- function(atlas, path, title = "eyeprocess validation evidence report") {
  if (!inherits(atlas, "eye_validation_evidence_atlas")) stop("atlas must be an eye_validation_evidence_atlas.", call. = FALSE)
  gaps <- eyeprocess_validation_atlas_gaps(atlas)
  lines <- c(
    paste0("# ", title), "",
    paste0("Atlas hash: `", atlas$hash, "`"), "",
    "## Evidence components", "",
    paste0("- ", atlas$component_status$component, ": ", ifelse(atlas$component_status$present, "present", "missing")), "",
    "## Claim status", "",
    paste0("- `", atlas$claims$claim_id, "` - ", atlas$claims$status, ": ", atlas$claims$claim), "",
    "## Interpretation guardrail", "",
    atlas$guardrail, "",
    paste0("Missing components: ", if (length(gaps$missing_components)) paste(gaps$missing_components, collapse = ", ") else "none")
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' @export
print.eye_validation_evidence_atlas <- function(x, ...) {
  cat("eyeprocess validation evidence atlas\n")
  cat("  claims     :", nrow(x$claims), "\n")
  cat("  components :", sum(x$component_status$present), "/", nrow(x$component_status), "\n")
  cat("  hash       :", x$hash, "\n")
  invisible(x)
}

#' @export
print.eye_validation_atlas_freeze <- function(x, ...) {
  cat("eyeprocess frozen validation atlas\n")
  cat("  hash    :", x$hash, "\n")
  cat("  verified:", verify_eyeprocess_validation_atlas(x), "\n")
  invisible(x)
}
