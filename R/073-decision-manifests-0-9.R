# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Research-decision manifests, locking, outcome-blind snapshots, and audit.

.ep09_flatten_manifest <- function(x, prefix = "") {
  if (!is.list(x) || is.data.frame(x)) {
    val <- if (length(x) == 0L) NA_character_ else paste(as.character(x), collapse = ";")
    return(data.frame(path = prefix, value = val, stringsAsFactors = FALSE))
  }
  if (!length(x)) return(data.frame(path = prefix, value = NA_character_, stringsAsFactors = FALSE))
  nms <- names(x)
  if (is.null(nms)) nms <- paste0("[[", seq_along(x), "]]" )
  nms[is.na(nms) | !nzchar(nms)] <- paste0("[[", which(is.na(nms) | !nzchar(nms)), "]]" )
  .ep09_rbind_fill(lapply(seq_along(x), function(i) {
    nm <- nms[[i]]
    p <- if (nzchar(prefix)) paste(prefix, nm, sep = ".") else nm
    .ep09_flatten_manifest(x[[i]], p)
  }))
}

#' Create a machine-readable research decision manifest
#'
#' @param sampling Sampling decisions.
#' @param validity Validity/missingness decisions.
#' @param fixation Fixation construction decisions.
#' @param pupil Pupil preprocessing decisions.
#' @param aoi AOI assignment decisions.
#' @param model Statistical/psychometric model decisions.
#' @param sensitivity Sensitivity-analysis decisions.
#' @param exclusions Exclusion rules.
#' @param provenance Optional provenance fields.
#' @param notes Notes.
#' @param ... Additional named decision domains.
#' @return An object of class "eye_decision_manifest", stored as a named list, with components "domains", "notes", "created_at", "schema_version", "status", "caveat". It contains a machine-readable research decision manifest and associated metadata or diagnostics needed to interpret the result.
#' @export
eye_decision_manifest <- function(
    sampling = list(), validity = list(), fixation = list(), pupil = list(),
    aoi = list(), model = list(), sensitivity = list(), exclusions = list(),
    provenance = list(), notes = NULL, ...) {
  extra <- list(...)
  if (length(extra) && (is.null(names(extra)) || anyNA(names(extra)) || any(!nzchar(names(extra)))))
    stop("Additional manifest domains supplied through ... must be named.", call. = FALSE)
  domains <- c(list(
    sampling = sampling, validity = validity, fixation = fixation, pupil = pupil,
    aoi = aoi, model = model, sensitivity = sensitivity, exclusions = exclusions,
    provenance = provenance
  ), extra)
  x <- structure(list(
    domains = domains,
    notes = notes,
    created_at = as.character(Sys.time()),
    schema_version = "0.9.0.9000",
    status = "research_decision_manifest",
    caveat = "The manifest documents decisions; it does not endorse any decision as universally appropriate."
  ), class = "eye_decision_manifest")
  x$hash <- decision_manifest_hash(x)
  x
}

#' Validate a research decision manifest
#' @param x Manifest.
#' @param required_domains Domains that must exist.
#' @param require_nonempty If TRUE, required domains must contain at least one decision.
#' @return A logical value or vector indicating a research decision manifest.
#' @export
validate_decision_manifest <- function(
    x,
    required_domains = c("sampling", "validity", "fixation", "pupil", "aoi", "model", "sensitivity", "exclusions"),
    require_nonempty = FALSE) {
  if (!inherits(x, "eye_decision_manifest")) stop("x must be an eye_decision_manifest.", call. = FALSE)
  miss <- setdiff(required_domains, names(x$domains))
  if (length(miss)) stop("Manifest is missing required domain(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (isTRUE(require_nonempty)) {
    empty <- required_domains[vapply(x$domains[required_domains], length, integer(1)) == 0L]
    if (length(empty)) stop("Required manifest domain(s) are empty: ", paste(empty, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Flatten a decision manifest to a table
#' @param x Manifest.
#' @return A logical value or vector indicating flatten a decision manifest to a table.
#' @export
decision_manifest_table <- function(x) {
  validate_decision_manifest(x, required_domains = character())
  out <- .ep09_flatten_manifest(x$domains)
  out$manifest_hash <- x$hash
  out
}

#' Stable hash of decision content
#' @param x Manifest or decision object.
#' @return An R object containing stable hash of decision content. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
decision_manifest_hash <- function(x) {
  if (inherits(x, "eye_decision_manifest")) {
    payload <- list(domains = x$domains, notes = x$notes, schema_version = x$schema_version)
  } else payload <- x
  .ep09_hash_object(payload)
}

#' Lock a decision manifest by content hash
#' @param x Manifest.
#' @param label Optional lock label.
#' @return An object of class "eye_decision_manifest_lock", stored as a named list, with components "manifest", "manifest_hash", "label", "locked_at", "status". It contains lock a decision manifest by content hash and associated metadata or diagnostics needed to interpret the result.
#' @export
lock_decision_manifest <- function(x, label = "analysis_decisions") {
  validate_decision_manifest(x, required_domains = character())
  structure(list(
    manifest = x,
    manifest_hash = decision_manifest_hash(x),
    label = as.character(label)[1L],
    locked_at = as.character(Sys.time()),
    status = "locked_decision_manifest"
  ), class = "eye_decision_manifest_lock")
}

#' Verify that a locked manifest has not changed
#' @param x Manifest lock.
#' @return A logical value or vector indicating verify that a locked manifest has not changed.
#' @export
verify_decision_manifest_lock <- function(x) {
  if (!inherits(x, "eye_decision_manifest_lock")) stop("x must be an eye_decision_manifest_lock.", call. = FALSE)
  identical(x$manifest_hash, decision_manifest_hash(x$manifest))
}

#' Compare two research decision manifests
#' @param old Earlier manifest.
#' @param new Later manifest.
#' @return An object of class "eye_decision_manifest_diff", "data.frame", stored as a data frame, containing two research decision manifests and associated metadata needed to interpret the result.
#' @export
compare_decision_manifests <- function(old, new) {
  a <- decision_manifest_table(old); b <- decision_manifest_table(new)
  keys <- union(a$path, b$path)
  out <- lapply(keys, function(k) {
    av <- a$value[match(k, a$path)]; bv <- b$value[match(k, b$path)]
    if (!length(av)) av <- NA_character_; if (!length(bv)) bv <- NA_character_
    data.frame(path = k, old = av[[1L]], new = bv[[1L]],
               changed = !identical(av[[1L]], bv[[1L]]), stringsAsFactors = FALSE)
  })
  structure(.ep09_rbind_fill(out), class = c("eye_decision_manifest_diff", "data.frame"))
}

#' Alias for manifest comparison emphasizing changed decision paths
#' @inheritParams compare_decision_manifests
#' @return An R object containing alias for manifest comparison emphasizing changed decision paths. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
decision_manifest_diff <- function(old, new) compare_decision_manifests(old, new)

#' Write a decision manifest
#' @param x Manifest.
#' @param path Output path.
#' @param format `rds`, `dput`, or `json`.
#' @return An R object containing a decision manifest. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
write_decision_manifest <- function(x, path, format = c("rds", "dput", "json")) {
  validate_decision_manifest(x, required_domains = character())
  format <- match.arg(format)
  if (format == "rds") saveRDS(x, path, version = 3)
  if (format == "dput") dput(x, file = path)
  if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package `jsonlite` is required for JSON export.", call. = FALSE)
    jsonlite::write_json(unclass(x), path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  invisible(path)
}

#' Read a decision manifest written by eyeprocess
#' @param path Input path.
#' @param format Optional format; inferred from extension when omitted.
#' @return A logical value or vector indicating a decision manifest written by eyeprocess.
#' @export
read_decision_manifest <- function(path, format = NULL) {
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(path))
    format <- if (ext == "rds") "rds" else if (ext == "json") "json" else "dput"
  }
  format <- match.arg(format, c("rds", "dput", "json"))
  x <- if (format == "rds") readRDS(path) else if (format == "dput") dget(path) else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package `jsonlite` is required for JSON import.", call. = FALSE)
    raw <- jsonlite::read_json(path, simplifyVector = FALSE)
    structure(raw, class = "eye_decision_manifest")
  }
  validate_decision_manifest(x, required_domains = character())
  if (is.null(x$hash) || length(x$hash) != 1L || !identical(as.character(x$hash), decision_manifest_hash(x)))
    stop("Decision manifest hash is missing or does not match the imported decision content.", call. = FALSE)
  x
}

#' Audit decision provenance and completeness
#' @param x Manifest.
#' @param required_domains Required decision domains.
#' @param required_provenance Provenance keys expected under `provenance`.
#' @return An object of class "eye_decision_provenance_audit", stored as a named list, with components "missing_domains", "empty_domains", "missing_provenance", "complete", "manifest_hash". It contains decision provenance and completeness and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_decision_provenance <- function(
    x,
    required_domains = c("sampling", "validity", "fixation", "pupil", "aoi", "model", "sensitivity", "exclusions"),
    required_provenance = c("data_source", "software_version", "analysis_commit")) {
  validate_decision_manifest(x, required_domains = character())
  missing_domains <- setdiff(required_domains, names(x$domains))
  empty_domains <- intersect(required_domains, names(x$domains))[vapply(x$domains[intersect(required_domains, names(x$domains))], length, integer(1)) == 0L]
  prov <- x$domains$provenance
  if (is.null(prov)) prov <- list()
  missing_prov <- required_provenance[!required_provenance %in% names(prov) |
                                      vapply(required_provenance, function(nm) {
                                        z <- prov[[nm]]; is.null(z) || !length(z) || !nzchar(paste(z, collapse = ""))
                                      }, logical(1))]
  structure(list(
    missing_domains = missing_domains,
    empty_domains = empty_domains,
    missing_provenance = missing_prov,
    complete = !length(missing_domains) && !length(empty_domains) && !length(missing_prov),
    manifest_hash = x$hash
  ), class = "eye_decision_provenance_audit")
}

#' Create an outcome-blind data snapshot
#'
#' @param data Data frame.
#' @param outcome Outcome column(s) to remove from the analysis snapshot.
#' @param id Optional identifier columns retained in the snapshot.
#' @return An object of class "eye_outcome_blind_snapshot", stored as a named list, with components "data", "removed_outcomes", "id", "source_columns", "blinded_columns", "blinded_hash", "created_at", "caveat". It contains an outcome-blind data snapshot and associated metadata or diagnostics needed to interpret the result.
#' @export
outcome_blind_snapshot <- function(data, outcome, id = NULL) {
  d <- .ep09_as_df(data)
  outcome <- unique(as.character(outcome)); .ep09_req_cols(d, outcome, "data")
  if (!is.null(id)) .ep09_req_cols(d, unique(as.character(id)), "data")
  blinded <- d[setdiff(names(d), outcome)]
  structure(list(
    data = blinded,
    removed_outcomes = outcome,
    id = id,
    source_columns = names(d),
    blinded_columns = names(blinded),
    blinded_hash = .ep09_hash_object(blinded),
    created_at = as.character(Sys.time()),
    caveat = "Outcome blinding limits direct access through this snapshot only; it cannot guarantee analysts were otherwise unaware of outcomes."
  ), class = "eye_outcome_blind_snapshot")
}

#' Verify an outcome-blind snapshot has not changed
#' @param x Snapshot.
#' @return A logical value or vector indicating verify an outcome-blind snapshot has not changed.
#' @export
verify_outcome_blind_snapshot <- function(x) {
  if (!inherits(x, "eye_outcome_blind_snapshot")) stop("x must be an eye_outcome_blind_snapshot.", call. = FALSE)
  identical(x$blinded_hash, .ep09_hash_object(x$data)) && !any(x$removed_outcomes %in% names(x$data))
}

#' Entropy of an explicitly enumerated analysis-decision space
#'
#' @param ... Named option vectors.
#' @param base Logarithm base.
#' @return Data frame with option counts and maximum entropy under equal weighting.
#' @export
analysis_decision_entropy <- function(..., base = 2) {
  opts <- list(...)
  if (!length(opts) || is.null(names(opts)) || anyNA(names(opts)) || any(!nzchar(names(opts))) || anyDuplicated(names(opts)))
    stop("Supply uniquely named decision option vectors.", call. = FALSE)
  if (length(base) != 1L || !is.finite(base) || base <= 0 || base == 1) stop("base must be finite, positive, and not equal to 1.", call. = FALSE)
  if (any(lengths(opts) == 0L)) stop("Decision option vectors cannot be empty.", call. = FALSE)
  counts <- vapply(opts, function(z) length(unique(z)), integer(1))
  entropy <- ifelse(counts > 0, log(counts, base = base), NA_real_)
  out <- data.frame(decision = names(opts), options = counts, max_entropy = entropy, stringsAsFactors = FALSE)
  attr(out, "joint_specifications") <- prod(counts)
  attr(out, "joint_max_entropy") <- sum(entropy)
  out
}

#' Coverage of a declared decision space by evaluated specifications
#' @param grid Sensitivity grid.
#' @param evaluated Sensitivity result or vector of evaluated specification IDs.
#' @return A data frame containing coverage of a declared decision space by evaluated specifications. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
decision_space_coverage <- function(grid, evaluated) {
  grid <- .ep09_as_df(grid); .ep09_req_cols(grid, "specification_id", "grid")
  ids <- if (inherits(evaluated, "eye_process_sensitivity")) unique(evaluated$results$specification_id) else as.character(evaluated)
  covered <- grid$specification_id %in% ids
  data.frame(
    planned = nrow(grid),
    evaluated = sum(covered),
    coverage = if (length(covered)) mean(covered) else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' @export
print.eye_decision_manifest <- function(x, ...) {
  cat("eyeprocess research decision manifest\n")
  cat("  domains:", length(x$domains), "\n")
  cat("  hash   :", x$hash, "\n")
  invisible(x)
}
