# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Evidence bundles and traceable claim matrices for software/methods papers.

#' Construct a software-paper evidence bundle
#' @param claims Claim table or list.
#' @param validation Validation evidence table/list.
#' @param examples Optional example inventory.
#' @param articles Optional article inventory.
#' @param benchmarks Optional benchmark evidence.
#' @param reproducibility Optional reproducibility fingerprint.
#' @param metadata Optional metadata.
#' @export
software_paper_evidence_bundle <- function(claims = NULL, validation = NULL, examples = NULL, articles = NULL,
                                           benchmarks = NULL, reproducibility = NULL, metadata = list()) {
  x <- list(schema_version = "eyeprocess-paper-evidence-0.9", claims = claims, validation = validation,
            examples = examples, articles = articles, benchmarks = benchmarks, reproducibility = reproducibility,
            metadata = metadata, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  x$bundle_hash <- object_hash(x); class(x) <- "eye_software_paper_evidence"; x
}

#' Create or normalize a software-paper claim matrix
#' @param claim Claim text.
#' @param evidence_id Evidence identifiers.
#' @param evidence_type Evidence type.
#' @param status Status such as supported, qualified, pending, or unsupported.
#' @param scope Explicit scope/qualification.
#' @param source Optional source/location.
#' @export
software_paper_claim_matrix <- function(claim, evidence_id = NA_character_, evidence_type = NA_character_,
                                        status = "pending", scope = NA_character_, source = NA_character_) {
  if (!length(claim) || any(is.na(claim)) || any(!nzchar(as.character(claim)))) stop("claim must contain non-empty claim text.", call. = FALSE)
  n <- max(length(claim), length(evidence_id), length(evidence_type), length(status), length(scope), length(source)); r <- function(x) rep(x, length.out = n)
  status0 <- as.character(r(status))
  allowed <- c("supported", "qualified", "pending", "unsupported")
  if (any(is.na(status0) | !status0 %in% allowed)) stop("status must be one of supported, qualified, pending, or unsupported.", call. = FALSE)
  data.frame(claim_id = sprintf("CL%03d", seq_len(n)), claim = as.character(r(claim)), evidence_id = as.character(r(evidence_id)),
             evidence_type = as.character(r(evidence_type)), status = status0, scope = as.character(r(scope)),
             source = as.character(r(source)), stringsAsFactors = FALSE)
}

#' Summarise validation evidence for a software paper
#' @param x Validation result/evidence matrix/table.
#' @export
software_paper_validation_table <- function(x) {
  if (inherits(x, "eye_process_validation_result")) return(summarise_process_validation(x))
  if (is.list(x) && !is.data.frame(x)) {
    if (is.null(names(x)) || any(!nzchar(names(x)))) stop("List validation evidence must be named.", call. = FALSE)
    return(do.call(validation_evidence_matrix, x))
  }
  .ep09_as_df(x)
}

#' Compute descriptive evidence coverage
#' @param x Evidence bundle or claim matrix.
#' @param supported Status labels counted as covered.
#' @export
software_paper_coverage <- function(x, supported = c("supported", "qualified")) {
  claims <- if (inherits(x, "eye_software_paper_evidence")) x$claims else x
  claims <- .ep09_as_df(claims); .ep09_req_cols(claims, "status", "claims")
  ok <- claims$status %in% supported
  coverage <- if (nrow(claims) && any(!is.na(ok))) mean(ok[!is.na(ok)]) else NA_real_
  data.frame(n_claims = nrow(claims), n_covered = sum(ok, na.rm = TRUE), coverage = coverage,
             n_pending = sum(claims$status == "pending", na.rm = TRUE), n_unsupported = sum(claims$status == "unsupported", na.rm = TRUE))
}

#' Descriptive software-paper readiness audit
#'
#' Readiness is defined only against caller-specified requirements; it is not a
#' journal acceptance prediction.
#' @param x Evidence bundle.
#' @param required_statuses Statuses allowed for claims.
#' @param require_validation Require non-empty validation evidence.
#' @param require_reproducibility Require a reproducibility fingerprint.
#' @param require_examples Require examples.
#' @param require_articles Require articles.
#' @export
software_paper_readiness <- function(x, required_statuses = c("supported", "qualified"), require_validation = TRUE,
                                     require_reproducibility = TRUE, require_examples = TRUE, require_articles = TRUE) {
  if (!inherits(x, "eye_software_paper_evidence")) stop("x must be an eye_software_paper_evidence object.", call. = FALSE)
  claims <- tryCatch(.ep09_as_df(x$claims), error = function(e) data.frame())
  required_statuses <- as.character(required_statuses)
  required_statuses <- required_statuses[!is.na(required_statuses) & nzchar(required_statuses)]
  if (!length(required_statuses)) stop("required_statuses cannot be empty.", call. = FALSE)
  claim_ok <- nrow(claims) > 0L && "status" %in% names(claims) && !anyNA(claims$status) && all(claims$status %in% required_statuses)
  checks <- data.frame(
    requirement = c("claims", "validation", "reproducibility", "examples", "articles"),
    required = c(TRUE, require_validation, require_reproducibility, require_examples, require_articles),
    satisfied = c(claim_ok,
                  !is.null(x$validation) && length(x$validation) > 0L,
                  inherits(x$reproducibility, "eye_reproducibility_fingerprint"),
                  !is.null(x$examples) && length(x$examples) > 0L,
                  !is.null(x$articles) && length(x$articles) > 0L),
    stringsAsFactors = FALSE
  )
  out <- list(ready = all(checks$satisfied[checks$required]), checks = checks,
              interpretation = "Readiness is a completeness audit against explicitly declared evidence requirements, not a prediction of peer-review outcome.")
  class(out) <- "eye_software_paper_readiness"; out
}

#' Identify gaps in a software-paper evidence bundle
#' @param x Evidence bundle.
#' @export
software_paper_gap_analysis <- function(x) {
  r <- software_paper_readiness(x)
  gaps <- r$checks[r$checks$required & !r$checks$satisfied, , drop = FALSE]
  claims <- tryCatch(.ep09_as_df(x$claims), error = function(e) data.frame())
  claim_gaps <- if (nrow(claims) && "status" %in% names(claims)) claims[!claims$status %in% c("supported", "qualified"), , drop = FALSE] else data.frame()
  list(requirement_gaps = gaps, claim_gaps = claim_gaps)
}

#' Freeze software-paper evidence to an RDS with a hash
#' @param x Evidence bundle.
#' @param path Output path.
#' @export
freeze_software_paper_evidence <- function(x, path) {
  if (!inherits(x, "eye_software_paper_evidence")) stop("x must be an eye_software_paper_evidence object.", call. = FALSE)
  saveRDS(x, path, version = 3)
  data.frame(path = normalizePath(path, winslash = "/", mustWork = FALSE), hash = unname(tools::md5sum(path)), stringsAsFactors = FALSE)
}

#' Write a human-readable software-paper evidence report
#' @param x Evidence bundle.
#' @param path Markdown output path.
#' @export
write_software_paper_evidence <- function(x, path) {
  if (!inherits(x, "eye_software_paper_evidence")) stop("x must be an eye_software_paper_evidence object.", call. = FALSE)
  rd <- software_paper_readiness(x); cov <- tryCatch(software_paper_coverage(x), error = function(e) NULL)
  lines <- c("# eyeprocess software-paper evidence bundle", "", paste0("Bundle hash: `", x$bundle_hash, "`"), "",
             paste0("Descriptive readiness: **", if (rd$ready) "PASS" else "INCOMPLETE", "**"), "", "## Requirement audit", "")
  lines <- c(lines, paste0("- ", rd$checks$requirement, ": required=", rd$checks$required, ", satisfied=", rd$checks$satisfied))
  if (!is.null(cov)) lines <- c(lines, "", "## Claim coverage", "", paste0("- Claims: ", cov$n_claims), paste0("- Covered: ", cov$n_covered),
                                paste0("- Coverage: ", format(round(cov$coverage, 4), nsmall = 4)))
  lines <- c(lines, "", "> This report audits supplied evidence; it does not establish external validity or predict journal acceptance.")
  writeLines(lines, path, useBytes = TRUE); invisible(path)
}

#' Create a compact paper reproducibility manifest
#' @param evidence Evidence bundle.
#' @param manuscript Optional manuscript path.
#' @param figures Optional figure paths.
#' @param tables Optional table paths.
#' @export
paper_reproducibility_manifest <- function(evidence, manuscript = NULL, figures = NULL, tables = NULL) {
  if (!inherits(evidence, "eye_software_paper_evidence")) stop("evidence must be an eye_software_paper_evidence object.", call. = FALSE)
  paths <- c(manuscript, figures, tables); paths <- paths[!is.na(paths) & nzchar(paths)]
  list(evidence_hash = evidence$bundle_hash,
       manuscript = manuscript,
       files = if (!length(paths)) data.frame() else if (all(file.exists(paths))) file_hash_manifest(paths) else stop("All supplied paper files must exist.", call. = FALSE),
       reproducibility = evidence$reproducibility,
       generated_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
}

#' @export
print.eye_software_paper_evidence <- function(x, ...) {
  cat("<eye_software_paper_evidence>\n", " hash: ", x$bundle_hash, "\n", sep = "")
  cov <- tryCatch(software_paper_coverage(x), error = function(e) NULL)
  if (!is.null(cov)) cat(" claims: ", cov$n_claims, " | covered: ", cov$n_covered, "\n", sep = "")
  invisible(x)
}

#' @export
print.eye_software_paper_readiness <- function(x, ...) {
  cat("<eye_software_paper_readiness>\n", " ready: ", x$ready, "\n", sep = "")
  print(x$checks, row.names = FALSE); invisible(x)
}
