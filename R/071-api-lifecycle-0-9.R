# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Public API inventory, lifecycle metadata, canonical interfaces, and audits.

.ep09_api_family <- function(x) {
  ifelse(grepl("^plot[._]", x), "plot",
  ifelse(grepl("^(audit|validate|validation)_", x), "validation",
  ifelse(grepl("^(fit|predict|score|update)_", x), "model",
  ifelse(grepl("^(read|write|export|import)_", x), "io",
  ifelse(grepl("^(process|eye)_.*(spec|manifest|registry|design|pipeline)", x), "workflow",
  ifelse(grepl("^(compare|summari[sz]e|summary|extract)_", x), "summary",
  ifelse(grepl("^(simulate|inject|stress|benchmark)_", x), "simulation",
  "utility")))))))
}

#' Inventory the public eyeprocess API
#'
#' @param package Package name or namespace environment.
#' @param lifecycle Lifecycle registry. Defaults to the packaged 0.9 registry from `eye_api_lifecycle()`.
#' @return Data frame of exported symbols and lifecycle metadata.
#' @export
eye_api_inventory <- function(package = "eyeprocess", lifecycle = eye_api_lifecycle()) {
  ns <- if (is.environment(package)) package else {
    if (!requireNamespace(package, quietly = TRUE)) stop("Package `", package, "` is not installed/loaded.", call. = FALSE)
    asNamespace(package)
  }
  exports <- sort(getNamespaceExports(ns))
  kind <- vapply(exports, function(nm) {
    obj <- tryCatch(get(nm, envir = ns, inherits = FALSE), error = function(e) NULL)
    if (is.function(obj)) "function" else class(obj)[1L]
  }, character(1))
  out <- data.frame(
    name = exports,
    kind = kind,
    family = .ep09_api_family(exports),
    stringsAsFactors = FALSE
  )
  if (!is.null(lifecycle)) {
    reg <- eye_api_lifecycle(lifecycle)
    out <- merge(out, reg, by = "name", all.x = TRUE, sort = FALSE)
  }
  if (!"status" %in% names(out)) out$status <- "unreviewed"
  out$status[is.na(out$status)] <- "unreviewed"
  if (!"canonical" %in% names(out)) out$canonical <- NA_character_
  if (!"replacement" %in% names(out)) out$replacement <- NA_character_
  out
}

#' Normalize or create an API lifecycle registry
#'
#' @param registry Optional data frame with at least `name` and `status`. `NULL` loads the packaged 0.9 lifecycle registry.
#' @return `eye_api_lifecycle` data frame.
#' @export
eye_api_lifecycle <- function(registry = NULL) {
  allowed <- c("core", "workflow", "advanced", "experimental", "gated", "compatibility", "deprecated", "internal-candidate", "unreviewed")
  if (is.null(registry)) {
    registry_path <- system.file("extdata", "api-lifecycle-registry-0.9.csv", package = "eyeprocess")
    if (!nzchar(registry_path)) {
      source_candidate <- file.path("inst", "extdata", "api-lifecycle-registry-0.9.csv")
      if (file.exists(source_candidate)) registry_path <- source_candidate
    }
    if (nzchar(registry_path) && file.exists(registry_path)) {
      registry <- utils::read.csv(registry_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    } else {
      registry <- data.frame(name = character(), status = character(), canonical = character(),
                             replacement = character(), since = character(), notes = character(), stringsAsFactors = FALSE)
    }
  }
  registry <- .ep09_as_df(registry)
  .ep09_req_cols(registry, c("name", "status"), "registry")
  for (nm in c("canonical", "replacement", "since", "notes")) if (!nm %in% names(registry)) registry[[nm]] <- NA_character_
  registry$name <- as.character(registry$name)
  registry$status <- as.character(registry$status)
  if (anyNA(registry$name) || any(!nzchar(registry$name))) stop("Lifecycle registry API names must be non-missing and non-empty.", call. = FALSE)
  if (anyNA(registry$status) || any(!nzchar(registry$status))) stop("Lifecycle registry statuses must be non-missing and non-empty.", call. = FALSE)
  if (anyDuplicated(registry$name)) stop("Lifecycle registry contains duplicate API names.", call. = FALSE)
  bad <- setdiff(unique(registry$status), allowed)
  if (length(bad)) stop("Unknown lifecycle status: ", paste(bad, collapse = ", "), call. = FALSE)
  class(registry) <- c("eye_api_lifecycle", "data.frame")
  registry
}

#' Add or update API lifecycle metadata without global mutation
#' @param registry Existing lifecycle registry.
#' @param name API name.
#' @param status Lifecycle status.
#' @param canonical Canonical API for the same concept, if applicable.
#' @param replacement Replacement for deprecated/superseded API.
#' @param since Version in which status applies.
#' @param notes Notes.
#' @export
register_eye_api_status <- function(registry = eye_api_lifecycle(), name, status,
                                    canonical = NA_character_, replacement = NA_character_,
                                    since = "0.9.0.9000", notes = NA_character_) {
  reg <- eye_api_lifecycle(registry)
  row <- eye_api_lifecycle(data.frame(
    name = as.character(name)[1L], status = as.character(status)[1L],
    canonical = as.character(canonical)[1L], replacement = as.character(replacement)[1L],
    since = as.character(since)[1L], notes = as.character(notes)[1L], stringsAsFactors = FALSE
  ))
  reg <- reg[reg$name != row$name, , drop = FALSE]
  eye_api_lifecycle(rbind(reg, row))
}

#' Lookup lifecycle status for one or more APIs
#' @param name API names.
#' @param registry Lifecycle registry.
#' @export
eye_api_status <- function(name, registry = eye_api_lifecycle()) {
  reg <- eye_api_lifecycle(registry)
  name <- as.character(name)
  idx <- match(name, reg$name)
  data.frame(
    name = name,
    status = ifelse(is.na(idx), "unreviewed", reg$status[idx]),
    canonical = ifelse(is.na(idx), NA_character_, reg$canonical[idx]),
    replacement = ifelse(is.na(idx), NA_character_, reg$replacement[idx]),
    stringsAsFactors = FALSE
  )
}

#' Return superseded/deprecated compatibility interfaces
#' @param registry Lifecycle registry.
#' @export
eye_api_superseded <- function(registry = eye_api_lifecycle()) {
  reg <- eye_api_lifecycle(registry)
  reg[reg$status %in% c("deprecated", "compatibility") | (!is.na(reg$replacement) & nzchar(reg$replacement)), , drop = FALSE]
}

#' Canonical API mapping
#' @param registry Lifecycle registry.
#' @export
canonical_eye_api <- function(registry = eye_api_lifecycle()) {
  reg <- eye_api_lifecycle(registry)
  keep <- (!is.na(reg$canonical) & nzchar(reg$canonical)) | reg$status %in% c("core", "workflow")
  reg[keep, , drop = FALSE]
}

#' Summarise API surface by family and lifecycle status
#' @param inventory Output of `eye_api_inventory()` or compatible table.
#' @export
api_surface_summary <- function(inventory) {
  inventory <- .ep09_as_df(inventory)
  .ep09_req_cols(inventory, c("name", "family", "status"), "inventory")
  as.data.frame(with(inventory, table(family, status)), stringsAsFactors = FALSE)
}

#' Map exported APIs to conceptual families
#' @param inventory API inventory or character names.
#' @export
api_family_map <- function(inventory) {
  if (is.character(inventory)) {
    return(data.frame(name = inventory, family = .ep09_api_family(inventory), stringsAsFactors = FALSE))
  }
  inventory <- .ep09_as_df(inventory)
  .ep09_req_cols(inventory, "name", "inventory")
  data.frame(name = inventory$name,
             family = if ("family" %in% names(inventory)) inventory$family else .ep09_api_family(inventory$name),
             stringsAsFactors = FALSE)
}

#' Audit API lifecycle completeness and replacement contracts
#' @param inventory API inventory.
#' @param registry Lifecycle registry.
#' @export
audit_eye_api <- function(inventory = eye_api_inventory(), registry = eye_api_lifecycle()) {
  inv <- .ep09_as_df(inventory); reg <- eye_api_lifecycle(registry)
  .ep09_req_cols(inv, "name", "inventory")
  status <- eye_api_status(inv$name, reg)
  tab <- merge(inv, status, by = "name", all.x = TRUE, suffixes = c("", "_registry"), sort = FALSE)
  if ("status_registry" %in% names(tab)) tab$status <- tab$status_registry
  if ("canonical_registry" %in% names(tab)) tab$canonical <- tab$canonical_registry
  if ("replacement_registry" %in% names(tab)) tab$replacement <- tab$replacement_registry
  exported <- inv$name
  invalid_replacement <- !is.na(tab$replacement) & nzchar(tab$replacement) & !tab$replacement %in% exported
  invalid_canonical <- !is.na(tab$canonical) & nzchar(tab$canonical) & !tab$canonical %in% exported
  tab$replacement_exists <- !invalid_replacement
  tab$canonical_exists <- !invalid_canonical
  unreviewed <- tab$name[tab$status == "unreviewed"]
  structure(list(
    table = tab,
    unreviewed = unreviewed,
    invalid_replacements = tab$name[invalid_replacement],
    invalid_canonical = tab$name[invalid_canonical],
    reviewed_fraction = if (nrow(tab)) mean(tab$status != "unreviewed") else NA_real_,
    valid = !any(invalid_replacement | invalid_canonical)
  ), class = "eye_api_audit")
}

#' Lifecycle recommendation for API review
#' @param audit `eye_api_audit` object.
#' @export
eye_api_recommendation <- function(audit) {
  if (!inherits(audit, "eye_api_audit")) stop("audit must be an eye_api_audit.", call. = FALSE)
  tab <- audit$table
  data.frame(
    name = tab$name,
    recommendation = ifelse(
      tab$status == "unreviewed", "classify",
      ifelse(!tab$replacement_exists, "repair_replacement",
      ifelse(!tab$canonical_exists, "repair_canonical", "retain"))
    ),
    stringsAsFactors = FALSE
  )
}

#' Write API lifecycle registry to CSV
#' @param registry Lifecycle registry.
#' @param path Output path.
#' @export
write_api_lifecycle_registry <- function(registry, path) {
  reg <- eye_api_lifecycle(registry)
  utils::write.csv(reg, path, row.names = FALSE, na = "")
  invisible(path)
}

#' Read API lifecycle registry from CSV
#' @param path CSV path.
#' @export
read_api_lifecycle_registry <- function(path) {
  eye_api_lifecycle(utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA")))
}

#' Compare two API lifecycle registries
#' @param old Old registry.
#' @param new New registry.
#' @export
api_lifecycle_diff <- function(old, new) {
  old <- eye_api_lifecycle(old); new <- eye_api_lifecycle(new)
  keys <- union(old$name, new$name)
  out <- lapply(keys, function(nm) {
    a <- old[old$name == nm, , drop = FALSE]; b <- new[new$name == nm, , drop = FALSE]
    data.frame(
      name = nm,
      old_status = if (nrow(a)) a$status[[1L]] else NA_character_,
      new_status = if (nrow(b)) b$status[[1L]] else NA_character_,
      changed = !identical(if (nrow(a)) a$status[[1L]] else NA_character_, if (nrow(b)) b$status[[1L]] else NA_character_),
      stringsAsFactors = FALSE
    )
  })
  .ep09_rbind_fill(out)
}

#' @export
print.eye_api_audit <- function(x, ...) {
  cat("eyeprocess API lifecycle audit\n")
  cat("  APIs             :", nrow(x$table), "\n")
  cat("  reviewed fraction:", sprintf("%.1f%%", 100 * x$reviewed_fraction), "\n")
  cat("  invalid mappings :", length(x$invalid_replacements) + length(x$invalid_canonical), "\n")
  invisible(x)
}
