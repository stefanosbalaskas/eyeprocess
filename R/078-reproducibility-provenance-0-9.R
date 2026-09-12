# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Reproducibility fingerprints, lineage graphs, PROV-oriented and RO-Crate metadata.

#' Hash an R object reproducibly within an R serialization version
#' @param x R object.
#' @return An R object containing hash an R object reproducibly within an R serialization version. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
object_hash <- function(x) .ep09_hash_object(x)

#' Build a file hash manifest
#' @param paths File paths.
#' @param algorithm Hash algorithm; currently `md5` uses base R, `sha256` uses openssl when available.
#' @return A data frame containing a file hash manifest. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
file_hash_manifest <- function(paths, algorithm = c("md5", "sha256")) {
  algorithm <- match.arg(algorithm)
  if (!length(paths)) return(data.frame(path = character(), algorithm = character(), hash = character(), size_bytes = numeric(), modified = character(), stringsAsFactors = FALSE))
  paths <- normalizePath(paths, winslash = "/", mustWork = TRUE)
  hashes <- if (algorithm == "md5") unname(tools::md5sum(paths)) else {
    if (!requireNamespace("openssl", quietly = TRUE)) stop("sha256 requires the optional openssl package.", call. = FALSE)
    vapply(paths, function(p) {
      con <- file(p, open = "rb")
      tryCatch(paste0(as.character(openssl::sha256(con)), collapse = ""), finally = close(con))
    }, character(1))
  }
  info <- file.info(paths)
  data.frame(path = paths, algorithm = algorithm, hash = hashes, size_bytes = unname(info$size),
             modified = format(info$mtime, tz = "UTC", usetz = TRUE), stringsAsFactors = FALSE)
}

#' Snapshot an eyeprocess analysis environment
#' @param packages Optional package names; defaults to loaded namespaces.
#' @return A named list with components "r_version", "platform", "os", "locale", "timezone", "packages", containing snapshot an eyeprocess analysis environment and associated metadata or diagnostics.
#' @export
analysis_environment_snapshot <- function(packages = loadedNamespaces()) {
  packages <- sort(unique(as.character(packages)))
  packages <- packages[!is.na(packages) & nzchar(packages)]
  vers <- vapply(packages, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), character(1))
  list(
    r_version = R.version.string,
    platform = R.version$platform,
    os = Sys.info()[c("sysname", "release", "version", "machine")],
    locale = Sys.getlocale(),
    timezone = Sys.timezone(),
    packages = data.frame(package = packages, version = unname(vers), stringsAsFactors = FALSE)
  )
}

#' Create a session-level provenance manifest
#' @param data Optional data object.
#' @param files Optional input file paths.
#' @param adapter Adapter/importer identifier.
#' @param decisions Optional decision manifest.
#' @param pipeline Optional pipeline or run object.
#' @param seeds Optional named random seeds.
#' @param notes Optional notes.
#' @return A named list with components "created_utc", "eyeprocess_version", "data_hash", "files", "adapter", "decisions_hash", "pipeline_hash", "seeds", "environment", "notes", containing a session-level provenance manifest and associated metadata or diagnostics.
#' @export
eye_session_manifest <- function(data = NULL, files = NULL, adapter = NA_character_, decisions = NULL,
                                 pipeline = NULL, seeds = NULL, notes = NULL) {
  list(
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    eyeprocess_version = tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_),
    data_hash = if (is.null(data)) NA_character_ else object_hash(data),
    files = if (is.null(files)) data.frame() else file_hash_manifest(files),
    adapter = as.character(adapter)[1L],
    decisions_hash = if (is.null(decisions)) NA_character_ else object_hash(decisions),
    pipeline_hash = if (is.null(pipeline)) NA_character_ else object_hash(pipeline),
    seeds = seeds,
    environment = analysis_environment_snapshot(),
    notes = notes
  )
}

#' Construct a reproducibility fingerprint
#' @param data Input data or hashable object.
#' @param analysis_spec Analysis specification.
#' @param model_spec Model specification.
#' @param decisions Decision manifest.
#' @param result Optional result object.
#' @param files Optional input file paths.
#' @param seeds Optional seeds.
#' @param label Fingerprint label.
#' @return A named list with components "schema_version", "label", "eyeprocess_version", "data_hash", "analysis_spec_hash", "model_spec_hash", "decisions_hash", "result_hash", "file_manifest", "seeds", "environment", containing a reproducibility fingerprint and associated metadata or diagnostics.
#' @export
eye_reproducibility_fingerprint <- function(data = NULL, analysis_spec = NULL, model_spec = NULL,
                                            decisions = NULL, result = NULL, files = NULL,
                                            seeds = NULL, label = "eyeprocess_analysis") {
  label <- as.character(label)[1L]
  if (is.na(label) || !nzchar(label)) stop("label must be a non-empty scalar.", call. = FALSE)
  core <- list(
    schema_version = "eyeprocess-reproducibility-0.9",
    label = label,
    eyeprocess_version = tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_),
    data_hash = if (is.null(data)) NA_character_ else object_hash(data),
    analysis_spec_hash = if (is.null(analysis_spec)) NA_character_ else object_hash(analysis_spec),
    model_spec_hash = if (is.null(model_spec)) NA_character_ else object_hash(model_spec),
    decisions_hash = if (is.null(decisions)) NA_character_ else object_hash(decisions),
    result_hash = if (is.null(result)) NA_character_ else object_hash(result),
    file_manifest = if (is.null(files)) data.frame() else file_hash_manifest(files),
    seeds = seeds,
    environment = analysis_environment_snapshot()
  )
  core$fingerprint_hash <- object_hash(core)
  class(core) <- "eye_reproducibility_fingerprint"
  core
}

#' Compare two reproducibility fingerprints
#' @param old,new Fingerprints.
#' @return A named list with components "detail", "identical", "old_hash", "new_hash", containing two reproducibility fingerprints and associated metadata or diagnostics.
#' @export
compare_reproducibility_fingerprints <- function(old, new) {
  fields <- c("eyeprocess_version", "data_hash", "analysis_spec_hash", "model_spec_hash", "decisions_hash", "result_hash")
  rows <- lapply(fields, function(f) data.frame(field = f, old = as.character(old[[f]]), new = as.character(new[[f]]),
                                                identical = identical(old[[f]], new[[f]]), stringsAsFactors = FALSE))
  out <- list(detail = do.call(rbind, rows), identical = identical(old$fingerprint_hash, new$fingerprint_hash),
              old_hash = old$fingerprint_hash, new_hash = new$fingerprint_hash)
  class(out) <- "eye_reproducibility_comparison"
  out
}

#' Verify an internally stored fingerprint hash
#' @param x Fingerprint.
#' @return A logical value or vector indicating verify an internally stored fingerprint hash.
#' @export
verify_reproducibility_fingerprint <- function(x) {
  if (!inherits(x, "eye_reproducibility_fingerprint")) stop("x must be an eye_reproducibility_fingerprint.", call. = FALSE)
  y <- x; stored <- y$fingerprint_hash; y$fingerprint_hash <- NULL; class(y) <- NULL
  identical(stored, object_hash(y))
}

#' Write a reproducibility fingerprint
#' @param x Fingerprint.
#' @param path Output path.
#' @param format `rds`, `dput`, or `json`.
#' @return A character string or vector giving the path or identifier for a reproducibility fingerprint.
#' @export
write_reproducibility_fingerprint <- function(x, path, format = c("rds", "dput", "json")) {
  format <- match.arg(format)
  if (format == "rds") saveRDS(x, path, version = 3) else if (format == "dput") dput(x, file = path) else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("JSON output requires jsonlite.", call. = FALSE)
    jsonlite::write_json(unclass(x), path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Read a reproducibility fingerprint
#' @param path Input path.
#' @param format Optional format.
#' @return A logical value or vector indicating a reproducibility fingerprint.
#' @export
read_reproducibility_fingerprint <- function(path, format = NULL) {
  if (is.null(format)) format <- tolower(tools::file_ext(path))
  x <- if (format == "rds") readRDS(path) else if (format %in% c("dput", "r")) dget(path) else {
    if (!requireNamespace("jsonlite", quietly = TRUE)) stop("JSON input requires jsonlite.", call. = FALSE)
    jsonlite::read_json(path, simplifyVector = TRUE)
  }
  if (!inherits(x, "eye_reproducibility_fingerprint")) class(x) <- c("eye_reproducibility_fingerprint", class(x))
  if (!verify_reproducibility_fingerprint(x)) warning("Stored fingerprint hash does not match the imported object representation.", call. = FALSE)
  x
}

#' Build a provenance lineage node table
#' @param id Node identifiers.
#' @param type Node types such as entity, activity, agent.
#' @param label Human-readable labels.
#' @param value Optional values/locations.
#' @return A data frame containing a provenance lineage node table. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
provenance_lineage_table <- function(id, type = "entity", label = id, value = NA_character_) {
  n <- max(length(id), length(type), length(label), length(value)); r <- function(x) rep(x, length.out = n)
  if (!is.finite(n) || n < 1L) stop("At least one provenance node is required.", call. = FALSE)
  out <- data.frame(id = as.character(r(id)), type = as.character(r(type)), label = as.character(r(label)),
                    value = as.character(r(value)), stringsAsFactors = FALSE)
  if (any(is.na(out$id) | !nzchar(out$id))) stop("provenance node ids cannot be missing or empty.", call. = FALSE)
  if (any(is.na(out$type) | !nzchar(out$type))) stop("provenance node types cannot be missing or empty.", call. = FALSE)
  if (anyDuplicated(out$id)) stop("provenance node ids must be unique.", call. = FALSE)
  out
}

#' Build a provenance edge table
#' @param from,to Node identifiers.
#' @param relation PROV-like relation labels.
#' @return A data frame containing a provenance edge table. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
provenance_edge_table <- function(from, to, relation = "wasDerivedFrom") {
  n <- max(length(from), length(to), length(relation)); r <- function(x) rep(x, length.out = n)
  if (!is.finite(n) || n < 1L) return(data.frame(from = character(), to = character(), relation = character(), stringsAsFactors = FALSE))
  out <- data.frame(from = as.character(r(from)), to = as.character(r(to)), relation = as.character(r(relation)), stringsAsFactors = FALSE)
  if (any(is.na(out$from) | !nzchar(out$from) | is.na(out$to) | !nzchar(out$to))) stop("provenance edge endpoints cannot be missing or empty.", call. = FALSE)
  if (any(is.na(out$relation) | !nzchar(out$relation))) stop("provenance relations cannot be missing or empty.", call. = FALSE)
  out
}

#' Construct a lightweight provenance graph
#' @param nodes Node table.
#' @param edges Edge table.
#' @param metadata Optional metadata.
#' @return A named list with components "nodes", "edges", "metadata", containing a lightweight provenance graph and associated metadata or diagnostics.
#' @export
eye_prov_graph <- function(nodes, edges = data.frame(from = character(), to = character(), relation = character()), metadata = list()) {
  nodes <- .ep09_as_df(nodes); edges <- .ep09_as_df(edges)
  .ep09_req_cols(nodes, c("id", "type", "label"), "nodes")
  .ep09_req_cols(edges, c("from", "to", "relation"), "edges")
  x <- list(nodes = nodes, edges = edges, metadata = metadata)
  class(x) <- "eye_prov_graph"; validate_eye_prov_graph(x); x
}

#' Validate a provenance graph
#' @param x Provenance graph.
#' @return A logical value or vector indicating a provenance graph.
#' @export
validate_eye_prov_graph <- function(x) {
  if (!inherits(x, "eye_prov_graph")) stop("x must be an eye_prov_graph.", call. = FALSE)
  ids <- as.character(x$nodes$id)
  if (any(is.na(ids) | !nzchar(ids))) stop("provenance node ids cannot be missing or empty.", call. = FALSE)
  if (anyDuplicated(ids)) stop("duplicate provenance node ids.", call. = FALSE)
  bad <- setdiff(unique(c(x$edges$from, x$edges$to)), ids)
  if (length(bad)) stop("edges reference unknown node(s): ", paste(bad, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

#' Export lightweight PROV-oriented JSON
#'
#' This is a compact interoperability representation inspired by W3C PROV. It is
#' not asserted to be a complete PROV-O serialization; consumers requiring full
#' conformance should validate/transform it externally.
#' @param x Provenance graph.
#' @param path Output JSON path.
#' @return An R object containing lightweight PROV-oriented JSON. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
export_prov_json <- function(x, path) {
  validate_eye_prov_graph(x)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("JSON output requires jsonlite.", call. = FALSE)
  payload <- list(schema = "eyeprocess-prov-0.9", nodes = x$nodes, edges = x$edges, metadata = x$metadata)
  jsonlite::write_json(payload, path, dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' Export minimal RO-Crate 1.3 metadata
#'
#' @param path Output `ro-crate-metadata.json` path.
#' @param name Crate/dataset name.
#' @param description Description.
#' @param files Optional files to include as File entities.
#' @param creator Optional creator name.
#' @param license Optional license URL or identifier.
#' @param doi Optional DOI for the software/data product.
#' @return An R object containing minimal RO-Crate 1.3 metadata. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
export_ro_crate_metadata <- function(path = "ro-crate-metadata.json", name = "eyeprocess analysis",
                                     description = "Reproducible eyeprocess analysis crate", files = NULL,
                                     creator = NULL, license = NULL, doi = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("RO-Crate metadata export requires jsonlite.", call. = FALSE)
  graph <- list(
    list(`@id` = "ro-crate-metadata.json", `@type` = "CreativeWork", about = list(`@id` = "./"),
         conformsTo = list(`@id` = "https://w3id.org/ro/crate/1.3")),
    Filter(Negate(is.null), list(`@id` = "./", `@type` = "Dataset", name = name, description = description,
                                 datePublished = format(Sys.Date(), "%Y-%m-%d"),
                                 license = if (!is.null(license)) license else NULL,
                                 identifier = if (!is.null(doi)) doi else NULL,
                                 creator = if (!is.null(creator)) list(`@id` = "#creator") else NULL))
  )
  if (!is.null(creator)) graph[[length(graph) + 1L]] <- list(`@id` = "#creator", `@type` = "Person", name = creator)
  if (!is.null(files) && length(files)) {
    pp <- normalizePath(files, winslash = "/", mustWork = TRUE)
    base <- basename(pp)
    if (anyDuplicated(base)) stop("RO-Crate file basenames must be unique in this minimal exporter.", call. = FALSE)
    graph[[2L]]$hasPart <- lapply(base, function(z) list(`@id` = z))
    for (i in seq_along(pp)) graph[[length(graph) + 1L]] <- list(`@id` = base[[i]], `@type` = "File",
                                                                 contentSize = unname(file.info(pp[[i]])$size))
  }
  payload <- list(`@context` = "https://w3id.org/ro/crate/1.3/context", `@graph` = graph)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' Return Graphviz DOT for a provenance graph
#' @param x Provenance graph.
#' @return A character value or vector containing return Graphviz DOT for a provenance graph.
#' @export
write_prov_dot <- function(x) {
  validate_eye_prov_graph(x)
  esc <- function(z) gsub('"', '\\"', as.character(z), fixed = TRUE)
  nodes <- sprintf('  "%s" [label="%s\\n(%s)"];', esc(x$nodes$id), esc(x$nodes$label), esc(x$nodes$type))
  edges <- if (nrow(x$edges)) sprintf('  "%s" -> "%s" [label="%s"];', esc(x$edges$from), esc(x$edges$to), esc(x$edges$relation)) else character()
  paste(c("digraph eyeprocess_provenance {", nodes, edges, "}"), collapse = "\n")
}

#' @export
print.eye_reproducibility_fingerprint <- function(x, ...) {
  cat("<eye_reproducibility_fingerprint>\n", " label: ", x$label, "\n hash: ", x$fingerprint_hash,
      "\n eyeprocess: ", x$eyeprocess_version, "\n", sep = "")
  invisible(x)
}

#' @export
print.eye_reproducibility_comparison <- function(x, ...) {
  cat("<eye_reproducibility_comparison>\n", " identical: ", x$identical, "\n", sep = "")
  print(x$detail, row.names = FALSE); invisible(x)
}

#' @export
print.eye_prov_graph <- function(x, ...) {
  cat("<eye_prov_graph>\n", " nodes: ", nrow(x$nodes), "\n edges: ", nrow(x$edges), "\n", sep = "")
  invisible(x)
}
