# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Streaming/partial scoring and unified validation evidence bundles.

#' Score a partial response pattern from a calibrated mirt model
#'
#' @param model Calibrated `mirt` model.
#' @param response_pattern Full-length response vector with future/unobserved items as NA.
#' @param method mirt scoring method, typically MAP or EAP.
#' @param ... Passed to `mirt::fscores()`.
#' @return A data frame containing a partial response pattern from a calibrated mirt model. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
score_partial_response_pattern <- function(model, response_pattern,
                                           method = c("MAP", "EAP"), ...) {
  method <- match.arg(method)
  if (!requireNamespace("mirt", quietly = TRUE)) stop("Package `mirt` is required.", call. = FALSE)
  rp <- as.numeric(response_pattern)
  out <- mirt::fscores(model, method = method, response.pattern = rp, ...)
  as.data.frame(out)
}

#' Score a response stream cumulatively
#'
#' @param model Calibrated `mirt` model.
#' @param response_pattern Complete or partial response vector in item order.
#' @param observed_order Order in which observed items arrive. Defaults to sequence.
#' @param method MAP or EAP.
#' @param ... Passed to `mirt::fscores()`.
#' @return An `eye_streaming_score` object.
#' @export
score_response_stream <- function(model, response_pattern, observed_order = NULL,
                                  method = c("MAP", "EAP"), ...) {
  method <- match.arg(method)
  rp <- as.numeric(response_pattern)
  n <- length(rp)
  if (!n) stop("response_pattern must contain at least one item position.", call. = FALSE)
  if (is.null(observed_order)) observed_order <- which(!is.na(rp))
  observed_order <- as.integer(observed_order)
  if (!length(observed_order)) stop("No observed responses were supplied for streaming scoring.", call. = FALSE)
  if (any(!observed_order %in% seq_len(n)) || anyDuplicated(observed_order))
    stop("observed_order must contain unique valid item positions.", call. = FALSE)
  rows <- vector("list", length(observed_order))
  current <- rep(NA_real_, n)
  for (k in seq_along(observed_order)) {
    pos <- observed_order[k]
    current[pos] <- rp[pos]
    fs <- tryCatch(score_partial_response_pattern(model, current, method = method, ...),
                   error = function(e) NULL)
    if (is.null(fs) || !nrow(fs)) {
      rows[[k]] <- data.frame(step = k, item_position = pos, latest_response = rp[pos],
                              theta = NA_real_, theta_se = NA_real_, stringsAsFactors = FALSE)
    } else {
      theta_col <- if ("F1" %in% names(fs)) "F1" else names(fs)[1L]
      se_cols <- grep("^SE", names(fs), value = TRUE)
      rows[[k]] <- data.frame(
        step = k, item_position = pos, latest_response = rp[pos],
        theta = .ep08_num(fs[[theta_col]][1L]),
        theta_se = if (length(se_cols)) .ep08_num(fs[[se_cols[1L]]][1L]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  tab <- do.call(rbind, rows)
  structure(list(
    history = tab, response_pattern = rp, observed_order = observed_order,
    method = method, model = model,
    status = "streaming_scoring_simulation",
    caveat = paste(
      "Streaming scores are demonstrations/operational building blocks.",
      "High-stakes use requires calibrated banks, latency testing, privacy review, stopping rules, and score-governance validation."
    )
  ), class = "eye_streaming_score")
}

#' Update a partial person score with one new response
#'
#' @param model Calibrated model.
#' @param current_pattern Existing full-length partial pattern.
#' @param item_position Item receiving the new response.
#' @param response New response.
#' @param method Scoring method.
#' @return A named list with components "pattern", "score", containing update a partial person score with one new response and associated metadata or diagnostics.
#' @export
#' @param ... Additional arguments passed to the underlying method or helper.
update_person_score <- function(model, current_pattern, item_position, response,
                                method = c("MAP", "EAP"), ...) {
  method <- match.arg(method)
  rp <- as.numeric(current_pattern)
  item_position <- as.integer(item_position)
  if (length(item_position) != 1L || item_position < 1L || item_position > length(rp))
    stop("item_position is out of range.", call. = FALSE)
  response <- as.numeric(response)[1L]
  if (is.na(response)) stop("response must be non-missing when updating a person score.", call. = FALSE)
  rp[item_position] <- response
  list(pattern = rp, score = score_partial_response_pattern(model, rp, method = method, ...))
}

#' Extract streaming score history
#' @return An R object containing streaming score history. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
streaming_score_history <- function(x) {
  if (!inherits(x, "eye_streaming_score")) stop("x must be eye_streaming_score.", call. = FALSE)
  x$history
}

.ep08_validation_slot_names <- c(
  "model_spec", "evidence_grade", "recovery", "bias", "rmse", "coverage",
  "interval_width", "convergence", "identifiability", "mcse", "sbc", "ppc",
  "stress_tests", "external_validation", "transportability", "process_ablation",
  "incremental_information", "negative_controls", "semantic_validation",
  "preflight", "drift", "software_provenance", "session_provenance"
)

#' Collect validation evidence into a common bundle
#'
#' @param ... Named validation objects.
#' @param model_name Optional model/workflow label.
#' @param notes Optional free-text notes.
#' @return An `eye_validation_bundle` object.
#' @export
collect_validation_evidence <- function(..., model_name = NULL, notes = NULL) {
  xs <- list(...)
  if (length(xs) && is.null(names(xs))) stop("Validation evidence supplied through ... must be named.", call. = FALSE)
  if (length(xs) && any(!nzchar(names(xs)))) stop("All validation evidence entries must have names.", call. = FALSE)
  unknown <- setdiff(names(xs), .ep08_validation_slot_names)
  if (length(unknown)) warning("Non-standard validation evidence slot(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  structure(list(
    model_name = if (is.null(model_name)) "unnamed_model" else as.character(model_name)[1L],
    evidence = xs,
    notes = notes,
    created_at = as.character(Sys.time()),
    session = utils::sessionInfo(),
    status = "validation_evidence_bundle",
    caveat = "A validation bundle organizes evidence; the presence of an object does not itself establish adequacy."
  ), class = "eye_validation_bundle")
}

.ep08_evidence_status <- function(x) {
  if (is.null(x)) return("missing")
  if (is.data.frame(x) && nrow(x) == 0L) return("empty")
  if (inherits(x, "error")) return("error")
  "available"
}

#' Create a machine-readable validation manifest
#' @return A data frame containing a machine-readable validation manifest. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param x Object to process, inspect, compare, or plot.
validation_bundle_manifest <- function(x) {
  if (!inherits(x, "eye_validation_bundle")) stop("x must be eye_validation_bundle.", call. = FALSE)
  slots <- unique(c(.ep08_validation_slot_names, names(x$evidence)))
  data.frame(
    slot = slots,
    status = vapply(slots, function(s) .ep08_evidence_status(x$evidence[[s]]), character(1)),
    class = vapply(slots, function(s) {
      z <- x$evidence[[s]]
      if (is.null(z)) NA_character_ else paste(class(z), collapse = ";")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

.ep08_find_scalar <- function(x, names_to_try) {
  if (is.null(x)) return(NA_real_)
  if (is.data.frame(x)) {
    hit <- names_to_try[names_to_try %in% names(x)]
    if (length(hit) && nrow(x)) return(.ep08_num(x[[hit[1L]]][1L]))
  }
  if (is.list(x)) {
    hit <- names_to_try[names_to_try %in% names(x)]
    if (length(hit)) {
      z <- x[[hit[1L]]]
      if (length(z)) return(.ep08_num(z[1L]))
    }
  }
  NA_real_
}

#' Render a conservative validation report
#'
#' @param x Validation bundle.
#' @param include_session Include abbreviated session provenance.
#' @return Character vector of report lines.
#' @export
validation_report <- function(x, include_session = TRUE) {
  if (!inherits(x, "eye_validation_bundle")) stop("x must be eye_validation_bundle.", call. = FALSE)
  man <- validation_bundle_manifest(x)
  avail <- man$slot[man$status == "available"]
  missing <- man$slot[man$status == "missing"]
  lines <- c(
    paste0("eyeprocess validation report: ", x$model_name),
    paste(rep("=", 30 + nchar(x$model_name)), collapse = ""),
    "",
    paste0("Created: ", x$created_at),
    "",
    "Evidence inventory",
    "------------------",
    paste0("Available: ", if (length(avail)) paste(avail, collapse = ", ") else "none"),
    paste0("Missing/not supplied: ", if (length(missing)) paste(missing, collapse = ", ") else "none"),
    ""
  )

  rec <- x$evidence$recovery
  if (!is.null(rec)) lines <- c(lines, "Parameter recovery evidence was supplied.")
  cov <- x$evidence$coverage
  if (!is.null(cov)) lines <- c(lines, "Interval-coverage evidence was supplied.")
  conv <- x$evidence$convergence
  if (!is.null(conv)) lines <- c(lines, "Convergence evidence was supplied.")
  ext <- x$evidence$external_validation
  trans <- x$evidence$transportability
  if (!is.null(ext) || !is.null(trans)) lines <- c(lines, "External/transportability evidence was supplied.")
  abl <- x$evidence$process_ablation
  if (!is.null(abl)) lines <- c(lines, "Process-channel ablation evidence was supplied.")
  neg <- x$evidence$negative_controls
  if (!is.null(neg)) lines <- c(lines, "Negative-control evidence was supplied.")
  pf <- x$evidence$preflight
  if (!is.null(pf)) lines <- c(lines, "Biometric pre-flight evidence was supplied.")
  dr <- x$evidence$drift
  if (!is.null(dr)) lines <- c(lines, "Deployment-drift evidence was supplied.")

  lines <- c(lines, "", "Interpretation guardrails", "-------------------------",
             "- Convergence is not validation.",
             "- Predictive improvement is not causal evidence.",
             "- Gaze/pupil/process channels require sensitivity to preprocessing, missingness, and data quality.",
             "- External validity and transportability should be evaluated before generalization.",
             "- Screening, anomaly, accessibility, and profile outputs are review tools, not clinical or misconduct labels.")
  if (!is.null(x$notes)) lines <- c(lines, "", "Notes", "-----", as.character(x$notes))
  if (isTRUE(include_session)) {
    lines <- c(lines, "", "Software provenance", "-------------------",
               paste0("R version: ", x$session$R.version$version.string),
               paste0("Platform: ", x$session$platform))
  }
  lines
}

#' Write a validation report to disk
#' @param x Validation bundle.
#' @param path Output text-file path.
#' @param ... Passed to `validation_report()`.
#' @return A character string or vector giving the path or identifier for a validation report to disk.
#' @export
write_validation_report <- function(x, path, ...) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(validation_report(x, ...), con = path, useBytes = TRUE)
  path
}

.ep08_write_df <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
  path
}

#' Export a validation evidence bundle
#'
#' @param x Validation bundle.
#' @param directory Output directory.
#' @param overwrite Allow writing into a non-empty target directory.
#' @param include_rds Save full R objects as RDS.
#' @return An object of class "eye_validation_export", stored as a named list, with components "directory", "files", "manifest". It contains a validation evidence bundle and associated metadata or diagnostics needed to interpret the result.
#' @export
export_validation_bundle <- function(x, directory, overwrite = FALSE, include_rds = TRUE) {
  if (!inherits(x, "eye_validation_bundle")) stop("x must be eye_validation_bundle.", call. = FALSE)
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  if (dir.exists(directory) && length(list.files(directory, all.files = FALSE)) && !isTRUE(overwrite))
    stop("Target directory is not empty; set overwrite=TRUE to continue.", call. = FALSE)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  paths <- character()
  manifest <- validation_bundle_manifest(x)
  paths <- c(paths, manifest = .ep08_write_df(manifest, file.path(directory, "validation_bundle_manifest.csv")))
  paths <- c(paths, report = write_validation_report(x, file.path(directory, "validation_report.txt")))

  for (nm in names(x$evidence)) {
    obj <- x$evidence[[nm]]
    if (is.data.frame(obj)) {
      paths <- c(paths, setNames(.ep08_write_df(obj, file.path(directory, paste0(nm, ".csv"))), nm))
    } else if (isTRUE(include_rds) && !is.null(obj)) {
      p <- file.path(directory, paste0(nm, ".rds")); saveRDS(obj, p); paths <- c(paths, setNames(p, nm))
    }
  }
  if (isTRUE(include_rds)) {
    p <- file.path(directory, "validation_bundle.rds"); saveRDS(x, p); paths <- c(paths, bundle = p)
  }
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(
      list(model_name = x$model_name, created_at = x$created_at,
           manifest = manifest, files = as.list(paths)),
      path = file.path(directory, "validation_bundle_manifest.json"),
      pretty = TRUE, auto_unbox = TRUE, null = "null"
    )
    paths <- c(paths, json_manifest = file.path(directory, "validation_bundle_manifest.json"))
  }
  structure(list(directory = directory, files = paths, manifest = manifest),
            class = "eye_validation_export")
}

#' Print a validation bundle object
#' @return Invisibly returns the input object after printing its summary; the object's class and contents are unchanged.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
print.eye_validation_bundle <- function(x, ...) {
  cat("<eye_validation_bundle>", x$model_name, "\n")
  m <- validation_bundle_manifest(x)
  cat(" evidence available:", sum(m$status == "available"), "/", nrow(m), "\n")
  invisible(x)
}

#' Summarize a validation bundle object
#' @return A named list with components "model_name", "manifest", "report", containing a validation bundle object and associated metadata or diagnostics.
#' @export
#' @param object Object supplied to the S3 method.
#' @param ... Additional arguments passed to the underlying method or helper.
summary.eye_validation_bundle <- function(object, ...) {
  list(model_name = object$model_name,
       manifest = validation_bundle_manifest(object),
       report = validation_report(object, include_session = FALSE))
}
