# eyeprocess 0.7.0.9000 ------------------------------------------------------
# Diagnostic plots for API-completion objects. Base graphics only.

.ep07_status_score <- function(x) {
  map <- c(LOSSLESS = 5, SEMANTICALLY_EQUIVALENT = 4,
           UNIT_TRANSFORMED = 4, COORDINATE_TRANSFORMED = 4,
           DERIVED = 3, INTENTIONALLY_DROPPED = 2,
           UNSUPPORTED = 1, AMBIGUOUS = 0,
           REGRESSION_OR_AMBIGUOUS = 0)
  unname(map[as.character(x)])
}

#' Plot eye gaze informed missingness irt
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye gaze informed missingness irt.
#' @export
plot.eye_gaze_informed_missingness_irt <- function(x, ...) {
  d <- x$data
  if (is.null(d) || !nrow(d)) {
    graphics::plot.new(); graphics::title(main = "Gaze-informed missingness")
    return(invisible(x))
  }
  # Aggregate empirical missingness over exposure quantiles. This avoids
  # pretending the reference two-part model supplies a full IRT missingness IRF.
  br <- unique(stats::quantile(d$.log_exposure, probs = seq(0, 1, .1), na.rm = TRUE,
                               names = FALSE, type = 8))
  if (length(br) < 3L) br <- pretty(d$.log_exposure, n = 5)
  g <- cut(d$.log_exposure, breaks = br, include.lowest = TRUE)
  m <- tapply(d$.missing, g, mean, na.rm = TRUE)
  xc <- tapply(d$.log_exposure, g, mean, na.rm = TRUE)
  graphics::plot(xc, m, type = "b", pch = 19, ylim = c(0, 1),
                 xlab = "log(1 + gaze exposure)", ylab = "Observed missingness rate",
                 main = "Gaze-informed missingness diagnostic", ...)
  invisible(data.frame(log_exposure = as.numeric(xc), missing_rate = as.numeric(m)))
}

#' Plot eye process facet effects
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye process facet effects.
#' @export
plot.eye_process_facet_effects <- function(x, ...) {
  re <- x$random_effects
  if (is.null(re) || !nrow(re)) {
    graphics::plot.new(); graphics::title(main = paste("Facet effects:", x$facet))
    return(invisible(x))
  }
  v <- as.numeric(re[[1L]])
  labs <- rownames(re) %||% seq_along(v)
  ord <- order(v)
  graphics::dotchart(v[ord], labels = labs[ord], xlab = "Random intercept",
                     main = paste(x$facet, "facet effects -", x$channel), ...)
  graphics::abline(v = 0, lty = 3)
  invisible(x)
}

#' Plot eye latent distribution comparison
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye latent distribution comparison.
#' @export
plot.eye_latent_distribution_comparison <- function(x, ...) {
  tab <- x$comparison
  if (is.null(tab) || !nrow(tab)) {
    graphics::plot.new(); graphics::title(main = "Latent-distribution comparison")
    return(invisible(x))
  }
  graphics::barplot(tab$delta_BIC, names.arg = tab$model, las = 2,
                    ylab = expression(Delta*BIC), main = "Latent-distribution stress test", ...)
  invisible(tab)
}

#' Plot eye event time irt
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye event time irt.
#' @export
plot.eye_event_time_irt <- function(x, ...) {
  if (!inherits(x, "eye_event_time_irt")) stop("Invalid event-time IRT object.", call. = FALSE)
  if (!identical(x$engine, "cox_reference")) {
    graphics::plot.new(); graphics::title(main = "Event-time IRT")
    graphics::text(.5, .5, "Use the external engine's native plot method")
    return(invisible(x))
  }
  sf <- survival::survfit(x$model)
  graphics::plot(sf, xlab = "Event time", ylab = "Survival probability",
                 main = "Event-time process reference model", ...)
  invisible(sf)
}

#' Plot eye vendor semantic validation
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye vendor semantic validation.
#' @export
plot.eye_vendor_semantic_validation <- function(x, ...) {
  z <- x$fields
  if (is.null(z) || !nrow(z)) {
    graphics::plot.new(); graphics::title(main = paste("Vendor semantics:", x$vendor))
    return(invisible(x))
  }
  score <- ifelse(z$present, 1, 0)
  graphics::barplot(score, names.arg = z$field, las = 2, ylim = c(0, 1.1),
                    ylab = "Field present", main = paste("Vendor semantic contract:", x$vendor), ...)
  invisible(z)
}

#' Plot eye event roundtrip audit
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye event roundtrip audit.
#' @export
plot.eye_event_roundtrip_audit <- function(x, ...) {
  core <- x$event_semantics
  vals <- c(label_fidelity = core$exact_label_fraction,
            count_fidelity = if (max(core$source_n, core$roundtrip_n) == 0) 1 else
              core$matched_n / max(core$source_n, core$roundtrip_n))
  if (!is.null(x$hed)) vals <- c(vals, HED_source = x$hed$valid_fraction_source,
                                 HED_roundtrip = x$hed$valid_fraction_roundtrip)
  graphics::barplot(vals, ylim = c(0, 1), ylab = "Fraction",
                    main = paste("Event round trip:", x$status), ...)
  invisible(vals)
}

#' Plot eye bids roundtrip
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye bids roundtrip.
#' @export
plot.eye_bids_roundtrip <- function(x, ...) {
  if (!inherits(x$audit, "eye_semantic_roundtrip")) stop("BIDS roundtrip does not contain a semantic audit.", call. = FALSE)
  plot(x$audit, ...)
}

#' Plot eye adapter regression audit
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot eye adapter regression audit.
#' @export
plot.eye_adapter_regression_audit <- function(x, ...) {
  f <- x$fidelity$fields
  if (is.null(f) || !nrow(f)) {
    graphics::plot.new(); graphics::title(main = "Adapter regression audit")
    return(invisible(x))
  }
  score <- .ep07_status_score(f$status)
  graphics::dotchart(score, labels = f$field, xlim = c(0, 5),
                     xlab = "Semantic fidelity score (diagnostic ordering)",
                     main = paste(x$baseline_version, "->", x$candidate_version), ...)
  invisible(f)
}

#' Print eye vendor schema contract
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the input object after printing its summary; the object's class and contents are unchanged.
#' @export
print.eye_vendor_schema_contract <- function(x, ...) {
  cat("<eye_vendor_schema_contract>", x$vendor)
  if (!is.na(x$version) && nzchar(x$version)) cat(" version", x$version)
  cat("\n")
  cat(" required:", if (length(x$required_fields)) paste(x$required_fields, collapse = ", ") else "<none>", "\n")
  cat(" optional:", if (length(x$optional_fields)) paste(x$optional_fields, collapse = ", ") else "<none>", "\n")
  invisible(x)
}
