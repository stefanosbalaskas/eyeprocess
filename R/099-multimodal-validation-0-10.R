#' Validate a multimodal IRT development object
#'
#' @param x Measurement, fit, simulation, or process-information object.
#' @return An `eye_multimodal_validation`.
#' @export
validate_multimodal_irt <- function(x) {
    checks <- data.frame(check=character(), pass=logical(), detail=character(),
                         stringsAsFactors=FALSE)
    add <- function(name, pass, detail="") {
        checks <<- rbind(checks, data.frame(check=name, pass=isTRUE(pass),
                                            detail=as.character(detail),
                                            stringsAsFactors=FALSE))
    }

    if (inherits(x, "eye_multimodal_measurement")) {
        a <- audit_multimodal_measurement(x)
        add("measurement_valid", a$valid, paste(a$issues, collapse=","))
        add("unique_keys", a$key_unique)
        add("has_channels", length(x$channels) > 0L)
    } else if (inherits(x, "eye_multimodal_simulation")) {
        add("measurement_class", inherits(x$measurement,"eye_multimodal_measurement"))
        add("truth_present", is.list(x$truth) && length(x$truth)>0L)
        add("finite_response", all(x$data$response %in% c(0L,1L)))
        add("positive_rt", all(x$data$rt > 0, na.rm=TRUE))
        add("nonnegative_gaze", all(x$data$gaze_fixation_count >= 0, na.rm=TRUE))
    } else if (inherits(x, "eye_multimodal_irt_fit")) {
        add("fit_backend", identical(x$backend,"cmdstanr"), x$backend)
        diag <- tryCatch(x$fit$diagnostic_summary(), error=function(e) NULL)
        if (!is.null(diag)) {
            add("no_divergences", diag$num_divergent == 0L,
                paste("divergent=",diag$num_divergent))
            add("no_max_treedepth", diag$num_max_treedepth == 0L,
                paste("max_treedepth=",diag$num_max_treedepth))
        }
        add("not_promoted_without_evidence",
            identical(x$validation_status,"experimental_unvalidated"),
            x$validation_status)
    } else if (inherits(x, "eye_process_information")) {
        add("finite_information", all(is.finite(x$value)))
        add("matched_targets", nrow(x) > 0L)
    } else {
        stop("Unsupported object class for multimodal validation.", call.=FALSE)
    }

    out <- list(valid=all(checks$pass), checks=checks,
                scope="Software/estimator development validation only; not construct validity.")
    class(out) <- c("eye_multimodal_validation","list")
    out
}

#' @export
print.eye_multimodal_validation <- function(x, ...) {
    cat("<eye_multimodal_validation>\n")
    cat(" valid:", x$valid, "\n")
    print(x$checks, row.names=FALSE)
    invisible(x)
}

#' Audit basic multimodal design identifiability
#'
#' This is a structural pre-flight screen, not a proof of statistical
#' identifiability.
#'
#' @param x An `eye_multimodal_measurement`.
#' @param min_person,min_item Minimum structural counts.
#' @return An `eye_multimodal_identifiability_audit`.
#' @export
audit_multimodal_identifiability <- function(x, min_person=30L, min_item=5L) {
    stopifnot(inherits(x,"eye_multimodal_measurement"))
    p <- length(unique(x$data[[x$keys$person]]))
    i <- length(unique(x$data[[x$keys$item]]))
    miss <- vapply(x$channels, function(col) mean(is.na(x$data[[col]])), numeric(1))
    issues <- character()
    if (p < min_person) issues <- c(issues,"few_persons")
    if (i < min_item) issues <- c(issues,"few_items")
    if (any(miss >= .5)) issues <- c(issues,"channel_missingness_ge_50_percent")
    out <- list(
        supported=length(issues)==0L, persons=p, items=i,
        channel_missingness=miss, issues=issues,
        caveat="Passing this structural screen does not establish model identifiability."
    )
    class(out) <- c("eye_multimodal_identifiability_audit","list")
    out
}
