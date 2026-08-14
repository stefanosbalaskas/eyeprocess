.multimodal_draw_matrix <- function(x) {
    if (is.vector(x) && is.numeric(x)) return(matrix(x, nrow=1L))
    if (is.data.frame(x)) x <- as.matrix(x)
    if (!is.matrix(x) || !is.numeric(x))
        stop("Posterior draws must be a numeric vector, matrix, or data.frame.", call.=FALSE)
    x
}

#' Quantify incremental process information
#'
#' Compares posterior draws for the same latent target under a baseline and an
#' augmented model. This implementation intentionally uses posterior uncertainty
#' metrics rather than assuming Fisher information is additively decomposable
#' across heterogeneous channels.
#'
#' @param baseline,augmented Numeric posterior draws. Rows are draws and columns
#'   are matched latent targets.
#' @param metric `variance_reduction`, `precision_gain`, or `entropy_reduction`.
#' @return An `eye_process_information` data.frame.
#' @export
process_information <- function(
    baseline,
    augmented,
    metric = c("variance_reduction","precision_gain","entropy_reduction")
) {
    metric <- match.arg(metric)
    b <- .multimodal_draw_matrix(baseline)
    a <- .multimodal_draw_matrix(augmented)
    if (ncol(b) != ncol(a)) stop("Baseline and augmented draws must target the same columns.", call.=FALSE)

    vb <- apply(b, 2L, stats::var)
    va <- apply(a, 2L, stats::var)
    if (any(!is.finite(vb)) || any(!is.finite(va)) || any(vb <= 0) || any(va <= 0))
        stop("Posterior variances must be finite and positive.", call.=FALSE)

    value <- switch(metric,
        variance_reduction = vb - va,
        precision_gain = 1/va - 1/vb,
        entropy_reduction = 0.5 * log(vb/va)
    )
    rel <- 1 - va/vb
    target <- colnames(b)
    if (is.null(target)) target <- paste0("target_", seq_along(value))

    out <- data.frame(
        target=target, metric=metric, value=as.numeric(value),
        baseline_variance=vb, augmented_variance=va,
        relative_variance_reduction=rel,
        stringsAsFactors=FALSE
    )
    class(out) <- c("eye_process_information","data.frame")
    attr(out, "interpretation") <-
        "Positive values indicate reduced posterior uncertainty under the augmented model; this is not a causal claim."
    out
}

#' @export
print.eye_process_information <- function(x, ...) {
    cat("<eye_process_information>\n")
    print.data.frame(x, row.names=FALSE)
    invisible(x)
}

#' Create formal channel-ablation datasets
#'
#' @param x An `eye_multimodal_measurement`.
#' @param include Character vector of channels eligible for ablation.
#' @param include_response Whether response must remain in every scenario.
#' @return An `eye_multimodal_ablation` list.
#' @export
ablate_multimodal_channels <- function(
    x,
    include = c("response","rt","gaze","pupil"),
    include_response = TRUE
) {
    stopifnot(inherits(x, "eye_multimodal_measurement"))
    available <- intersect(include, names(x$channels))
    if (include_response && !"response" %in% available)
        stop("Response channel is required for response-anchored ablation.", call.=FALSE)

    varying <- setdiff(available, if (include_response) "response" else character())
    masks <- unlist(lapply(0:length(varying), function(k) {
        if (k == 0L) list(character()) else combn(varying, k, simplify=FALSE)
    }), recursive=FALSE)

    scenarios <- lapply(masks, function(extra) {
        keep <- c(if (include_response) "response" else character(), extra)
        y <- x
        y$channels <- x$channels[keep]
        y$availability <- x$availability[keep]
        y$ablation_keep <- keep
        y
    })
    names(scenarios) <- vapply(scenarios, function(y)
        paste(names(y$channels), collapse="+"), character(1))

    out <- list(
        scenarios=scenarios,
        available_channels=available,
        principle="Each scenario retains the same rows/keys; only channel inclusion changes."
    )
    class(out) <- c("eye_multimodal_ablation","list")
    out
}

#' @export
print.eye_multimodal_ablation <- function(x, ...) {
    cat("<eye_multimodal_ablation>\n")
    cat(" scenarios:", length(x$scenarios), "\n")
    cat(paste0("  - ", names(x$scenarios)), sep="\n")
    cat("\n")
    invisible(x)
}
