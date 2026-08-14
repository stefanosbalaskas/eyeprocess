#' Prepare a canonical multimodal person-item-trial measurement object
#'
#' Creates a loss-aware person-item-trial table for response, response time,
#' gaze, and pupil channels while retaining references to optional sample-level
#' or sequence payloads. No time series are silently aggregated.
#'
#' @param data A data.frame containing one row per intended person-item-trial.
#' @param person,item,trial Column names identifying the measurement keys.
#' @param response,rt,gaze,pupil Optional column names for channel summaries.
#' @param quality Optional character vector of quality-field names.
#' @param device Optional list or data.frame of device metadata.
#' @param payloads Named list of sample-level/sequence payloads.
#' @param provenance Optional provenance list.
#' @return An `eye_multimodal_measurement` object.
#' @export
prepare_multimodal_irt_data <- function(
    data,
    person,
    item,
    trial = NULL,
    response = NULL,
    rt = NULL,
    gaze = NULL,
    pupil = NULL,
    quality = character(),
    device = NULL,
    payloads = list(),
    provenance = list()
) {
    stopifnot(is.data.frame(data), is.character(person), length(person) == 1L,
              is.character(item), length(item) == 1L)
    needed <- unique(c(person, item, trial, response, rt, gaze, pupil, quality))
    needed <- needed[!is.na(needed) & nzchar(needed)]
    missing <- setdiff(needed, names(data))
    if (length(missing)) {
        stop("Missing measurement columns: ", paste(missing, collapse = ", "), call. = FALSE)
    }

    key_names <- c(person, item, trial)
    key_names <- key_names[!is.na(key_names) & nzchar(key_names)]
    key <- data[key_names]

    duplicated_key <- duplicated(key)
    if (any(duplicated_key)) {
        stop("Person-item-trial keys are not unique; ", sum(duplicated_key),
             " duplicated rows detected.", call. = FALSE)
    }

    channel_map <- list(
        response = response,
        rt = rt,
        gaze = gaze,
        pupil = pupil
    )
    channel_map <- channel_map[vapply(channel_map, function(x)
        !is.null(x) && length(x) == 1L && !is.na(x) && nzchar(x), logical(1))]

    missingness <- lapply(channel_map, function(col) is.na(data[[col]]))
    availability <- vapply(channel_map, function(col) sum(!is.na(data[[col]])), integer(1))

    out <- list(
        data = data,
        keys = list(person = person, item = item, trial = trial),
        channels = channel_map,
        quality = quality,
        device = device,
        payloads = payloads,
        provenance = provenance,
        missingness = missingness,
        availability = availability,
        created_at = Sys.time(),
        interpretation = c(
            "Gaze and pupil are observed process measurements.",
            "No channel is assigned a psychological construct automatically.",
            "Time-series payloads are retained rather than silently aggregated."
        )
    )
    class(out) <- c("eye_multimodal_measurement", "list")
    out
}

#' @export
print.eye_multimodal_measurement <- function(x, ...) {
    cat("<eye_multimodal_measurement>\n")
    cat(" rows     :", nrow(x$data), "\n")
    cat(" channels :", paste(names(x$channels), collapse = ", "), "\n")
    cat(" payloads :", paste(names(x$payloads), collapse = ", "), "\n")
    invisible(x)
}

#' Audit a multimodal measurement object
#'
#' @param x An `eye_multimodal_measurement`.
#' @return An `eye_multimodal_audit`.
#' @export
audit_multimodal_measurement <- function(x) {
    stopifnot(inherits(x, "eye_multimodal_measurement"))
    d <- x$data
    key_cols <- unlist(x$keys, use.names = FALSE)
    key_cols <- key_cols[!is.na(key_cols) & nzchar(key_cols)]
    key_ok <- !anyDuplicated(d[key_cols])

    channel_rows <- lapply(names(x$channels), function(ch) {
        col <- x$channels[[ch]]
        z <- d[[col]]
        data.frame(
            channel = ch,
            column = col,
            n = length(z),
            observed = sum(!is.na(z)),
            missing = sum(is.na(z)),
            missing_fraction = mean(is.na(z)),
            finite_fraction = if (is.numeric(z)) mean(is.finite(z), na.rm = TRUE) else NA_real_,
            stringsAsFactors = FALSE
        )
    })
    channel_table <- if (length(channel_rows)) do.call(rbind, channel_rows) else
        data.frame(channel=character(), column=character(), n=integer(),
                   observed=integer(), missing=integer(), missing_fraction=double(),
                   finite_fraction=double())

    issues <- character()
    if (!key_ok) issues <- c(issues, "duplicated_person_item_trial_keys")
    if (!length(x$channels)) issues <- c(issues, "no_measurement_channels")
    if ("rt" %in% names(x$channels)) {
        rt <- d[[x$channels$rt]]
        if (any(rt <= 0, na.rm = TRUE)) issues <- c(issues, "nonpositive_response_time")
    }
    if ("gaze" %in% names(x$channels)) {
        gz <- d[[x$channels$gaze]]
        if (is.numeric(gz) && any(gz < 0, na.rm = TRUE)) issues <- c(issues, "negative_gaze_measurement")
    }

    out <- list(
        valid = length(issues) == 0L,
        issues = unique(issues),
        key_unique = key_ok,
        channel_table = channel_table,
        payload_names = names(x$payloads),
        quality_fields = x$quality,
        device = x$device
    )
    class(out) <- c("eye_multimodal_audit", "list")
    out
}

#' @export
print.eye_multimodal_audit <- function(x, ...) {
    cat("<eye_multimodal_audit>\n")
    cat(" valid  :", x$valid, "\n")
    cat(" issues :", if (length(x$issues)) paste(x$issues, collapse=", ") else "none", "\n")
    print(x$channel_table, row.names = FALSE)
    invisible(x)
}

#' Consolidated multimodal IRT specification
#'
#' This is a thin multimodal adapter over the established
#' `irt_model_spec()` / `eye_irt_model_spec` architecture. It composes
#' existing `eye_irt_channel` objects and does not define a parallel
#' channel or model-specification ecosystem.
#'
#' @param response,rt,gaze,pupil Existing `eye_irt_channel` objects or NULL.
#' @param model Development model identifier.
#' @param backend Requested backend.
#' @param identification Named identification settings retained as explicit
#'   multimodal metadata.
#' @param priors Named prior settings retained as explicit multimodal metadata.
#' @return An `eye_multimodal_irt_spec` convenience subclass of the
#'   established `eye_irt_model_spec`.
#' @export
multimodal_irt_spec <- function(
    response = NULL,
    rt = NULL,
    gaze = NULL,
    pupil = NULL,
    model = c("M0", "M1", "M2", "M3"),
    backend = c("cmdstanr", "existing"),
    identification = list(),
    priors = list()
) {
    model <- match.arg(model)
    backend <- match.arg(backend)

    channels <- Filter(
        Negate(is.null),
        list(
            response = response,
            rt = rt,
            gaze = gaze,
            pupil = pupil
        )
    )

    if (!length(channels)) {
        stop(
            "At least one existing eyeprocess IRT channel must be supplied.",
            call. = FALSE
        )
    }

    bad <- names(channels)[
        !vapply(
            channels,
            inherits,
            logical(1),
            what = "eye_irt_channel"
        )
    ]

    if (length(bad)) {
        stop(
            "Channels must be existing eyeprocess eye_irt_channel objects: ",
            paste(bad, collapse = ", "),
            call. = FALSE
        )
    }

    required <- switch(
        model,
        M0 = "response",
        M1 = c("response", "rt"),
        M2 = c("response", "rt", "gaze"),
        M3 = c("response", "rt", "gaze", "pupil")
    )

    absent <- setdiff(
        required,
        names(channels)
    )

    if (length(absent)) {
        stop(
            "Model ",
            model,
            " requires channels: ",
            paste(absent, collapse = ", "),
            call. = FALSE
        )
    }

    latent <- vapply(
        channels,
        function(z) {
            value <- z$latent

            if (
                is.null(value) ||
                length(value) != 1L ||
                is.na(value) ||
                !nzchar(as.character(value))
            ) {
                stop(
                    "Every multimodal channel must carry exactly one explicit latent identifier.",
                    call. = FALSE
                )
            }

            as.character(value)
        },
        character(1)
    )

    latent <- unique(
        unname(latent)
    )

    lifecycle_status <- if (
        identical(backend, "cmdstanr")
    ) {
        "gated"
    } else {
        "experimental"
    }

    interpretation <- paste(
        "Process channels are observational measurements,",
        "not named psychological constructs."
    )

    out <- irt_model_spec(
        id = paste0(
            "multimodal_",
            tolower(model),
            "_0_10"
        ),
        latent = latent,
        channels = channels,
        status = lifecycle_status,
        description = paste(
            "eyeprocess 0.10 staged multimodal measurement specification",
            model
        ),
        requirements = if (
            identical(backend, "cmdstanr")
        ) {
            "cmdstanr"
        } else {
            character()
        },
        metadata = list(
            architecture = "irt_model_spec_adapter",
            multimodal_model = model,
            backend = backend,
            identification = identification,
            priors = priors,
            interpretation = interpretation
        )
    )

    # Backward-compatible 0.10 convenience fields. The object itself
    # remains an established eye_irt_model_spec underneath the
    # eye_multimodal_irt_spec convenience subclass.
    out$model <- model
    out$backend <- backend
    out$identification <- identification
    out$priors <- priors
    out$lifecycle_status <- lifecycle_status
    out$interpretation <- interpretation

    class(out) <- unique(
        c(
            "eye_multimodal_irt_spec",
            class(out)
        )
    )

    out
}

print.eye_multimodal_irt_spec <- function(x, ...) {
    cat("<eye_multimodal_irt_spec>\n")
    cat(" model    :", x$model, "\n")
    cat(" backend  :", x$backend, "\n")
    cat(" channels :", paste(names(x$channels), collapse=", "), "\n")
    cat(" status   :", x$lifecycle_status, "\n")
    invisible(x)
}
