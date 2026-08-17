# eyeprocess 0.10 - M3 response + RT + gaze + pupil reference layer
#
# M3 extends the frozen M2 measurement architecture with a neutral pupil
# process channel. The reference pupil likelihood is a trial-level Gaussian
# summary with explicit nuisance adjustment. It is not a cognitive-load model.

.ep10_m3_reference <- list(
    model = "M3 four-channel response + RT + gaze + pupil",
    m2_reference_doi = "10.1177/01466216221089344",
    pupil_method_references = c(
        baseline = "10.3758/s13428-017-1007-2",
        gaze_position = "10.3758/s13428-011-0109-5",
        gaze_position_mapping = "10.3758/s13428-015-0588-x"
    ),
    likelihood = paste(
        "Rasch response + lognormal RT + negative-binomial fixation count +",
        "Gaussian pupil summary with explicit nuisance terms"
    ),
    interpretation = paste(
        "Pupil is an observed process measurement channel.",
        "The latent pupil dimension is named pupil responsivity and is not",
        "automatically interpreted as cognitive load, effort, or arousal."
    )
)

.ep10_m3_nuisance_names <- c(
    "baseline",
    "luminance",
    "gaze_x",
    "gaze_y",
    "quality",
    "blink",
    "interpolated",
    "time_on_task"
)

.ep10_m3_stan_file <- function(kind = c("full", "ablation")) {
    kind <- match.arg(kind)
    fname <- switch(
        kind,
        full = "m3-response-rt-gaze-pupil-0-10.stan",
        ablation = "m3-ablation-subset-0-10.stan"
    )

    installed <- system.file("stan", fname, package = "eyeprocess")
    if (nzchar(installed) && file.exists(installed)) {
        return(installed)
    }

    candidates <- c(
        file.path("inst", "stan", fname),
        file.path(getwd(), "inst", "stan", fname)
    )
    hit <- candidates[file.exists(candidates)]
    if (!length(hit)) {
        stop("Cannot locate eyeprocess 0.10 M3 Stan program: ", fname, call. = FALSE)
    }
    normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.ep10_m3_numeric <- function(x, name, allow_na = TRUE) {
    z <- suppressWarnings(as.numeric(x))
    if (!allow_na && anyNA(z)) {
        stop("`", name, "` must not contain missing values.", call. = FALSE)
    }
    bad <- !is.na(z) & !is.finite(z)
    if (any(bad)) {
        stop("Observed `", name, "` values must be finite.", call. = FALSE)
    }
    z
}

.ep10_m3_binary <- function(x, name) {
    z <- .ep10_m3_numeric(x, name)
    bad <- !is.na(z) & !z %in% c(0, 1)
    if (any(bad)) {
        stop("Observed `", name, "` values must be coded 0/1.", call. = FALSE)
    }
    z
}

.ep10_m3_scale_vector <- function(x, name, required_rows = rep(TRUE, length(x))) {
    x <- as.numeric(x)
    required_rows <- as.logical(required_rows)
    if (length(x) != length(required_rows)) {
        stop("Internal M3 scaling length mismatch for ", name, ".", call. = FALSE)
    }

    available_rows <- !is.na(x)
    if (!any(available_rows)) {
        return(list(
            value = rep(0, length(x)),
            center = NA_real_,
            scale = NA_real_,
            available = FALSE,
            degenerate = FALSE,
            missing_required = sum(required_rows)
        ))
    }

    missing_required <- sum(required_rows & is.na(x))
    if (missing_required > 0L) {
        stop(
            "M3 nuisance column `", name,
            "` is present but has missing values on observed pupil rows. ",
            "Either repair/justify those values or omit the nuisance column explicitly; ",
            "M3 does not silently impute pupil confounds.",
            call. = FALSE
        )
    }

    # Fix the nuisance parameterization using every available measurement, not
    # only rows whose pupil outcome happens to be observed. This avoids letting
    # channel dropout redefine covariate centering/scaling. Missing nuisance
    # values are permitted only outside the observed-pupil likelihood rows.
    center <- mean(x[available_rows])
    scale <- stats::sd(x[available_rows])
    degenerate <- !is.finite(scale) || scale <= sqrt(.Machine$double.eps)

    value <- rep(0, length(x))
    if (degenerate) {
        value[available_rows] <- 0
        scale <- 1
    } else {
        value[available_rows] <- (x[available_rows] - center) / scale
    }

    list(
        value = value,
        center = center,
        scale = scale,
        available = TRUE,
        degenerate = degenerate,
        missing_required = 0L
    )
}

.ep10_m3_as_data <- function(
    x,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    pupil = "pupil",
    baseline = "pupil_baseline",
    luminance = "luminance",
    gaze_x = "gaze_x",
    gaze_y = "gaze_y",
    quality = "pupil_quality",
    time_on_task = "time_on_task",
    blink = "pupil_blink",
    interpolated = "pupil_interpolated",
    device = "device",
    session = "session",
    sampling_rate = "sampling_rate_hz",
    pupil_scale = c("z", "raw")
) {
    pupil_scale <- match.arg(pupil_scale)

    if (inherits(x, "eye_multimodal_m3_simulation")) {
        x <- x$data
    } else if (inherits(x, "eye_multimodal_m3_fit")) {
        x <- x$data$raw
    } else if (
        inherits(x, "eye_multimodal_measurement") &&
        is.list(x) && is.data.frame(x$data)
    ) {
        x <- x$data
    } else if (is.list(x) && !is.data.frame(x) && is.data.frame(x$data)) {
        x <- x$data
    }

    if (!is.data.frame(x)) {
        stop(
            "`x` must be a data frame, eye_multimodal_m3_simulation, M3 fit, or compatible multimodal measurement object.",
            call. = FALSE
        )
    }

    required <- c(person, item, response, rt, gaze, pupil)
    absent <- setdiff(required, names(x))
    if (length(absent)) {
        stop("M3 data are missing required columns: ", paste(absent, collapse = ", "), call. = FALSE)
    }

    optional_map <- c(
        pupil_baseline = baseline,
        luminance = luminance,
        gaze_x = gaze_x,
        gaze_y = gaze_y,
        pupil_quality = quality,
        time_on_task = time_on_task,
        pupil_blink = blink,
        pupil_interpolated = interpolated,
        device = device,
        session = session,
        sampling_rate_hz = sampling_rate
    )

    get_optional <- function(nm, default = NA) {
        source <- optional_map[[nm]]
        if (is.null(source) || !length(source) || is.na(source) || !nzchar(source) || !source %in% names(x)) {
            return(rep(default, nrow(x)))
        }
        x[[source]]
    }

    d <- data.frame(
        person_id = as.character(x[[person]]),
        item_id = as.character(x[[item]]),
        response = x[[response]],
        rt = x[[rt]],
        gaze = x[[gaze]],
        pupil = x[[pupil]],
        pupil_baseline = get_optional("pupil_baseline"),
        luminance = get_optional("luminance"),
        gaze_x = get_optional("gaze_x"),
        gaze_y = get_optional("gaze_y"),
        pupil_quality = get_optional("pupil_quality"),
        time_on_task = get_optional("time_on_task"),
        pupil_blink = get_optional("pupil_blink"),
        pupil_interpolated = get_optional("pupil_interpolated"),
        device = as.character(get_optional("device", NA_character_)),
        session = as.character(get_optional("session", NA_character_)),
        sampling_rate_hz = get_optional("sampling_rate_hz"),
        source_row = seq_len(nrow(x)),
        stringsAsFactors = FALSE
    )

    if (
        anyNA(d$person_id) || any(!nzchar(d$person_id)) ||
        anyNA(d$item_id) || any(!nzchar(d$item_id))
    ) {
        stop("Person and item identifiers must be non-missing and non-empty.", call. = FALSE)
    }

    key <- paste(d$person_id, d$item_id, sep = "\r")
    if (anyDuplicated(key)) {
        stop("M3 reference data require at most one row per person-item key.", call. = FALSE)
    }

    d$response <- .ep10_m3_binary(d$response, "response")
    d$rt <- .ep10_m3_numeric(d$rt, "rt")
    if (any(!is.na(d$rt) & d$rt <= 0)) {
        stop("Observed response times must be strictly positive.", call. = FALSE)
    }

    d$gaze <- .ep10_m3_numeric(d$gaze, "gaze")
    if (any(!is.na(d$gaze) & (d$gaze < 0 | abs(d$gaze - round(d$gaze)) > sqrt(.Machine$double.eps)))) {
        stop("Observed gaze fixation counts must be non-negative integers.", call. = FALSE)
    }
    d$gaze[!is.na(d$gaze)] <- as.integer(round(d$gaze[!is.na(d$gaze)]))

    d$pupil <- .ep10_m3_numeric(d$pupil, "pupil")
    d$pupil_baseline <- .ep10_m3_numeric(d$pupil_baseline, "pupil_baseline")
    d$luminance <- .ep10_m3_numeric(d$luminance, "luminance")
    d$gaze_x <- .ep10_m3_numeric(d$gaze_x, "gaze_x")
    d$gaze_y <- .ep10_m3_numeric(d$gaze_y, "gaze_y")
    d$pupil_quality <- .ep10_m3_numeric(d$pupil_quality, "pupil_quality")
    d$time_on_task <- .ep10_m3_numeric(d$time_on_task, "time_on_task")
    d$pupil_blink <- .ep10_m3_binary(d$pupil_blink, "pupil_blink")
    d$pupil_interpolated <- .ep10_m3_binary(d$pupil_interpolated, "pupil_interpolated")
    d$sampling_rate_hz <- .ep10_m3_numeric(d$sampling_rate_hz, "sampling_rate_hz")
    if (any(!is.na(d$sampling_rate_hz) & d$sampling_rate_hz <= 0)) {
        stop("Observed sampling rates must be strictly positive.", call. = FALSE)
    }

    observed <- list(
        response = !is.na(d$response),
        rt = !is.na(d$rt),
        gaze = !is.na(d$gaze),
        pupil = !is.na(d$pupil)
    )

    person_levels <- sort(unique(d$person_id))
    item_levels <- sort(unique(d$item_id))
    d$person_index <- match(d$person_id, person_levels)
    d$item_index <- match(d$item_id, item_levels)

    pupil_rows <- observed$pupil
    pupil_transform <- list(center = 0, scale = 1, mode = pupil_scale)
    d$pupil_model <- d$pupil
    if (identical(pupil_scale, "z") && any(pupil_rows)) {
        center <- mean(d$pupil[pupil_rows])
        sc <- stats::sd(d$pupil[pupil_rows])
        if (!is.finite(sc) || sc <= sqrt(.Machine$double.eps)) {
            stop("Observed pupil summary has effectively zero variance; M3 pupil dimension is unsupported.", call. = FALSE)
        }
        d$pupil_model[pupil_rows] <- (d$pupil[pupil_rows] - center) / sc
        pupil_transform <- list(center = center, scale = sc, mode = "z")
    }

    nuisance_sources <- list(
        baseline = .ep10_m3_scale_vector(d$pupil_baseline, "pupil_baseline", pupil_rows),
        luminance = .ep10_m3_scale_vector(d$luminance, "luminance", pupil_rows),
        gaze_x = .ep10_m3_scale_vector(d$gaze_x, "gaze_x", pupil_rows),
        gaze_y = .ep10_m3_scale_vector(d$gaze_y, "gaze_y", pupil_rows),
        quality = .ep10_m3_scale_vector(d$pupil_quality, "pupil_quality", pupil_rows),
        blink = .ep10_m3_scale_vector(d$pupil_blink, "pupil_blink", pupil_rows),
        interpolated = .ep10_m3_scale_vector(d$pupil_interpolated, "pupil_interpolated", pupil_rows),
        time_on_task = .ep10_m3_scale_vector(d$time_on_task, "time_on_task", pupil_rows)
    )

    nuisance_matrix <- do.call(
        cbind,
        lapply(nuisance_sources, `[[`, "value")
    )
    colnames(nuisance_matrix) <- .ep10_m3_nuisance_names

    nuisance_meta <- data.frame(
        covariate = .ep10_m3_nuisance_names,
        available = vapply(nuisance_sources, `[[`, logical(1L), "available"),
        degenerate = vapply(nuisance_sources, `[[`, logical(1L), "degenerate"),
        center = vapply(nuisance_sources, `[[`, numeric(1L), "center"),
        scale = vapply(nuisance_sources, `[[`, numeric(1L), "scale"),
        stringsAsFactors = FALSE
    )

    list(
        raw = d,
        person_levels = person_levels,
        item_levels = item_levels,
        observed = observed,
        nuisance_matrix = nuisance_matrix,
        nuisance = nuisance_meta,
        pupil_transform = pupil_transform,
        optional_columns = optional_map
    )
}

.ep10_m3_to_stan <- function(
    data,
    prior_profile = c("regularized", "paper_centered"),
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names)
) {
    prior_profile <- match.arg(prior_profile)
    d <- data$raw
    rr <- which(data$observed$response)
    tr <- which(data$observed$rt)
    gr <- which(data$observed$gaze)
    pr <- which(data$observed$pupil)

    if (!length(rr) || !length(tr) || !length(gr) || !length(pr)) {
        stop("M3 fitting requires at least one observed value in response, RT, gaze, and pupil channels.", call. = FALSE)
    }

    if (is.null(names(nuisance)) || !all(.ep10_m3_nuisance_names %in% names(nuisance))) {
        stop("`nuisance` must name all M3 pupil nuisance terms.", call. = FALSE)
    }
    nuisance <- as.logical(nuisance[.ep10_m3_nuisance_names])
    use_cov <- as.integer(data$nuisance$available & !data$nuisance$degenerate & nuisance)
    X <- data$nuisance_matrix[pr, , drop = FALSE]

    out <- list(
        J = length(data$person_levels),
        I = length(data$item_levels),
        N_response = length(rr),
        person_response = as.integer(d$person_index[rr]),
        item_response = as.integer(d$item_index[rr]),
        y_response = as.integer(d$response[rr]),
        N_rt = length(tr),
        person_rt = as.integer(d$person_index[tr]),
        item_rt = as.integer(d$item_index[tr]),
        log_rt = log(as.numeric(d$rt[tr])),
        N_gaze = length(gr),
        person_gaze = as.integer(d$person_index[gr]),
        item_gaze = as.integer(d$item_index[gr]),
        gaze = as.integer(d$gaze[gr]),
        N_pupil = length(pr),
        person_pupil = as.integer(d$person_index[pr]),
        item_pupil = as.integer(d$item_index[pr]),
        pupil = as.numeric(d$pupil_model[pr]),
        X_pupil = unname(as.matrix(X)),
        use_pupil_covariate = use_cov,
        prior_profile = if (identical(prior_profile, "regularized")) 1L else 2L
    )

    attr(out, "row_index") <- list(response = rr, rt = tr, gaze = gr, pupil = pr)
    attr(out, "nuisance_names") <- .ep10_m3_nuisance_names
    out
}

#' M3 response + RT + gaze + pupil specification
#'
#' Extends the established eyeprocess multimodal IRT specification with a
#' continuous pupil-responsivity channel. The summary representation is fitted
#' by the bundled four-channel Stan model. `functional_score` records that the
#' pupil input was derived from a trajectory/functional workflow; it does not
#' silently turn the scalar reference likelihood into a functional likelihood.
#'
#' @param backend Currently `"cmdstanr"` only.
#' @param prior_profile Prior profile.
#' @param missingness Currently `"ignorable"` only.
#' @param pupil_representation `"summary"` or `"functional_score"`.
#' @param nuisance Named logical vector selecting baseline, luminance, gaze X/Y,
#'   quality, blink, interpolation and time-on-task nuisance terms. Availability
#'   is also audited from data.
#' @return An `eye_multimodal_m3_spec` inheriting the established multimodal and IRT spec classes.
#' @export
multimodal_m3_spec <- function(
    backend = "cmdstanr",
    prior_profile = c("regularized", "paper_centered"),
    missingness = "ignorable",
    pupil_representation = c("summary", "functional_score"),
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names)
) {
    if (!identical(backend, "cmdstanr")) {
        stop("M3 currently supports only `cmdstanr`; no fallback estimator is substituted.", call. = FALSE)
    }
    if (!identical(missingness, "ignorable")) {
        stop("The M3 reference likelihood currently supports ignorable channel missingness only; use recovery/sensitivity scenarios to study violations.", call. = FALSE)
    }
    prior_profile <- match.arg(prior_profile)
    pupil_representation <- match.arg(pupil_representation)

    if (is.null(names(nuisance)) || !all(.ep10_m3_nuisance_names %in% names(nuisance))) {
        stop("`nuisance` must be a named logical vector containing: ", paste(.ep10_m3_nuisance_names, collapse = ", "), call. = FALSE)
    }
    nuisance <- as.logical(nuisance[.ep10_m3_nuisance_names])
    names(nuisance) <- .ep10_m3_nuisance_names

    response_channel <- irt_response_channel(family = "rasch", latent = "ability")
    rt_channel <- irt_rt_channel(family = "lognormal", latent = "speed")
    gaze_channel <- irt_count_channel(family = "negative_binomial", latent = "gaze_process")
    continuous_formals <- names(formals(irt_continuous_channel))
    continuous_args <- list()
    if ("family" %in% continuous_formals || "..." %in% continuous_formals) {
        continuous_args$family <- "gaussian"
    }
    if ("latent" %in% continuous_formals || "..." %in% continuous_formals) {
        continuous_args$latent <- "pupil_responsivity"
    } else {
        stop(
            "The installed eyeprocess `irt_continuous_channel()` does not expose an explicit latent identifier; M3 refuses to create a parallel pupil channel.",
            call. = FALSE
        )
    }
    pupil_channel <- do.call(irt_continuous_channel, continuous_args)
    if (!inherits(pupil_channel, "eye_irt_channel")) {
        stop("Existing `irt_continuous_channel()` did not return an eye_irt_channel.", call. = FALSE)
    }

    out <- multimodal_irt_spec(
        response = response_channel,
        rt = rt_channel,
        gaze = gaze_channel,
        pupil = pupil_channel,
        model = "M3",
        backend = "cmdstanr",
        identification = list(
            person_latent_means = c(
                ability = 0,
                speed = 0,
                gaze_process = 0,
                pupil_responsivity = 0
            ),
            response_discrimination = 1,
            rt_person_loading = -1,
            gaze_person_loading = 1,
            pupil_person_loading = 1,
            item_effects = "hierarchical correlated location parameters"
        ),
        priors = list(
            profile = prior_profile,
            note = "Regularized four-channel priors; no exact published four-channel hyperprior reproduction is claimed."
        )
    )

    out$reference <- .ep10_m3_reference
    out$missingness <- missingness
    out$pupil_representation <- pupil_representation
    out$pupil_nuisance <- nuisance
    out$fidelity <- list(
        response = "Rasch / 1PL",
        response_time = "lognormal with item time discrimination",
        gaze = "negative-binomial fixation count",
        pupil = "Gaussian standardized trial-level pupil summary",
        person_structure = "correlated ability-speed-gaze-pupil effects",
        item_structure = "correlated response-time-gaze-pupil location effects"
    )
    out$lifecycle_status <- "experimental"
    out$interpretation <- .ep10_m3_reference$interpretation

    class(out) <- unique(c("eye_multimodal_m3_spec", class(out)))
    out
}

print.eye_multimodal_m3_spec <- function(x, ...) {
    cat(
        "<eye_multimodal_m3_spec>\n",
        "  model: M3 response + RT + gaze + pupil\n",
        "  backend: ", x$backend, "\n",
        "  pupil representation: ", x$pupil_representation, "\n",
        "  likelihood: Rasch + lognormal RT + NB gaze + Gaussian pupil\n",
        "  missingness: ", x$missingness, "\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        "  boundary: pupil responsivity is a neutral process dimension\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m3_compile <- function(kind = c("full", "ablation"), quiet = TRUE) {
    kind <- match.arg(kind)
    .ep10_m2_require_backend()
    cmdstanr::cmdstan_model(.ep10_m3_stan_file(kind), quiet = quiet)
}

#' Fit the M3 response + RT + gaze + pupil reference model
#'
#' Fits the four-channel reference likelihood using CmdStanR. Pupil summaries
#' are standardized by default for a scale-stable reference parameterization;
#' the transformation is retained in the returned data object. No missing
#' nuisance values are silently imputed when the corresponding nuisance column
#' is supplied.
#'
#' @param x Data frame, M3 simulation, or compatible measurement object.
#' @param person,item,response,rt,gaze,pupil Column names.
#' @param baseline,luminance,gaze_x,gaze_y,quality,time_on_task Optional nuisance columns.
#' @param blink,interpolated Optional pupil nuisance/audit indicators.
#' @param device,session,sampling_rate Optional measurement-context audit columns.
#' @param pupil_scale `"z"` or `"raw"`.
#' @param prior_profile Prior profile.
#' @param nuisance Named logical vector selecting the eight explicit pupil measurement nuisance terms.
#' @param chains,parallel_chains,iter_warmup,iter_sampling CmdStan sampling controls.
#' @param seed,adapt_delta,max_treedepth,refresh,quiet_compile CmdStan controls.
#' @param init CmdStan initialization; default zero initializes Cholesky factors at an interior identity point.
#' @return An `eye_multimodal_m3_fit`.
#' @export
fit_multimodal_m3 <- function(
    x,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    pupil = "pupil",
    baseline = "pupil_baseline",
    luminance = "luminance",
    gaze_x = "gaze_x",
    gaze_y = "gaze_y",
    quality = "pupil_quality",
    time_on_task = "time_on_task",
    blink = "pupil_blink",
    interpolated = "pupil_interpolated",
    device = "device",
    session = "session",
    sampling_rate = "sampling_rate_hz",
    pupil_scale = c("z", "raw"),
    prior_profile = c("regularized", "paper_centered"),
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    seed = 20260815L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    quiet_compile = TRUE,
    init = 0
) {
    pupil_scale <- match.arg(pupil_scale)
    prior_profile <- match.arg(prior_profile)
    .ep10_m2_require_backend()

    data <- .ep10_m3_as_data(
        x,
        person = person,
        item = item,
        response = response,
        rt = rt,
        gaze = gaze,
        pupil = pupil,
        baseline = baseline,
        luminance = luminance,
        gaze_x = gaze_x,
        gaze_y = gaze_y,
        quality = quality,
        time_on_task = time_on_task,
        blink = blink,
        interpolated = interpolated,
        device = device,
        session = session,
        sampling_rate = sampling_rate,
        pupil_scale = pupil_scale
    )

    audit <- audit_multimodal_m3_identifiability(data$raw, pupil_scale = pupil_scale)
    if (!isTRUE(audit$supported)) {
        stop("M3 structural/data audit does not support fitting. Inspect `audit_multimodal_m3_identifiability()` first.", call. = FALSE)
    }

    stan_data <- .ep10_m3_to_stan(data, prior_profile = prior_profile, nuisance = nuisance)
    mod <- .ep10_m3_compile("full", quiet = quiet_compile)
    fit <- mod$sample(
        data = stan_data,
        seed = as.integer(seed),
        chains = as.integer(chains),
        parallel_chains = as.integer(parallel_chains),
        iter_warmup = as.integer(iter_warmup),
        iter_sampling = as.integer(iter_sampling),
        adapt_delta = adapt_delta,
        max_treedepth = as.integer(max_treedepth),
        refresh = as.integer(refresh),
        init = init
    )

    spec <- multimodal_m3_spec(prior_profile = prior_profile, nuisance = nuisance)
    out <- list(
        model = "M3",
        fit = fit,
        data = data,
        stan_data = stan_data,
        spec = spec,
        audit = audit,
        reference = .ep10_m3_reference,
        prior_profile = prior_profile,
        pupil_scale = pupil_scale,
        pupil_nuisance = stats::setNames(
            as.logical(stan_data$use_pupil_covariate),
            .ep10_m3_nuisance_names
        ),
        seed = as.integer(seed),
        sampling_controls = list(
            chains = as.integer(chains),
            parallel_chains = as.integer(parallel_chains),
            iter_warmup = as.integer(iter_warmup),
            iter_sampling = as.integer(iter_sampling),
            adapt_delta = adapt_delta,
            max_treedepth = as.integer(max_treedepth),
            init = init
        ),
        lifecycle_status = "gated",
        interpretation = .ep10_m3_reference$interpretation,
        call = match.call()
    )
    class(out) <- c("eye_multimodal_m3_fit", "list")
    out
}

print.eye_multimodal_m3_fit <- function(x, ...) {
    cat(
        "<eye_multimodal_m3_fit>\n",
        "  persons: ", length(x$data$person_levels), "\n",
        "  items: ", length(x$data$item_levels), "\n",
        "  response observed: ", sum(x$data$observed$response), "\n",
        "  RT observed: ", sum(x$data$observed$rt), "\n",
        "  gaze observed: ", sum(x$data$observed$gaze), "\n",
        "  pupil observed: ", sum(x$data$observed$pupil), "\n",
        "  pupil scale: ", x$pupil_scale, "\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        sep = ""
    )
    invisible(x)
}

summary.eye_multimodal_m3_fit <- function(object, ...) {
    vars <- c(
        "mu_item", "sigma_person", "sigma_item", "corr_person", "corr_item",
        "nu", "s", "sigma_pupil", "gamma_pupil"
    )
    tab <- object$fit$summary(variables = vars, probs = c(0.025, 0.5, 0.975))
    structure(
        list(
            model = "M3",
            summary = tab,
            audit = object$audit,
            nuisance = object$data$nuisance,
            pupil_transform = object$data$pupil_transform,
            lifecycle_status = object$lifecycle_status,
            interpretation = object$interpretation
        ),
        class = "summary.eye_multimodal_m3_fit"
    )
}

print.summary.eye_multimodal_m3_fit <- function(x, ...) {
    cat(
        "<summary.eye_multimodal_m3_fit>\n",
        "  model: M3\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        "  pupil boundary: neutral pupil responsivity process dimension\n\n",
        sep = ""
    )
    print(x$summary, row.names = FALSE)
    invisible(x)
}

coef.eye_multimodal_m3_fit <- function(object, ...) {
    object$fit$summary(
        variables = c(
            "mu_item", "sigma_person", "sigma_item", "corr_person", "corr_item",
            "sigma_pupil", "gamma_pupil"
        ),
        probs = c(0.025, 0.5, 0.975)
    )
}

.ep10_m3_draw_mean <- function(object, variable) {
    z <- posterior::as_draws_matrix(object$fit$draws(variables = variable))
    colMeans(z)
}

fitted.eye_multimodal_m3_fit <- function(object, ...) {
    d <- object$data$raw
    theta <- .ep10_m3_draw_mean(object, "theta")
    tau <- .ep10_m3_draw_mean(object, "tau")
    omega <- .ep10_m3_draw_mean(object, "omega")
    rho <- .ep10_m3_draw_mean(object, "rho")
    b <- .ep10_m3_draw_mean(object, "b")
    beta <- .ep10_m3_draw_mean(object, "beta")
    m <- .ep10_m3_draw_mean(object, "m")
    kappa <- .ep10_m3_draw_mean(object, "kappa")
    nu <- .ep10_m3_draw_mean(object, "nu")
    s <- .ep10_m3_draw_mean(object, "s")
    gamma <- .ep10_m3_draw_mean(object, "gamma_pupil")

    j <- d$person_index
    i <- d$item_index
    X <- object$data$nuisance_matrix
    use <- as.numeric(object$stan_data$use_pupil_covariate)
    nuisance <- as.numeric(X %*% (gamma * use))

    pupil_model <- kappa[i] + rho[j] + nuisance
    pupil_raw <- if (identical(object$data$pupil_transform$mode, "z")) {
        object$data$pupil_transform$center + object$data$pupil_transform$scale * pupil_model
    } else {
        pupil_model
    }

    data.frame(
        source_row = d$source_row,
        response_probability = stats::plogis(theta[j] - b[i]),
        rt_median = exp(beta[i] - tau[j]),
        gaze_mean = exp(m[i] + omega[j]),
        pupil_mean = pupil_raw,
        stringsAsFactors = FALSE
    )
}

residuals.eye_multimodal_m3_fit <- function(object, ...) {
    f <- fitted(object)
    d <- object$data$raw
    data.frame(
        source_row = d$source_row,
        response = d$response - f$response_probability,
        log_rt = ifelse(is.na(d$rt), NA_real_, log(d$rt) - log(f$rt_median)),
        gaze = d$gaze - f$gaze_mean,
        pupil = d$pupil - f$pupil_mean,
        stringsAsFactors = FALSE
    )
}
