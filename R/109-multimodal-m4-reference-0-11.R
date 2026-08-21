# eyeprocess 0.11 - M4 trait-conditioned latent response-process states
#
# M4 extends the frozen M3 response + RT + gaze + pupil measurement model with
# a marginalized discrete latent state process over ordered trials. The state
# modifies process channels only in the reference implementation. It does not
# modify the scored-response ability equation and it has no automatic
# psychological interpretation.

.ep11_m4_reference <- list(
    model = "M4 response + RT + gaze + pupil + latent response-process state",
    parent = "M3 response + RT + gaze + pupil",
    state_definition = paste(
        "A model-based latent response-process state representing a recurring",
        "statistical configuration of observed process measurements conditional",
        "on the specified psychometric and sequential model."
    ),
    interpretation = paste(
        "M4 states are statistical response-process states.",
        "State membership does not by itself establish a cognitive strategy,",
        "attention, engagement, cognitive load, effort, emotion, guessing,",
        "misconduct, comprehension, or another psychological construct."
    ),
    reference_constraint = paste(
        "State effects enter RT, gaze, and pupil process channels only;",
        "the M3 Rasch scored-response equation remains state-independent."
    )
)

.ep11_m4_state_channels <- c("rt", "gaze", "pupil")
.ep11_m4_trait_names <- c("theta", "tau", "omega", "rho")

.ep11_m4_stan_file <- function() {
    fname <- "m4-trait-conditioned-state-0-11.stan"
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
        stop("Cannot locate eyeprocess 0.11 M4 Stan program: ", fname, call. = FALSE)
    }
    normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.ep11_m4_unwrap_data <- function(x) {
    if (inherits(x, "eye_multimodal_m4_simulation")) {
        return(x$data)
    }
    if (inherits(x, "eye_multimodal_m4_fit")) {
        return(x$data$raw)
    }
    if (inherits(x, "eye_multimodal_m3_simulation")) {
        return(x$data)
    }
    if (inherits(x, "eye_multimodal_measurement") && is.list(x) && is.data.frame(x$data)) {
        return(x$data)
    }
    if (is.list(x) && !is.data.frame(x) && is.data.frame(x$data)) {
        return(x$data)
    }
    if (!is.data.frame(x)) {
        stop(
            "`x` must be a data frame, M4 simulation/fit, or compatible eyeprocess measurement object.",
            call. = FALSE
        )
    }
    x
}

.ep11_m4_validate_sequence <- function(d, sequence = "sequence_id", order = "trial_index", min_sequence_length = 2L) {
    if (!sequence %in% names(d)) {
        stop("M4 data are missing sequence column `", sequence, "`.", call. = FALSE)
    }
    if (!order %in% names(d)) {
        stop("M4 data are missing ordering column `", order, "`.", call. = FALSE)
    }

    seq_id <- as.character(d[[sequence]])
    ord <- suppressWarnings(as.numeric(d[[order]]))
    if (anyNA(seq_id) || any(!nzchar(seq_id))) {
        stop("M4 sequence identifiers must be non-missing and non-empty.", call. = FALSE)
    }
    if (anyNA(ord) || any(!is.finite(ord))) {
        stop("M4 ordering values must be finite and non-missing.", call. = FALSE)
    }

    # Sequence blocks must already be contiguous. M4 never silently sorts rows.
    r <- rle(seq_id)
    if (anyDuplicated(r$values)) {
        stop(
            "Each M4 sequence must occupy one contiguous block of rows. Reorder explicitly before fitting; M4 does not silently sort data.",
            call. = FALSE
        )
    }

    starts <- cumsum(c(1L, head(r$lengths, -1L)))
    lens <- as.integer(r$lengths)
    seq_levels <- r$values

    bad_order <- logical(length(lens))
    dup_order <- logical(length(lens))
    for (s in seq_along(lens)) {
        idx <- starts[[s]]:(starts[[s]] + lens[[s]] - 1L)
        z <- ord[idx]
        dup_order[[s]] <- anyDuplicated(z) > 0L
        bad_order[[s]] <- length(z) > 1L && any(diff(z) <= 0)
    }
    if (any(dup_order)) {
        stop("Ordering values must be unique within each M4 sequence.", call. = FALSE)
    }
    if (any(bad_order)) {
        stop(
            "Rows are not strictly increasing by the declared M4 order within at least one sequence. Sort explicitly and preserve that decision in provenance.",
            call. = FALSE
        )
    }

    if ("person_id" %in% names(d)) {
        person_per_seq <- vapply(
            seq_along(lens),
            function(s) {
                idx <- starts[[s]]:(starts[[s]] + lens[[s]] - 1L)
                length(unique(as.character(d$person_id[idx])))
            },
            integer(1L)
        )
        if (any(person_per_seq != 1L)) {
            stop("Each M4 sequence must belong to exactly one person.", call. = FALSE)
        }
    }

    list(
        sequence_id = seq_id,
        order = ord,
        levels = seq_levels,
        start = as.integer(starts),
        length = lens,
        n_sequence = length(lens),
        n_transition = sum(pmax(lens - 1L, 0L)),
        short = lens < as.integer(min_sequence_length),
        min_sequence_length = as.integer(min_sequence_length)
    )
}

.ep11_m4_as_data <- function(
    x,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    pupil = "pupil",
    sequence = "sequence_id",
    order = "trial_index",
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
    min_sequence_length = 2L
) {
    pupil_scale <- match.arg(pupil_scale)
    raw_input <- .ep11_m4_unwrap_data(x)

    if (!sequence %in% names(raw_input) || !order %in% names(raw_input)) {
        missing <- setdiff(c(sequence, order), names(raw_input))
        stop("M4 data are missing required sequence fields: ", paste(missing, collapse = ", "), call. = FALSE)
    }

    # Reuse the established M3 measurement-data normalization exactly.
    m3 <- .ep10_m3_as_data(
        raw_input,
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

    m3$raw$sequence_id <- as.character(raw_input[[sequence]])
    m3$raw$trial_index <- suppressWarnings(as.numeric(raw_input[[order]]))
    sequence_info <- .ep11_m4_validate_sequence(
        m3$raw,
        sequence = "sequence_id",
        order = "trial_index",
        min_sequence_length = min_sequence_length
    )

    m3$sequence <- sequence_info
    m3$sequence_columns <- list(sequence = sequence, order = order)
    m3
}

.ep11_m4_named_logical <- function(x, allowed, name) {
    if (is.null(names(x))) {
        stop("`", name, "` must be a named logical vector.", call. = FALSE)
    }
    unknown <- setdiff(names(x), allowed)
    if (length(unknown)) {
        stop("Unknown `", name, "` names: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    out <- stats::setNames(rep(FALSE, length(allowed)), allowed)
    out[names(x)] <- as.logical(x)
    out
}

#' Specify M4 trait-conditioned latent response-process states
#'
#' Defines the M4 sequential extension of the validated M3 response + RT + gaze
#' + pupil model. The reference implementation keeps the scored-response Rasch
#' equation state-independent and lets latent states shift selected RT, gaze,
#' and pupil process channels. State labels are statistical identification labels
#' only and do not imply psychological constructs.
#'
#' @param n_states Number of latent states, from 1 through 4. `1` is the formal
#'   null state model and should be treated as scientifically meaningful.
#' @param state_channels Subset of `"rt"`, `"gaze"`, and `"pupil"` receiving
#'   state-dependent deviations.
#' @param transition_structure `"markov"` for first-order transitions or
#'   `"iid"` for independent state membership over ordered trials.
#' @param trait_conditioning Subset of `theta`, `tau`, `omega`, and `rho` used
#'   to condition transition logits. The conservative default is `theta + tau`.
#' @param initial_trait_conditioning Whether the same selected traits condition
#'   initial-state probabilities.
#' @param min_sequence_length Minimum sequence length required by the structural
#'   audit for fitting. No rows are silently removed when sequences are shorter.
#' @param identification State-label identification policy. The reference policy
#'   orders centered RT state deviations; this is a label convention only.
#' @param prior_profile Prior profile inherited from M3.
#' @param missingness Currently `"ignorable"` only.
#' @param nuisance Named pupil-nuisance selection vector inherited from M3.
#' @param backend Currently `"cmdstanr"` only.
#' @return An `eye_multimodal_m4_spec` inheriting the canonical eyeprocess
#'   multimodal/IRT specification classes.
#' @export
multimodal_m4_spec <- function(
    n_states = 2L,
    state_channels = c("rt", "gaze", "pupil"),
    transition_structure = c("markov", "iid"),
    trait_conditioning = c("theta", "tau"),
    initial_trait_conditioning = TRUE,
    min_sequence_length = 2L,
    identification = c("ordered_rt_effect"),
    prior_profile = c("regularized", "paper_centered"),
    missingness = "ignorable",
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
    backend = "cmdstanr"
) {
    n_states <- as.integer(n_states)
    if (length(n_states) != 1L || is.na(n_states) || n_states < 1L || n_states > 4L) {
        stop("`n_states` must be one integer from 1 through 4.", call. = FALSE)
    }
    state_channels <- unique(as.character(state_channels))
    if (!length(state_channels) || any(!state_channels %in% .ep11_m4_state_channels)) {
        stop("`state_channels` must be a non-empty subset of: rt, gaze, pupil.", call. = FALSE)
    }
    if (n_states > 1L && !"rt" %in% state_channels) {
        stop(
            "The 0.11 M4 reference identification orders RT state deviations, so K > 1 requires `rt` in `state_channels`. No-RT state models require a separately validated identification policy and are intentionally not substituted silently.",
            call. = FALSE
        )
    }
    transition_structure <- match.arg(transition_structure)
    trait_conditioning <- unique(as.character(trait_conditioning))
    if (any(!trait_conditioning %in% .ep11_m4_trait_names)) {
        stop("`trait_conditioning` may contain only: theta, tau, omega, rho.", call. = FALSE)
    }
    identification <- match.arg(identification)
    prior_profile <- match.arg(prior_profile)
    if (!identical(backend, "cmdstanr")) {
        stop("M4 currently supports only `cmdstanr`; no fallback state estimator is substituted.", call. = FALSE)
    }
    if (!identical(missingness, "ignorable")) {
        stop(
            "The M4 reference likelihood currently supports ignorable channel missingness only; use sensitivity/recovery stress scenarios for violations.",
            call. = FALSE
        )
    }
    min_sequence_length <- as.integer(min_sequence_length)
    if (length(min_sequence_length) != 1L || is.na(min_sequence_length) || min_sequence_length < 1L) {
        stop("`min_sequence_length` must be a positive integer.", call. = FALSE)
    }
    nuisance <- .ep11_m4_named_logical(nuisance, .ep10_m3_nuisance_names, "nuisance")

    # The canonical M3 channel object remains the measurement backbone.
    m3 <- multimodal_m3_spec(
        backend = backend,
        prior_profile = prior_profile,
        missingness = missingness,
        nuisance = nuisance
    )

    out <- m3
    out$model <- "M4"
    out$m3_parent <- m3
    out$reference <- .ep11_m4_reference
    out$n_states <- n_states
    out$state_channels <- state_channels
    out$transition_structure <- transition_structure
    out$trait_conditioning <- trait_conditioning
    out$initial_trait_conditioning <- isTRUE(initial_trait_conditioning)
    out$min_sequence_length <- min_sequence_length
    out$state_identification <- identification
    out$state_null <- n_states == 1L
    out$prior_profile <- prior_profile
    out$lifecycle_status <- "experimental"
    out$interpretation <- .ep11_m4_reference$interpretation
    out$identification$m4_state <- list(
        policy = identification,
        note = "Ordering RT state deviations with a proper adjacent-gap prior fixes labels away from the zero-separation identification boundary; it does not order psychological meaning."
    )
    class(out) <- unique(c("eye_multimodal_m4_spec", class(out)))
    out
}

print.eye_multimodal_m4_spec <- function(x, ...) {
    cat(
        "<eye_multimodal_m4_spec>\n",
        "  model: M4 response + RT + gaze + pupil + latent process state\n",
        "  states: ", x$n_states, if (isTRUE(x$state_null)) " (formal null)" else "", "\n",
        "  state channels: ", paste(x$state_channels, collapse = ", "), "\n",
        "  transition structure: ", x$transition_structure, "\n",
        "  trait conditioning: ", if (length(x$trait_conditioning)) paste(x$trait_conditioning, collapse = ", ") else "none", "\n",
        "  identification: ", x$state_identification, "\n",
        "  boundary: scored-response ability equation is state-independent\n",
        "  interpretation: statistical response-process states; no psychological label implied\n",
        sep = ""
    )
    invisible(x)
}

.ep11_m4_to_stan <- function(data, spec) {
    d <- data$raw
    N <- nrow(d)
    if (!N) stop("M4 fitting requires at least one row.", call. = FALSE)

    observed <- data$observed
    if (!any(observed$response)) {
        stop("M4 fitting requires at least one observed scored response.", call. = FALSE)
    }
    for (ch in spec$state_channels) {
        if (!any(observed[[ch]])) {
            stop("M4 state channel `", ch, "` has no observed values.", call. = FALSE)
        }
    }

    if (any(data$sequence$short)) {
        bad <- data$sequence$levels[data$sequence$short]
        stop(
            "M4 structural audit found sequences shorter than `min_sequence_length`: ",
            paste(utils::head(bad, 8L), collapse = ", "),
            if (length(bad) > 8L) " ..." else "",
            ". No sequences are silently discarded.",
            call. = FALSE
        )
    }

    rr <- which(observed$response)
    use_cov <- as.integer(
        data$nuisance$available & !data$nuisance$degenerate &
            as.logical(spec$pupil_nuisance[.ep10_m3_nuisance_names])
    )

    trait_use <- as.integer(.ep11_m4_trait_names %in% spec$trait_conditioning)
    if (!isTRUE(spec$initial_trait_conditioning)) {
        init_trait_use <- rep(0L, 4L)
    } else {
        init_trait_use <- trait_use
    }

    list(
        J = length(data$person_levels),
        I = length(data$item_levels),
        N = N,
        person = as.integer(d$person_index),
        item = as.integer(d$item_index),
        N_response = length(rr),
        response_row = as.integer(rr),
        y_response = as.integer(d$response[rr]),
        rt_observed = as.integer(observed$rt),
        log_rt = as.numeric(ifelse(observed$rt, log(d$rt), 0)),
        gaze_observed = as.integer(observed$gaze),
        gaze = as.integer(ifelse(observed$gaze, d$gaze, 0)),
        pupil_observed = as.integer(observed$pupil),
        pupil = as.numeric(ifelse(observed$pupil, d$pupil_model, 0)),
        X_pupil = unname(as.matrix(data$nuisance_matrix)),
        use_pupil_covariate = use_cov,
        S = data$sequence$n_sequence,
        seq_start = as.integer(data$sequence$start),
        seq_len = as.integer(data$sequence$length),
        K = as.integer(spec$n_states),
        use_state_channel = as.integer(.ep11_m4_state_channels %in% spec$state_channels),
        transition_structure = if (identical(spec$transition_structure, "markov")) 1L else 2L,
        use_transition_trait = trait_use,
        use_initial_trait = init_trait_use,
        prior_profile = if (identical(spec$prior_profile, "regularized")) 1L else 2L
    )
}

.ep11_m4_compile <- function(quiet = TRUE) {
    .ep10_m2_require_backend()

    source <- .ep11_m4_stan_file()
    source_md5 <- unname(tools::md5sum(source))

    if (!file.exists(source) || !nzchar(source_md5)) {
        stop("M4 Stan source is unavailable.", call. = FALSE)
    }

    compile_dir <- file.path(
        tempdir(),
        "eyeprocess-m4-cmdstan",
        source_md5
    )

    dir.create(
        compile_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )

    stan_file <- file.path(
        compile_dir,
        basename(source)
    )

    copy_required <-
        !file.exists(stan_file) ||
        !identical(
            unname(tools::md5sum(stan_file)),
            source_md5
        )

    if (copy_required) {
        copied <- file.copy(
            source,
            stan_file,
            overwrite = TRUE,
            copy.mode = TRUE,
            copy.date = TRUE
        )

        if (!isTRUE(copied)) {
            stop("Could not create temporary M4 Stan source copy.", call. = FALSE)
        }
    }

    if (!identical(
        unname(tools::md5sum(stan_file)),
        source_md5
    )) {
        stop("Temporary M4 Stan source failed byte-integrity verification.", call. = FALSE)
    }

    cmdstanr::cmdstan_model(
        stan_file,
        quiet = quiet
    )
}

#' Fit the M4 latent response-process state model
#'
#' Fits the marginalized M4 state model using CmdStanR. Sequence order is
#' validated but never silently changed. The latent state contributes only to
#' selected process channels in the reference implementation; the scored-response
#' Rasch equation remains exactly state-independent.
#'
#' @param x Data frame, M4 simulation, or compatible eyeprocess object.
#' @param spec Optional `multimodal_m4_spec()`. When omitted, a conservative
#'   two-state specification is created from the explicit arguments.
#' @param person,item,response,rt,gaze,pupil Column names.
#' @param sequence,order Sequence identifier and within-sequence order columns.
#' @param baseline,luminance,gaze_x,gaze_y,quality,time_on_task,blink,interpolated
#'   Optional M3 pupil nuisance columns.
#' @param device,session,sampling_rate Optional measurement-context columns.
#' @param pupil_scale `"z"` or `"raw"`.
#' @param n_states,state_channels,transition_structure,trait_conditioning Used
#'   only when `spec` is null.
#' @param initial_trait_conditioning,min_sequence_length Used only when `spec`
#'   is null.
#' @param prior_profile,nuisance Used only when `spec` is null.
#' @param chains,parallel_chains,iter_warmup,iter_sampling Sampling controls.
#' @param seed,adapt_delta,max_treedepth,refresh,quiet_compile,init CmdStan controls.
#' @return An `eye_multimodal_m4_fit` retaining data, sequence audit, specification,
#'   Stan data, sampling controls, and provenance.
#' @export
fit_multimodal_m4 <- function(
    x,
    spec = NULL,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    pupil = "pupil",
    sequence = "sequence_id",
    order = "trial_index",
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
    n_states = 2L,
    state_channels = c("rt", "gaze", "pupil"),
    transition_structure = c("markov", "iid"),
    trait_conditioning = c("theta", "tau"),
    initial_trait_conditioning = TRUE,
    min_sequence_length = 2L,
    prior_profile = c("regularized", "paper_centered"),
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    seed = 20260820L,
    adapt_delta = 0.97,
    max_treedepth = 13L,
    refresh = 100L,
    quiet_compile = TRUE,
    init = 0
) {
    pupil_scale <- match.arg(pupil_scale)
    transition_structure <- match.arg(transition_structure)
    prior_profile <- match.arg(prior_profile)
    .ep10_m2_require_backend()

    if (is.null(spec)) {
        spec <- multimodal_m4_spec(
            n_states = n_states,
            state_channels = state_channels,
            transition_structure = transition_structure,
            trait_conditioning = trait_conditioning,
            initial_trait_conditioning = initial_trait_conditioning,
            min_sequence_length = min_sequence_length,
            prior_profile = prior_profile,
            nuisance = nuisance
        )
    }
    if (!inherits(spec, "eye_multimodal_m4_spec")) {
        stop("`spec` must be produced by `multimodal_m4_spec()`.", call. = FALSE)
    }

    data <- .ep11_m4_as_data(
        x,
        person = person,
        item = item,
        response = response,
        rt = rt,
        gaze = gaze,
        pupil = pupil,
        sequence = sequence,
        order = order,
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
        pupil_scale = pupil_scale,
        min_sequence_length = spec$min_sequence_length
    )

    audit <- audit_multimodal_m4_identifiability(data, spec = spec, include_posterior = FALSE)
    if (identical(audit$overall, "FAIL")) {
        stop("M4 structural/data audit failed. Inspect `audit_multimodal_m4_identifiability()` before fitting.", call. = FALSE)
    }

    trait_markov_review <- any(
        audit$checks$criterion == "trait_conditioned_markov" &
            audit$checks$status == "REVIEW"
    )

    if (trait_markov_review) {
        warning(
            "M4 trait-conditioned Markov specification is gated: structural adequacy does not establish posterior identifiability. Require converged R-hat/ESS, stable state occupancy across chains, and separated state emissions before interpretation.",
            call. = FALSE
        )
    }

    stan_data <- .ep11_m4_to_stan(data, spec)
    mod <- .ep11_m4_compile(quiet = quiet_compile)
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

    out <- list(
        model = "M4",
        fit = fit,
        data = data,
        stan_data = stan_data,
        spec = spec,
        audit_pre = audit,
        reference = .ep11_m4_reference,
        pupil_scale = pupil_scale,
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
        provenance = list(
            package_version = as.character(utils::packageVersion("eyeprocess")),
            model = "M4",
            n_states = spec$n_states,
            state_channels = spec$state_channels,
            transition_structure = spec$transition_structure,
            trait_conditioning = spec$trait_conditioning,
            initial_trait_conditioning = spec$initial_trait_conditioning,
            sequence_column = sequence,
            order_column = order,
            missingness = spec$missingness,
            prior_profile = spec$prior_profile,
            state_identification = spec$state_identification,
            stan_file = basename(.ep11_m4_stan_file()),
            stan_md5 = unname(tools::md5sum(.ep11_m4_stan_file())),
            seed = as.integer(seed),
            created = format(Sys.time(), tz = "UTC", usetz = TRUE)
        ),
        lifecycle_status = "gated",
        interpretation = .ep11_m4_reference$interpretation,
        call = match.call()
    )
    class(out) <- c("eye_multimodal_m4_fit", "list")
    out
}

print.eye_multimodal_m4_fit <- function(x, ...) {
    cat(
        "<eye_multimodal_m4_fit>\n",
        "  persons: ", length(x$data$person_levels), "\n",
        "  items: ", length(x$data$item_levels), "\n",
        "  sequences: ", x$data$sequence$n_sequence, "\n",
        "  transitions: ", x$data$sequence$n_transition, "\n",
        "  states: ", x$spec$n_states, if (x$spec$n_states == 1L) " (formal null)" else "", "\n",
        "  state channels: ", paste(x$spec$state_channels, collapse = ", "), "\n",
        "  transition structure: ", x$spec$transition_structure, "\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        "  interpretation: statistical response-process states; uncertainty must be retained\n",
        sep = ""
    )
    invisible(x)
}

summary.eye_multimodal_m4_fit <- function(object, ...) {
    vars <- c(
        "mu_item", "sigma_person", "sigma_item", "corr_person", "corr_item",
        "nu", "s", "sigma_pupil", "gamma_pupil",
        "delta_rt", "delta_gaze", "delta_pupil",
        "init_intercept", "trans_intercept",
        "init_trait", "trans_trait"
    )
    tab <- object$fit$summary(variables = vars)
    states <- multimodal_m4_state_diagnostics(object)
    structure(
        list(
            model = "M4",
            summary = tab,
            state = states,
            audit = audit_multimodal_m4_identifiability(object),
            provenance = object$provenance,
            interpretation = object$interpretation
        ),
        class = "summary.eye_multimodal_m4_fit"
    )
}

print.summary.eye_multimodal_m4_fit <- function(x, ...) {
    cat(
        "<summary.eye_multimodal_m4_fit>\n",
        "  model: M4\n",
        "  states: ", nrow(x$state$occupancy), "\n",
        "  mean assignment entropy: ", format(x$state$summary$mean_entropy, digits = 4), "\n",
        "  boundary: state detection is not construct validation\n\n",
        sep = ""
    )
    print(x$summary, row.names = FALSE)
    invisible(x)
}

coef.eye_multimodal_m4_fit <- function(object, ...) {
    object$fit$summary(
        variables = c(
            "mu_item", "sigma_person", "sigma_item", "corr_person", "corr_item",
            "delta_rt", "delta_gaze", "delta_pupil",
            "init_intercept", "trans_intercept", "init_trait", "trans_trait"
        )
    )
}

.ep11_m4_draw_mean <- function(object, variable) {
    z <- posterior::as_draws_matrix(object$fit$draws(variables = variable))
    colMeans(z)
}

.ep11_m4_indexed_mean <- function(object, variable, dims) {
    vals <- .ep11_m4_draw_mean(object, variable)
    out <- array(NA_real_, dim = dims)
    nms <- names(vals)
    if (is.null(nms)) nms <- colnames(posterior::as_draws_matrix(object$fit$draws(variables = variable)))
    for (j in seq_along(vals)) {
        idx_txt <- sub(paste0("^", variable, "\\["), "", nms[[j]])
        idx_txt <- sub("\\]$", "", idx_txt)
        idx <- as.integer(strsplit(idx_txt, ",", fixed = TRUE)[[1L]])
        if (length(idx) == length(dims) && all(idx >= 1L)) {
            out[matrix(idx, nrow = 1L)] <- vals[[j]]
        }
    }
    out
}

.ep11_m4_state_prob_matrix <- function(object) {
    if (!inherits(object, "eye_multimodal_m4_fit")) {
        stop("State probabilities require an `eye_multimodal_m4_fit`.", call. = FALSE)
    }
    .ep11_m4_indexed_mean(object, "state_prob", c(nrow(object$data$raw), object$spec$n_states))
}

.ep11_m4_entropy <- function(p) {
    p <- pmax(as.numeric(p), .Machine$double.eps)
    -sum(p * log(p))
}

fitted.eye_multimodal_m4_fit <- function(object, type = c("measurement", "state"), ...) {
    type <- match.arg(type)
    if (identical(type, "state")) {
        p <- .ep11_m4_state_prob_matrix(object)
        out <- data.frame(
            source_row = object$data$raw$source_row,
            sequence_id = object$data$raw$sequence_id,
            trial_index = object$data$raw$trial_index,
            MAP_state = max.col(p, ties.method = "first"),
            posterior_entropy = apply(p, 1L, .ep11_m4_entropy),
            stringsAsFactors = FALSE
        )
        for (k in seq_len(ncol(p))) out[[paste0("state_", k, "_probability")]] <- p[, k]
        return(out)
    }

    d <- object$data$raw
    theta <- .ep11_m4_draw_mean(object, "theta")
    tau <- .ep11_m4_draw_mean(object, "tau")
    omega <- .ep11_m4_draw_mean(object, "omega")
    rho <- .ep11_m4_draw_mean(object, "rho")
    b <- .ep11_m4_draw_mean(object, "b")
    beta <- .ep11_m4_draw_mean(object, "beta")
    m <- .ep11_m4_draw_mean(object, "m")
    kappa <- .ep11_m4_draw_mean(object, "kappa")
    gamma <- .ep11_m4_draw_mean(object, "gamma_pupil")
    delta_rt <- .ep11_m4_draw_mean(object, "delta_rt")
    delta_gaze <- .ep11_m4_draw_mean(object, "delta_gaze")
    delta_pupil <- .ep11_m4_draw_mean(object, "delta_pupil")
    p <- .ep11_m4_state_prob_matrix(object)

    j <- d$person_index
    i <- d$item_index
    expected_rt_state <- as.numeric(p %*% delta_rt)
    expected_gaze_state <- as.numeric(p %*% delta_gaze)
    expected_pupil_state <- as.numeric(p %*% delta_pupil)
    X <- object$data$nuisance_matrix
    use <- as.numeric(object$stan_data$use_pupil_covariate)
    nuisance <- as.numeric(X %*% (gamma * use))
    pupil_model <- kappa[i] + rho[j] + expected_pupil_state + nuisance
    pupil_raw <- if (identical(object$data$pupil_transform$mode, "z")) {
        object$data$pupil_transform$center + object$data$pupil_transform$scale * pupil_model
    } else {
        pupil_model
    }

    data.frame(
        source_row = d$source_row,
        response_probability = stats::plogis(theta[j] - b[i]),
        rt_median = exp(beta[i] - tau[j] + expected_rt_state),
        gaze_mean = exp(m[i] + omega[j] + expected_gaze_state),
        pupil_mean = pupil_raw,
        stringsAsFactors = FALSE
    )
}

residuals.eye_multimodal_m4_fit <- function(object, ...) {
    f <- fitted(object, type = "measurement")
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
