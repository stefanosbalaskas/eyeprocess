# eyeprocess 0.10 — M2 likelihood-faithful response + RT + gaze reference layer
#
# Scientific target:
# Man, K., Harring, J. R., & Zhan, P. (2022).
# Bridging Models of Biometric and Psychometric Assessment:
# A Three-Way Joint Modeling Approach of Item Responses,
# Response Times, and Gaze Fixation Counts.
# Applied Psychological Measurement, 46(5), 361–381.
# https://doi.org/10.1177/01466216221089344
#
# The level-1 likelihood implemented here is:
#   response: Rasch / 1PL, logit P(Y_ij=1)=theta_j-b_i
#   log RT:   Normal(beta_i-tau_j, 1/nu_i)
#   gaze:     Negative-binomial with mean exp(m_i+omega_j), shape s_i
#
# Person effects (theta, tau, omega) and item effects (b, beta, m)
# are correlated through multivariate Gaussian structural layers.
#
# Interpretation boundary:
# response time, gaze fixation count, and their latent effects are
# observed/process measurement channels. They are not automatically
# "guessing", "attention", "difficulty", "engagement", "strategy",
# or any other psychological construct without external evidence.

.ep10_m2_reference <- list(
    doi = "10.1177/01466216221089344",
    model = "Man-Harring-Zhan 2022 three-way joint model",
    likelihood_fidelity = "response-Rasch + lognormal-RT + NB-fixation",
    prior_fidelity = "regularized-Stan; paper-centered option is not an exact reproduction of all published hyperpriors",
    interpretation = paste(
        "Process channels are observational measurements.",
        "Latent channel effects require study-specific construct validation."
    )
)

.ep10_m2_logit <- function(x) {
    1 / (1 + exp(-x))
}

.ep10_m2_status <- function() {
    ok_cmdstanr <- requireNamespace("cmdstanr", quietly = TRUE)
    ok_posterior <- requireNamespace("posterior", quietly = TRUE)
    ok_loo <- requireNamespace("loo", quietly = TRUE)
    cmdstan_path <- ""

    if (ok_cmdstanr) {
        cmdstan_path <- tryCatch(
            cmdstanr::cmdstan_path(),
            error = function(e) ""
        )
    }

    list(
        cmdstanr = ok_cmdstanr,
        cmdstan = nzchar(cmdstan_path) && dir.exists(cmdstan_path),
        posterior = ok_posterior,
        loo = ok_loo,
        ready = ok_cmdstanr &&
            nzchar(cmdstan_path) &&
            dir.exists(cmdstan_path) &&
            ok_posterior
    )
}

.ep10_m2_require_backend <- function(require_loo = FALSE) {
    s <- .ep10_m2_status()

    if (!isTRUE(s$cmdstanr)) {
        stop(
            "The M2 reference estimator requires the optional package `cmdstanr`; no fallback estimator is substituted.",
            call. = FALSE
        )
    }

    if (!isTRUE(s$cmdstan)) {
        stop(
            "CmdStan is not configured for cmdstanr. Install/configure CmdStan before fitting the M2 reference model.",
            call. = FALSE
        )
    }

    if (!isTRUE(s$posterior)) {
        stop(
            "The M2 reference estimator requires the optional package `posterior`.",
            call. = FALSE
        )
    }

    if (isTRUE(require_loo) && !isTRUE(s$loo)) {
        stop(
            "Response-target information comparison requires the optional package `loo`.",
            call. = FALSE
        )
    }

    invisible(s)
}

.ep10_m2_stan_file <- function(model = c("M0", "M1", "M2")) {
    model <- match.arg(model)

    fname <- switch(
        model,
        M0 = "m0-response-reference-0-10.stan",
        M1 = "m1-response-rt-reference-0-10.stan",
        M2 = "m2-man2022-response-rt-gaze-0-10.stan"
    )

    installed <- system.file(
        "stan",
        fname,
        package = "eyeprocess"
    )

    if (nzchar(installed) && file.exists(installed)) {
        return(installed)
    }

    candidates <- c(
        file.path("inst", "stan", fname),
        file.path(getwd(), "inst", "stan", fname)
    )

    hit <- candidates[file.exists(candidates)]

    if (!length(hit)) {
        stop(
            "Cannot locate the eyeprocess 0.10 Stan program: ",
            fname,
            call. = FALSE
        )
    }

    normalizePath(hit[[1L]], winslash = "/", mustWork = TRUE)
}

.ep10_m2_as_data <- function(
    x,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze"
) {
    if (inherits(x, "eye_multimodal_m2_simulation")) {
        x <- x$data
    } else if (inherits(x, "eye_multimodal_m2_fit")) {
        x <- x$data$raw
    } else if (
        inherits(x, "eye_multimodal_measurement") &&
        is.list(x) &&
        is.data.frame(x$data)
    ) {
        x <- x$data
    } else if (
        is.list(x) &&
        !is.data.frame(x) &&
        is.data.frame(x$data)
    ) {
        x <- x$data
    }

    if (!is.data.frame(x)) {
        stop(
            "`x` must be a data frame, an eye_multimodal_m2_simulation, or an eyeprocess multimodal measurement object containing a data frame.",
            call. = FALSE
        )
    }

    required <- c(person, item, response, rt, gaze)
    absent <- setdiff(required, names(x))

    if (length(absent)) {
        stop(
            "M2 data are missing required columns: ",
            paste(absent, collapse = ", "),
            call. = FALSE
        )
    }

    d <- data.frame(
        person_id = as.character(x[[person]]),
        item_id = as.character(x[[item]]),
        response = x[[response]],
        rt = x[[rt]],
        gaze = x[[gaze]],
        source_row = seq_len(nrow(x)),
        stringsAsFactors = FALSE
    )

    if (
        anyNA(d$person_id) ||
        any(!nzchar(d$person_id)) ||
        anyNA(d$item_id) ||
        any(!nzchar(d$item_id))
    ) {
        stop(
            "Person and item identifiers must be non-missing and non-empty.",
            call. = FALSE
        )
    }

    key <- paste(d$person_id, d$item_id, sep = "\r")

    if (anyDuplicated(key)) {
        stop(
            "M2 reference data require at most one row per person-item key.",
            call. = FALSE
        )
    }

    observed_response <- !is.na(d$response)

    if (any(observed_response)) {
        y <- suppressWarnings(as.numeric(d$response[observed_response]))

        if (
            any(!is.finite(y)) ||
            any(!y %in% c(0, 1))
        ) {
            stop(
                "Observed responses must be coded 0/1.",
                call. = FALSE
            )
        }

        d$response[observed_response] <- y
    }

    observed_rt <- !is.na(d$rt)

    if (any(observed_rt)) {
        z <- suppressWarnings(as.numeric(d$rt[observed_rt]))

        if (
            any(!is.finite(z)) ||
            any(z <= 0)
        ) {
            stop(
                "Observed response times must be finite and strictly positive.",
                call. = FALSE
            )
        }

        d$rt[observed_rt] <- z
    }

    observed_gaze <- !is.na(d$gaze)

    if (any(observed_gaze)) {
        g <- suppressWarnings(as.numeric(d$gaze[observed_gaze]))

        if (
            any(!is.finite(g)) ||
            any(g < 0) ||
            any(abs(g - round(g)) > sqrt(.Machine$double.eps))
        ) {
            stop(
                "Observed gaze fixation counts must be finite non-negative integers.",
                call. = FALSE
            )
        }

        d$gaze[observed_gaze] <- as.integer(round(g))
    }

    person_levels <- sort(unique(d$person_id))
    item_levels <- sort(unique(d$item_id))

    d$person_index <- match(d$person_id, person_levels)
    d$item_index <- match(d$item_id, item_levels)

    list(
        raw = d,
        person_levels = person_levels,
        item_levels = item_levels,
        observed = list(
            response = observed_response,
            rt = observed_rt,
            gaze = observed_gaze
        )
    )
}

.ep10_m2_to_stan <- function(data, model = c("M0", "M1", "M2"), prior_profile = c("regularized", "paper_centered")) {
    model <- match.arg(model)
    prior_profile <- match.arg(prior_profile)

    d <- data$raw
    rr <- which(data$observed$response)
    tr <- which(data$observed$rt)
    gr <- which(data$observed$gaze)

    if (!length(rr)) {
        stop(
            "At least one observed response is required.",
            call. = FALSE
        )
    }

    if (model %in% c("M1", "M2") && !length(tr)) {
        stop(
            model,
            " requires observed response times.",
            call. = FALSE
        )
    }

    if (identical(model, "M2") && !length(gr)) {
        stop(
            "M2 requires observed gaze fixation counts.",
            call. = FALSE
        )
    }

    base <- list(
        J = length(data$person_levels),
        I = length(data$item_levels),
        N_response = length(rr),
        person_response = as.integer(d$person_index[rr]),
        item_response = as.integer(d$item_index[rr]),
        y_response = as.integer(d$response[rr]),
        prior_profile = if (identical(prior_profile, "regularized")) 1L else 2L
    )

    if (model %in% c("M1", "M2")) {
        base$N_rt <- length(tr)
        base$person_rt <- as.integer(d$person_index[tr])
        base$item_rt <- as.integer(d$item_index[tr])
        base$log_rt <- log(as.numeric(d$rt[tr]))
    }

    if (identical(model, "M2")) {
        base$N_gaze <- length(gr)
        base$person_gaze <- as.integer(d$person_index[gr])
        base$item_gaze <- as.integer(d$item_index[gr])
        base$gaze <- as.integer(d$gaze[gr])
    }

    attr(base, "row_index") <- list(
        response = rr,
        rt = tr,
        gaze = gr
    )

    base
}

#' M2 response + RT + gaze reference specification
#'
#' Creates a thin M2 specialization of the established
#' [multimodal_irt_spec()] / `irt_model_spec()` architecture. The
#' measurement likelihood follows Man, Harring, and Zhan (2022):
#' Rasch response, lognormal response time, and negative-binomial
#' gaze-fixation counts. Priors are implemented in Stan using either
#' a regularized profile or a paper-centered profile; the latter is
#' not claimed to reproduce every published hyperprior exactly.
#'
#' @param backend Currently `"cmdstanr"` only.
#' @param prior_profile `"regularized"` or `"paper_centered"`.
#' @param missingness Currently `"ignorable"` only. Channel-specific
#'   missing observations contribute no level-1 likelihood term.
#' @return An `eye_multimodal_m2_spec`, inheriting the established
#'   `eye_multimodal_irt_spec` and `eye_irt_model_spec` classes.
#' @export
multimodal_m2_spec <- function(
    backend = "cmdstanr",
    prior_profile = c("regularized", "paper_centered"),
    missingness = "ignorable"
) {
    if (!identical(backend, "cmdstanr")) {
        stop(
            "The M2 likelihood-faithful reference implementation currently supports only `cmdstanr`; no fallback is substituted.",
            call. = FALSE
        )
    }

    if (!identical(missingness, "ignorable")) {
        stop(
            "The M2 reference likelihood currently supports ignorable channel missingness only. MNAR/shared-parameter extensions remain a separate validation problem.",
            call. = FALSE
        )
    }

    prior_profile <- match.arg(prior_profile)

    response_channel <- irt_response_channel(
        family = "rasch",
        latent = "ability"
    )

    rt_channel <- irt_rt_channel(
        family = "lognormal",
        latent = "speed"
    )

    gaze_channel <- irt_count_channel(
        family = "negative_binomial",
        latent = "gaze_process"
    )

    out <- multimodal_irt_spec(
        response = response_channel,
        rt = rt_channel,
        gaze = gaze_channel,
        model = "M2",
        backend = "cmdstanr",
        identification = list(
            person_latent_means = c(
                ability = 0,
                speed = 0,
                gaze_process = 0
            ),
            response_discrimination = 1,
            rt_person_loading = -1,
            gaze_person_loading = 1,
            item_effects = "hierarchical correlated location parameters"
        ),
        priors = list(
            profile = prior_profile,
            note = .ep10_m2_reference$prior_fidelity
        )
    )

    out$reference <- .ep10_m2_reference
    out$missingness <- missingness
    out$fidelity <- list(
        response = "Rasch / 1PL",
        response_time = "lognormal with item time discrimination",
        gaze = "negative-binomial fixation count",
        person_structure = "correlated ability-speed-gaze effects",
        item_structure = "correlated difficulty-time-intensity-gaze-intensity effects",
        priors = .ep10_m2_reference$prior_fidelity
    )
    out$lifecycle_status <- "experimental"

    class(out) <- unique(
        c(
            "eye_multimodal_m2_spec",
            class(out)
        )
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_spec <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_spec>\n",
        "  model: M2 response + RT + gaze\n",
        "  backend: ", x$backend, "\n",
        "  likelihood: Rasch + lognormal RT + negative-binomial gaze\n",
        "  reference DOI: ", x$reference$doi, "\n",
        "  prior profile: ", x$priors$profile, "\n",
        "  missingness: ", x$missingness, "\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        "  boundary: process channels are observations, not automatic psychological constructs\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m2_compile <- function(model = c("M0", "M1", "M2"), quiet = TRUE) {
    model <- match.arg(model)
    .ep10_m2_require_backend()

    cmdstanr::cmdstan_model(
        .ep10_m2_stan_file(model),
        quiet = quiet
    )
}

.ep10_m2_fit_reference <- function(
    x,
    model = c("M0", "M1", "M2"),
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    prior_profile = c("regularized", "paper_centered"),
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    seed = 20260814L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    quiet_compile = TRUE
) {
    model <- match.arg(model)
    prior_profile <- match.arg(prior_profile)
    .ep10_m2_require_backend()

    data <- .ep10_m2_as_data(
        x,
        person = person,
        item = item,
        response = response,
        rt = rt,
        gaze = gaze
    )

    audit <- audit_multimodal_m2_identifiability(
        data$raw,
        person = "person_id",
        item = "item_id",
        response = "response",
        rt = "rt",
        gaze = "gaze",
        model = model
    )

    if (!isTRUE(audit$supported)) {
        stop(
            "M2 structural/data identifiability audit did not support fitting. Inspect `audit_multimodal_m2_identifiability()` before estimation.",
            call. = FALSE
        )
    }

    stan_data <- .ep10_m2_to_stan(
        data,
        model = model,
        prior_profile = prior_profile
    )

    stan_model <- .ep10_m2_compile(
        model,
        quiet = quiet_compile
    )

    fit <- stan_model$sample(
        data = stan_data,
        seed = as.integer(seed),
        chains = as.integer(chains),
        parallel_chains = as.integer(parallel_chains),
        iter_warmup = as.integer(iter_warmup),
        iter_sampling = as.integer(iter_sampling),
        adapt_delta = adapt_delta,
        max_treedepth = as.integer(max_treedepth),
        refresh = as.integer(refresh)
    )

    spec <- if (identical(model, "M2")) {
        multimodal_m2_spec(
            prior_profile = prior_profile
        )
    } else if (identical(model, "M1")) {
        multimodal_irt_spec(
            response = irt_response_channel(
                family = "rasch",
                latent = "ability"
            ),
            rt = irt_rt_channel(
                family = "lognormal",
                latent = "speed"
            ),
            model = "M1",
            backend = "cmdstanr",
            identification = list(
                person_latent_means = c(
                    ability = 0,
                    speed = 0
                ),
                response_discrimination = 1,
                rt_person_loading = -1
            ),
            priors = list(profile = prior_profile)
        )
    } else {
        multimodal_irt_spec(
            response = irt_response_channel(
                family = "rasch",
                latent = "ability"
            ),
            model = "M0",
            backend = "cmdstanr",
            identification = list(
                person_latent_mean = 0,
                response_discrimination = 1
            ),
            priors = list(profile = prior_profile)
        )
    }

    out <- list(
        model = model,
        fit = fit,
        data = data,
        stan_data = stan_data,
        spec = spec,
        audit = audit,
        reference = .ep10_m2_reference,
        prior_profile = prior_profile,
        seed = as.integer(seed),
        sampling_controls = list(
            chains = as.integer(chains),
            parallel_chains = as.integer(parallel_chains),
            iter_warmup = as.integer(iter_warmup),
            iter_sampling = as.integer(iter_sampling),
            adapt_delta = adapt_delta,
            max_treedepth = as.integer(max_treedepth)
        ),
        lifecycle_status = if (identical(model, "M2")) "gated" else "experimental",
        interpretation = .ep10_m2_reference$interpretation,
        call = match.call()
    )

    class(out) <- c(
        "eye_multimodal_m2_fit",
        "list"
    )

    out
}

#' Fit the M2 response + RT + gaze reference model
#'
#' Fits the likelihood-faithful M2 reference model with CmdStanR.
#' There is no silent fallback. Missing observations are omitted from
#' their channel-specific likelihood under an explicit ignorable
#' missingness assumption; the person and item structural layers remain
#' joint across the observed channels.
#'
#' @param x Data frame, `eye_multimodal_m2_simulation`, or compatible
#'   eyeprocess multimodal measurement object.
#' @param person,item,response,rt,gaze Column names.
#' @param prior_profile Prior profile.
#' @param chains,parallel_chains,iter_warmup,iter_sampling CmdStan sampling controls.
#' @param seed Reproducibility seed.
#' @param adapt_delta,max_treedepth,refresh CmdStan controls.
#' @param quiet_compile Suppress CmdStan compilation messages.
#' @return An `eye_multimodal_m2_fit`.
#' @export
fit_multimodal_m2 <- function(
    x,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    prior_profile = c("regularized", "paper_centered"),
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    seed = 20260814L,
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 100L,
    quiet_compile = TRUE
) {
    .ep10_m2_fit_reference(
        x = x,
        model = "M2",
        person = person,
        item = item,
        response = response,
        rt = rt,
        gaze = gaze,
        prior_profile = match.arg(prior_profile),
        chains = chains,
        parallel_chains = parallel_chains,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling,
        seed = seed,
        adapt_delta = adapt_delta,
        max_treedepth = max_treedepth,
        refresh = refresh,
        quiet_compile = quiet_compile
    )
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_fit <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_fit>\n",
        "  model: ", x$model, "\n",
        "  persons: ", length(x$data$person_levels), "\n",
        "  items: ", length(x$data$item_levels), "\n",
        "  observed response: ", sum(x$data$observed$response), "\n",
        "  observed RT: ", sum(x$data$observed$rt), "\n",
        "  observed gaze: ", sum(x$data$observed$gaze), "\n",
        "  prior profile: ", x$prior_profile, "\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        "  reference DOI: ", x$reference$doi, "\n",
        sep = ""
    )
    invisible(x)
}

# S3 method registered in the hand-maintained NAMESPACE.
summary.eye_multimodal_m2_fit <- function(object, ...) {
    variables <- switch(
        object$model,
        M0 = c("mu_b", "sigma_theta", "sigma_b"),
        M1 = c(
            "mu_item",
            "sigma_person",
            "sigma_item",
            "corr_person",
            "corr_item",
            "nu"
        ),
        M2 = c(
            "mu_item",
            "sigma_person",
            "sigma_item",
            "corr_person",
            "corr_item",
            "nu",
            "s"
        )
    )

    tab <- object$fit$summary(
        variables = variables,
        probs = c(0.025, 0.5, 0.975)
    )

    structure(
        list(
            model = object$model,
            summary = tab,
            audit = object$audit,
            reference = object$reference,
            lifecycle_status = object$lifecycle_status
        ),
        class = "summary.eye_multimodal_m2_fit"
    )
}

# S3 method registered in the hand-maintained NAMESPACE.
print.summary.eye_multimodal_m2_fit <- function(x, ...) {
    cat(
        "<summary.eye_multimodal_m2_fit>\n",
        "  model: ", x$model, "\n",
        "  lifecycle: ", x$lifecycle_status, "\n",
        "  reference DOI: ", x$reference$doi, "\n\n",
        sep = ""
    )
    print(x$summary, row.names = FALSE)
    invisible(x)
}

.ep10_m2_draws_matrix <- function(x, variable) {
    if (!inherits(x, "eye_multimodal_m2_fit")) {
        stop(
            "`x` must be an eye_multimodal_m2_fit.",
            call. = FALSE
        )
    }

    posterior::as_draws_matrix(
        x$fit$draws(
            variables = variable
        )
    )
}

.ep10_m2_parameter_summary <- function(x, variables) {
    x$fit$summary(
        variables = variables,
        probs = c(0.025, 0.5, 0.975)
    )
}
