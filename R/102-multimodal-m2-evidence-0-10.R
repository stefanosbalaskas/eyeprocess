# eyeprocess 0.10 — M2 evidence, identifiability, PPC, ablation, information

.ep10_m2_union_find_components <- function(person, item) {
    p <- paste0("P:", person)
    i <- paste0("I:", item)
    nodes <- unique(c(p, i))
    parent <- seq_along(nodes)
    names(parent) <- nodes

    root <- function(k) {
        while (parent[[k]] != k) {
            parent[[k]] <<- parent[[parent[[k]]]]
            k <- parent[[k]]
        }
        k
    }

    unite <- function(a, b) {
        ia <- unname(parent[[a]])
        ib <- unname(parent[[b]])
        ra <- root(ia)
        rb <- root(ib)
        if (ra != rb) {
            parent[[rb]] <<- ra
        }
        invisible(NULL)
    }

    for (k in seq_along(person)) {
        unite(
            paste0("P:", person[[k]]),
            paste0("I:", item[[k]])
        )
    }

    roots <- vapply(
        seq_along(nodes),
        root,
        integer(1)
    )

    length(unique(roots))
}

.ep10_m2_channel_design <- function(d, observed) {
    z <- d[observed, , drop = FALSE]

    if (!nrow(z)) {
        return(
            data.frame(
                n_observed = 0L,
                n_person = 0L,
                n_item = 0L,
                components = NA_integer_,
                min_person_obs = 0L,
                min_item_obs = 0L,
                stringsAsFactors = FALSE
            )
        )
    }

    po <- table(z$person_id)
    io <- table(z$item_id)

    data.frame(
        n_observed = nrow(z),
        n_person = length(po),
        n_item = length(io),
        components = .ep10_m2_union_find_components(
            z$person_id,
            z$item_id
        ),
        min_person_obs = min(po),
        min_item_obs = min(io),
        stringsAsFactors = FALSE
    )
}

#' Audit structural and data identifiability for M0-M2
#'
#' This is a conservative pre-fit audit. It does not prove global
#' identifiability. It verifies support, design connectivity, channel
#' coverage, response variation, and the explicit scale constraints
#' used by the reference likelihoods.
#'
#' @param x Data frame or compatible M2 object.
#' @param person,item,response,rt,gaze Column names.
#' @param model `"M0"`, `"M1"`, or `"M2"`.
#' @param min_persons,min_items Conservative design thresholds.
#' @return An `eye_multimodal_m2_identifiability` object.
#' @export
audit_multimodal_m2_identifiability <- function(
    x,
    person = "person_id",
    item = "item_id",
    response = "response",
    rt = "rt",
    gaze = "gaze",
    model = c("M2", "M1", "M0"),
    min_persons = 20L,
    min_items = 5L
) {
    model <- match.arg(model)

    data <- .ep10_m2_as_data(
        x,
        person = person,
        item = item,
        response = response,
        rt = rt,
        gaze = gaze
    )

    d <- data$raw

    response_design <- .ep10_m2_channel_design(
        d,
        data$observed$response
    )

    rt_design <- .ep10_m2_channel_design(
        d,
        data$observed$rt
    )

    gaze_design <- .ep10_m2_channel_design(
        d,
        data$observed$gaze
    )

    y <- d$response[data$observed$response]

    response_variation <- (
        length(y) > 0L &&
        length(unique(y)) == 2L
    )

    item_response_variation <- if (length(y)) {
        by_item <- split(
            d$response[data$observed$response],
            d$item_id[data$observed$response]
        )
        sum(
            vapply(
                by_item,
                function(v) length(unique(v)) == 2L,
                logical(1)
            )
        )
    } else {
        0L
    }

    person_response_variation <- if (length(y)) {
        by_person <- split(
            d$response[data$observed$response],
            d$person_id[data$observed$response]
        )
        sum(
            vapply(
                by_person,
                function(v) length(unique(v)) == 2L,
                logical(1)
            )
        )
    } else {
        0L
    }

    g <- d$gaze[data$observed$gaze]

    gaze_variation <- (
        length(g) > 1L &&
        stats::var(as.numeric(g)) > 0
    )

    rt_values <- d$rt[data$observed$rt]

    rt_variation <- (
        length(rt_values) > 1L &&
        stats::var(log(as.numeric(rt_values))) > 0
    )

    required_designs <- list(response = response_design)

    if (model %in% c("M1", "M2")) {
        required_designs$rt <- rt_design
    }

    if (identical(model, "M2")) {
        required_designs$gaze <- gaze_design
    }

    connected <- all(
        vapply(
            required_designs,
            function(z) {
                is.finite(z$components[[1L]]) &&
                    z$components[[1L]] == 1L
            },
            logical(1)
        )
    )

    n_person_global <- length(data$person_levels)
    n_item_global <- length(data$item_levels)

    size_ok <- (
        n_person_global >= as.integer(min_persons) &&
        n_item_global >= as.integer(min_items)
    )

    channel_presence <- (
        response_design$n_observed[[1L]] > 0L &&
        (!model %in% c("M1", "M2") || rt_design$n_observed[[1L]] > 0L) &&
        (!identical(model, "M2") || gaze_design$n_observed[[1L]] > 0L)
    )

    variation_ok <- (
        response_variation &&
        (!model %in% c("M1", "M2") || rt_variation) &&
        (!identical(model, "M2") || gaze_variation)
    )

    supported <- (
        size_ok &&
        channel_presence &&
        connected &&
        variation_ok
    )

    checks <- data.frame(
        check = c(
            "sample_size",
            "channel_presence",
            "design_connected",
            "response_variation",
            if (model %in% c("M1", "M2")) "rt_variation" else NULL,
            if (identical(model, "M2")) "gaze_variation" else NULL,
            "person_latent_means_fixed_zero",
            "response_discrimination_fixed_one",
            if (model %in% c("M1", "M2")) "rt_person_loading_fixed_minus_one" else NULL,
            if (identical(model, "M2")) "gaze_person_loading_fixed_one" else NULL
        ),
        pass = c(
            size_ok,
            channel_presence,
            connected,
            response_variation,
            if (model %in% c("M1", "M2")) rt_variation else NULL,
            if (identical(model, "M2")) gaze_variation else NULL,
            TRUE,
            TRUE,
            if (model %in% c("M1", "M2")) TRUE else NULL,
            if (identical(model, "M2")) TRUE else NULL
        ),
        stringsAsFactors = FALSE
    )

    out <- list(
        model = model,
        supported = supported,
        checks = checks,
        n_person = n_person_global,
        n_item = n_item_global,
        response_design = response_design,
        rt_design = rt_design,
        gaze_design = gaze_design,
        response_variable_items = item_response_variation,
        response_variable_persons = person_response_variation,
        missing_fraction = c(
            response = mean(!data$observed$response),
            rt = mean(!data$observed$rt),
            gaze = mean(!data$observed$gaze)
        ),
        statement = paste(
            "This audit is a conservative structural/data-support screen.",
            "It does not establish global identifiability, construct validity,",
            "or robustness to MNAR channel missingness."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_identifiability",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_identifiability <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_identifiability>\n",
        "  model: ", x$model, "\n",
        "  persons: ", x$n_person, "\n",
        "  items: ", x$n_item, "\n",
        "  supported: ", x$supported, "\n",
        "  missing fractions: ",
        paste(
            names(x$missing_fraction),
            sprintf("%.3f", x$missing_fraction),
            sep = "=",
            collapse = ", "
        ),
        "\n",
        "  boundary: ", x$statement, "\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m2_extract_item_draws <- function(fit, variable, I) {
    m <- .ep10_m2_draws_matrix(fit, variable)

    expected <- paste0(
        variable,
        "[",
        seq_len(I),
        "]"
    )

    absent <- setdiff(expected, colnames(m))

    if (length(absent)) {
        stop(
            "Posterior draws are missing expected variables: ",
            paste(absent, collapse = ", "),
            call. = FALSE
        )
    }

    m[, expected, drop = FALSE]
}

.ep10_m2_ppp_channel <- function(fit, observed_name, replicate_name, channel) {
    I <- length(fit$data$item_levels)

    obs <- .ep10_m2_extract_item_draws(
        fit,
        observed_name,
        I
    )

    rep <- .ep10_m2_extract_item_draws(
        fit,
        replicate_name,
        I
    )

    ppp <- colMeans(
        rep >= obs
    )

    data.frame(
        item_id = fit$data$item_levels,
        channel = channel,
        ppp = as.numeric(ppp),
        lower_tail = ppp < 0.05,
        upper_tail = ppp > 0.95,
        flagged = ppp < 0.05 | ppp > 0.95,
        stringsAsFactors = FALSE
    )
}

#' Posterior predictive checks for the M2 three-way model
#'
#' Reproduces the channel-specific discrepancy logic used in the
#' published three-way framework: response W, response-time L, and
#' fixation-count M residual statistics, aggregated by item.
#'
#' @param x An `eye_multimodal_m2_fit` with `model == "M2"`.
#' @return An `eye_multimodal_m2_ppc`.
#' @export
multimodal_m2_ppc <- function(x) {
    if (
        !inherits(x, "eye_multimodal_m2_fit") ||
        !identical(x$model, "M2")
    ) {
        stop(
            "`x` must be an M2 `eye_multimodal_m2_fit`.",
            call. = FALSE
        )
    }

    .ep10_m2_require_backend()

    response <- .ep10_m2_ppp_channel(
        x,
        "W_obs",
        "W_rep",
        "response"
    )

    rt <- .ep10_m2_ppp_channel(
        x,
        "L_obs",
        "L_rep",
        "rt"
    )

    gaze <- .ep10_m2_ppp_channel(
        x,
        "M_obs",
        "M_rep",
        "gaze"
    )

    tab <- rbind(
        response,
        rt,
        gaze
    )

    out <- list(
        table = tab,
        flag_rate = mean(tab$flagged),
        reference = x$reference,
        interpretation = paste(
            "Posterior predictive p-values diagnose model-data discrepancy.",
            "They do not validate psychological interpretations of the channels."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_ppc",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_ppc <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_ppc>\n",
        "  item-channel checks: ", nrow(x$table), "\n",
        "  tail-flag rate: ", sprintf("%.3f", x$flag_rate), "\n",
        "  reference DOI: ", x$reference$doi, "\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m2_sampler_audit <- function(x) {
    variables <- switch(
        x$model,
        M0 = c(
            "theta_raw",
            "sigma_theta",
            "b_raw",
            "mu_b",
            "sigma_b",
            "theta",
            "b"
        ),
        M1 = c(
            "z_person",
            "sigma_person",
            "L_person",
            "z_item",
            "mu_item",
            "sigma_item",
            "L_item",
            "nu",
            "theta",
            "tau",
            "b",
            "beta",
            "corr_person",
            "corr_item"
        ),
        M2 = c(
            "z_person",
            "sigma_person",
            "L_person",
            "z_item",
            "mu_item",
            "sigma_item",
            "L_item",
            "nu",
            "s",
            "theta",
            "tau",
            "omega",
            "b",
            "beta",
            "m",
            "corr_person",
            "corr_item"
        )
    )

    s <- x$fit$summary(
        variables = variables
    )

    finite_rhat <- s$rhat[is.finite(s$rhat)]
    finite_bulk <- s$ess_bulk[is.finite(s$ess_bulk)]
    finite_tail <- s$ess_tail[is.finite(s$ess_tail)]

    diagnostics <- tryCatch(
        posterior::as_draws_matrix(
            x$fit$sampler_diagnostics()
        ),
        error = function(e) NULL
    )

    divergences <- NA_integer_
    max_treedepth_hits <- NA_integer_

    if (!is.null(diagnostics)) {
        if ("divergent__" %in% colnames(diagnostics)) {
            divergences <- sum(
                diagnostics[, "divergent__"],
                na.rm = TRUE
            )
        }

        if (
            "treedepth__" %in% colnames(diagnostics) &&
            !is.null(x$sampling_controls$max_treedepth)
        ) {
            max_treedepth_hits <- sum(
                diagnostics[, "treedepth__"] >=
                    x$sampling_controls$max_treedepth,
                na.rm = TRUE
            )
        }
    }

    list(
        max_rhat = if (length(finite_rhat)) max(finite_rhat) else NA_real_,
        min_ess_bulk = if (length(finite_bulk)) min(finite_bulk) else NA_real_,
        min_ess_tail = if (length(finite_tail)) min(finite_tail) else NA_real_,
        divergences = divergences,
        max_treedepth_hits = max_treedepth_hits
    )
}

#' Validate an M2 fit or simulation
#'
#' For simulations, performs data/support and identifiability checks.
#' For fitted models, additionally audits MCMC diagnostics and optionally
#' channel-specific posterior predictive checks.
#'
#' @param x M2 simulation or fit.
#' @param include_ppc Include posterior predictive checks for fitted M2 models.
#' @param rhat_max Maximum acceptable R-hat.
#' @param ess_min Minimum bulk/tail ESS threshold.
#' @return An `eye_multimodal_m2_validation`.
#' @export
validate_multimodal_m2 <- function(
    x,
    include_ppc = TRUE,
    rhat_max = 1.01,
    ess_min = 200
) {
    if (inherits(x, "eye_multimodal_m2_simulation")) {
        audit <- audit_multimodal_m2_identifiability(
            x$data,
            model = "M2"
        )

        checks <- audit$checks

        out <- list(
            valid = isTRUE(audit$supported),
            model = "M2",
            type = "simulation",
            checks = checks,
            identifiability = audit,
            diagnostics = NULL,
            ppc = NULL,
            boundary = paste(
                "A successful synthetic-data validation establishes support and",
                "internal generative consistency only; it is not empirical construct validation."
            )
        )

        class(out) <- c(
            "eye_multimodal_m2_validation",
            "list"
        )

        return(out)
    }

    if (!inherits(x, "eye_multimodal_m2_fit")) {
        stop(
            "`x` must be an M2 simulation or fit.",
            call. = FALSE
        )
    }

    diagnostics <- .ep10_m2_sampler_audit(x)

    diag_checks <- data.frame(
        check = c(
            "max_rhat",
            "min_ess_bulk",
            "min_ess_tail",
            "divergences",
            "max_treedepth_hits"
        ),
        value = c(
            diagnostics$max_rhat,
            diagnostics$min_ess_bulk,
            diagnostics$min_ess_tail,
            diagnostics$divergences,
            diagnostics$max_treedepth_hits
        ),
        pass = c(
            is.finite(diagnostics$max_rhat) &&
                diagnostics$max_rhat <= rhat_max,
            is.finite(diagnostics$min_ess_bulk) &&
                diagnostics$min_ess_bulk >= ess_min,
            is.finite(diagnostics$min_ess_tail) &&
                diagnostics$min_ess_tail >= ess_min,
            is.na(diagnostics$divergences) ||
                diagnostics$divergences == 0L,
            is.na(diagnostics$max_treedepth_hits) ||
                diagnostics$max_treedepth_hits == 0L
        ),
        stringsAsFactors = FALSE
    )

    ppc <- NULL
    ppc_ok <- TRUE

    if (
        isTRUE(include_ppc) &&
        identical(x$model, "M2")
    ) {
        ppc <- multimodal_m2_ppc(x)
        ppc_ok <- ppc$flag_rate <= 0.20
    }

    ident <- x$audit

    all_checks <- rbind(
        data.frame(
            check = paste0("ident_", ident$checks$check),
            value = NA_real_,
            pass = ident$checks$pass,
            stringsAsFactors = FALSE
        ),
        diag_checks,
        data.frame(
            check = "ppc_tail_flag_rate_le_0.20",
            value = if (is.null(ppc)) NA_real_ else ppc$flag_rate,
            pass = ppc_ok,
            stringsAsFactors = FALSE
        )
    )

    valid <- all(all_checks$pass)

    out <- list(
        valid = valid,
        model = x$model,
        type = "fit",
        checks = all_checks,
        identifiability = ident,
        diagnostics = diagnostics,
        ppc = ppc,
        boundary = paste(
            "Computational diagnostics and PPC assess estimation and model-data fit.",
            "They do not establish empirical construct validity or causal interpretation."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_validation",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_validation <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_validation>\n",
        "  type: ", x$type, "\n",
        "  model: ", x$model, "\n",
        "  valid: ", x$valid, "\n",
        "  checks passed: ", sum(x$checks$pass), "/", nrow(x$checks), "\n",
        "  boundary: ", x$boundary, "\n",
        sep = ""
    )
    invisible(x)
}

#' Fit M0, M1, and M2 as a response-target ablation sequence
#'
#' Fits response-only (M0), response+RT (M1), and response+RT+gaze
#' (M2) with compatible hierarchical Stan implementations. The sequence
#' is designed for response-target comparison rather than for asserting
#' that information from distinct channels is algebraically additive.
#'
#' @param x Data accepted by [fit_multimodal_m2()].
#' @param ... Sampling arguments forwarded to the internal reference fitters.
#' @return An `eye_multimodal_m2_ablation`.
#' @export
multimodal_m2_ablation <- function(x, ...) {
    fits <- list(
        M0 = .ep10_m2_fit_reference(
            x,
            model = "M0",
            ...
        ),
        M1 = .ep10_m2_fit_reference(
            x,
            model = "M1",
            ...
        ),
        M2 = .ep10_m2_fit_reference(
            x,
            model = "M2",
            ...
        )
    )

    out <- list(
        fits = fits,
        models = names(fits),
        target = "response",
        interpretation = paste(
            "M0/M1/M2 are compared on the same response target.",
            "Incremental information is not assumed to be additive across channels."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_ablation",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_ablation <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_ablation>\n",
        "  models: ", paste(x$models, collapse = " -> "), "\n",
        "  target: ", x$target, "\n",
        "  boundary: ", x$interpretation, "\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m2_response_loo <- function(fit) {
    .ep10_m2_require_backend(require_loo = TRUE)

    ll <- fit$fit$draws(
        variables = "log_lik_response",
        format = "draws_array"
    )

    ll <- as.array(ll)

    r_eff <- loo::relative_eff(
        exp(ll)
    )

    loo::loo(
        ll,
        r_eff = r_eff
    )
}

.ep10_m2_theta_variance <- function(fit) {
    theta <- .ep10_m2_draws_matrix(
        fit,
        "theta"
    )

    apply(
        theta,
        2L,
        stats::var
    )
}

#' Quantify response-target process information in the M0-M2 sequence
#'
#' Computes two complementary quantities on a common response target:
#' response-target PSIS-LOO ELPD and posterior variance of person ability.
#' This avoids simply summing channel Fisher information under a joint
#' correlated model.
#'
#' @param x An `eye_multimodal_m2_ablation`.
#' @return An `eye_multimodal_m2_information`.
#' @export
multimodal_m2_process_information <- function(x) {
    if (!inherits(x, "eye_multimodal_m2_ablation")) {
        stop(
            "`x` must be created by `multimodal_m2_ablation()`.",
            call. = FALSE
        )
    }

    .ep10_m2_require_backend(require_loo = TRUE)

    loo_objects <- lapply(
        x$fits,
        .ep10_m2_response_loo
    )

    elpd <- vapply(
        loo_objects,
        function(z) {
            z$estimates["elpd_loo", "Estimate"]
        },
        numeric(1)
    )

    elpd_se <- vapply(
        loo_objects,
        function(z) {
            z$estimates["elpd_loo", "SE"]
        },
        numeric(1)
    )

    pointwise_elpd <- lapply(
        loo_objects,
        function(z) {
            as.numeric(
                z$pointwise[, "elpd_loo"]
            )
        }
    )

    m0_pointwise <- pointwise_elpd[["M0"]]

    delta_elpd_se <- vapply(
        pointwise_elpd,
        function(z) {
            if (length(z) != length(m0_pointwise)) {
                return(NA_real_)
            }

            delta <- z - m0_pointwise

            if (length(delta) < 2L) {
                return(NA_real_)
            }

            sqrt(
                length(delta) *
                    stats::var(delta)
            )
        },
        numeric(1)
    )

    pareto_k_max <- vapply(
        loo_objects,
        function(z) {
            k <- tryCatch(
                loo::pareto_k_values(z),
                error = function(e) numeric()
            )

            if (!length(k)) {
                return(NA_real_)
            }

            max(k, na.rm = TRUE)
        },
        numeric(1)
    )

    theta_var <- lapply(
        x$fits,
        .ep10_m2_theta_variance
    )

    mean_theta_var <- vapply(
        theta_var,
        mean,
        numeric(1)
    )

    m0_var <- mean_theta_var[["M0"]]

    variance_reduction <- if (
        is.finite(m0_var) &&
        m0_var > 0
    ) {
        1 - mean_theta_var / m0_var
    } else {
        rep(
            NA_real_,
            length(mean_theta_var)
        )
    }

    tab <- data.frame(
        model = names(x$fits),
        channels = c(
            "response",
            "response + RT",
            "response + RT + gaze"
        ),
        response_elpd_loo = as.numeric(elpd),
        response_elpd_se = as.numeric(elpd_se),
        delta_response_elpd_vs_M0 = as.numeric(
            elpd - elpd[["M0"]]
        ),
        delta_response_elpd_se_vs_M0 = as.numeric(delta_elpd_se),
        max_pareto_k = as.numeric(pareto_k_max),
        mean_theta_posterior_variance = as.numeric(mean_theta_var),
        theta_variance_reduction_vs_M0 = as.numeric(variance_reduction),
        stringsAsFactors = FALSE
    )

    out <- list(
        table = tab,
        loo = loo_objects,
        theta_variance = theta_var,
        target = "response cells for observed persons and items",
        interpretation = paste(
            "Positive response-target ELPD change and lower ability posterior variance",
            "can indicate added measurement information for held-out response cells among",
            "the observed person/item population under the fitted model.",
            "This is not a new-person or new-item transport estimate.",
            "Neither quantity establishes construct validity or causal value of a sensor."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_information",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_information <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_information>\n",
        "  target: ", x$target, "\n",
        "  comparison: M0 response -> M1 +RT -> M2 +gaze\n",
        "  non-additivity: explicitly retained\n\n",
        sep = ""
    )
    print(x$table, row.names = FALSE)
    invisible(x)
}

.ep10_m2_person_summaries <- function(d) {
    persons <- sort(unique(d$person_id))

    do.call(
        rbind,
        lapply(
            persons,
            function(p) {
                z <- d[d$person_id == p, , drop = FALSE]

                data.frame(
                    person_id = p,
                    response_mean = if (all(is.na(z$response))) {
                        NA_real_
                    } else {
                        mean(z$response, na.rm = TRUE)
                    },
                    log_rt_mean = if (all(is.na(z$rt))) {
                        NA_real_
                    } else {
                        mean(log(z$rt), na.rm = TRUE)
                    },
                    gaze_mean = if (all(is.na(z$gaze))) {
                        NA_real_
                    } else {
                        mean(z$gaze, na.rm = TRUE)
                    },
                    stringsAsFactors = FALSE
                )
            }
        )
    )
}

.ep10_m2_person_correlations <- function(d, label) {
    p <- .ep10_m2_person_summaries(d)

    pairs <- list(
        response_rt = c("response_mean", "log_rt_mean"),
        response_gaze = c("response_mean", "gaze_mean"),
        rt_gaze = c("log_rt_mean", "gaze_mean")
    )

    do.call(
        rbind,
        lapply(
            names(pairs),
            function(nm) {
                cols <- pairs[[nm]]
                cc <- stats::complete.cases(
                    p[, cols, drop = FALSE]
                )

                value <- if (sum(cc) >= 3L) {
                    stats::cor(
                        p[[cols[[1L]]]][cc],
                        p[[cols[[2L]]]][cc]
                    )
                } else {
                    NA_real_
                }

                data.frame(
                    dataset = label,
                    pair = nm,
                    correlation = value,
                    n = sum(cc),
                    stringsAsFactors = FALSE
                )
            }
        )
    )
}

#' Generate M2 alignment negative controls
#'
#' Generates deterministic within-item permutations that preserve each
#' item's marginal channel distribution while breaking person-level
#' alignment for gaze, RT, or response. These are falsification controls,
#' not causal interventions and not misconduct detectors.
#'
#' @param x M2-compatible data.
#' @param seed Seed for deterministic permutations.
#' @return An `eye_multimodal_m2_negative_controls`.
#' @export
multimodal_m2_negative_controls <- function(
    x,
    seed = 20260814L
) {
    data <- .ep10_m2_as_data(x)
    d <- data$raw

    set.seed(as.integer(seed))

    permute_within_item <- function(dat, column) {
        out <- dat
        ids <- split(
            seq_len(nrow(out)),
            out$item_id
        )

        for (ind in ids) {
            observed <- ind[!is.na(out[[column]][ind])]

            if (length(observed) > 1L) {
                out[[column]][observed] <-
                    sample(
                        out[[column]][observed],
                        length(observed),
                        replace = FALSE
                    )
            }
        }

        out
    }

    controls <- list(
        observed = d,
        gaze_within_item = permute_within_item(
            d,
            "gaze"
        ),
        rt_within_item = permute_within_item(
            d,
            "rt"
        ),
        response_within_item = permute_within_item(
            d,
            "response"
        )
    )

    diagnostics <- do.call(
        rbind,
        Map(
            .ep10_m2_person_correlations,
            controls,
            names(controls)
        )
    )

    provenance <- data.frame(
        control = c(
            "gaze_within_item",
            "rt_within_item",
            "response_within_item"
        ),
        changed_channel = c(
            "gaze",
            "rt",
            "response"
        ),
        preserved = "within-item marginal observed values and missingness pattern",
        broken = "person-level alignment for the named channel",
        interpretation = "falsification control; not causal and not a misconduct classifier",
        stringsAsFactors = FALSE
    )

    out <- list(
        datasets = controls,
        diagnostics = diagnostics,
        provenance = provenance,
        seed = as.integer(seed),
        interpretation = paste(
            "Negative controls test whether apparent incremental process information",
            "depends on person-level channel alignment. They do not identify a causal",
            "mechanism or label participant behavior."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_negative_controls",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_negative_controls <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_negative_controls>\n",
        "  controls: ",
        paste(
            setdiff(
                names(x$datasets),
                "observed"
            ),
            collapse = ", "
        ),
        "\n",
        "  seed: ", x$seed, "\n",
        "  boundary: ", x$interpretation, "\n",
        sep = ""
    )
    invisible(x)
}
