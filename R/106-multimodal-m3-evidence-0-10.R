# eyeprocess 0.10 - M3 evidence, identifiability, PPC, ablation, information

.ep10_m3_channel_design <- function(d, observed) {
    .ep10_m2_channel_design(d, observed)
}

.ep10_m3_vector_variation <- function(x, log_scale = FALSE) {
    x <- x[!is.na(x)]
    if (length(x) < 2L) return(FALSE)
    if (isTRUE(log_scale)) x <- log(x)
    is.finite(stats::var(as.numeric(x))) && stats::var(as.numeric(x)) > 0
}

#' Audit structural and measurement support for M3
#'
#' Performs a conservative pre-fit audit for the four-channel response, RT,
#' fixation-count and pupil reference model. The audit checks design
#' connectivity, channel coverage and variation, pupil nuisance availability,
#' blink/interpolation burden, device/session representation and explicit M3
#' identification constraints. It is a support screen, not proof of global
#' identifiability or construct validity.
#'
#' @param x M3-compatible data, simulation, fit, or measurement object.
#' @param pupil_scale Pupil transformation used by the reference likelihood.
#' @param min_persons,min_items Conservative design thresholds.
#' @param max_pupil_missing,max_blink_rate,max_interpolation_rate Warning thresholds.
#' @return An `eye_multimodal_m3_identifiability` object.
#' @export
audit_multimodal_m3_identifiability <- function(
    x,
    pupil_scale = c("z", "raw"),
    min_persons = 20L,
    min_items = 5L,
    max_pupil_missing = 0.50,
    max_blink_rate = 0.30,
    max_interpolation_rate = 0.30
) {
    pupil_scale <- match.arg(pupil_scale)
    data <- .ep10_m3_as_data(x, pupil_scale = pupil_scale)
    d <- data$raw

    designs <- lapply(data$observed, function(obs) .ep10_m3_channel_design(d, obs))
    names(designs) <- names(data$observed)

    y <- d$response[data$observed$response]
    variation <- c(
        response = length(y) > 1L && length(unique(y)) == 2L,
        rt = .ep10_m3_vector_variation(d$rt[data$observed$rt], log_scale = TRUE),
        gaze = .ep10_m3_vector_variation(d$gaze[data$observed$gaze]),
        pupil = .ep10_m3_vector_variation(d$pupil[data$observed$pupil])
    )

    connected <- vapply(
        designs,
        function(z) is.finite(z$components[[1L]]) && z$components[[1L]] == 1L,
        logical(1L)
    )
    present <- vapply(designs, function(z) z$n_observed[[1L]] > 0L, logical(1L))

    pupil_missing <- mean(!data$observed$pupil)
    blink_rate <- if (all(is.na(d$pupil_blink))) NA_real_ else mean(d$pupil_blink, na.rm = TRUE)
    interpolation_rate <- if (all(is.na(d$pupil_interpolated))) NA_real_ else mean(d$pupil_interpolated, na.rm = TRUE)

    quality_checks <- c(
        pupil_missingness = pupil_missing <= max_pupil_missing,
        blink_burden = is.na(blink_rate) || blink_rate <= max_blink_rate,
        interpolation_burden = is.na(interpolation_rate) || interpolation_rate <= max_interpolation_rate
    )

    n_person <- length(data$person_levels)
    n_item <- length(data$item_levels)
    size_ok <- n_person >= as.integer(min_persons) && n_item >= as.integer(min_items)

    pupil_by_item <- split(d$pupil[data$observed$pupil], d$item_id[data$observed$pupil])
    pupil_by_person <- split(d$pupil[data$observed$pupil], d$person_id[data$observed$pupil])
    variable_pupil_items <- sum(vapply(pupil_by_item, .ep10_m3_vector_variation, logical(1L)))
    variable_pupil_persons <- sum(vapply(pupil_by_person, .ep10_m3_vector_variation, logical(1L)))

    checks <- data.frame(
        check = c(
            "sample_size",
            paste0(names(present), "_presence"),
            paste0(names(connected), "_design_connected"),
            paste0(names(variation), "_variation"),
            names(quality_checks),
            "person_latent_means_fixed_zero",
            "response_discrimination_fixed_one",
            "rt_person_loading_fixed_minus_one",
            "gaze_person_loading_fixed_one",
            "pupil_person_loading_fixed_one",
            "pupil_nuisance_missing_values_not_imputed"
        ),
        pass = c(
            size_ok,
            unname(present),
            unname(connected),
            unname(variation),
            unname(quality_checks),
            rep(TRUE, 6L)
        ),
        stringsAsFactors = FALSE
    )

    supported <- all(c(size_ok, present, connected, variation))
    warning_free <- all(checks$pass)

    device_table <- if (all(is.na(d$device))) {
        data.frame(device = NA_character_, n = nrow(d), pupil_missing = pupil_missing, stringsAsFactors = FALSE)
    } else {
        do.call(rbind, lapply(split(seq_len(nrow(d)), d$device), function(ind) {
            data.frame(
                device = as.character(d$device[ind[[1L]]]),
                n = length(ind),
                pupil_missing = mean(is.na(d$pupil[ind])),
                pupil_mean = if (all(is.na(d$pupil[ind]))) NA_real_ else mean(d$pupil[ind], na.rm = TRUE),
                stringsAsFactors = FALSE
            )
        }))
    }

    out <- list(
        model = "M3",
        supported = supported,
        warning_free = warning_free,
        checks = checks,
        n_person = n_person,
        n_item = n_item,
        design = designs,
        variation = variation,
        missing_fraction = vapply(data$observed, function(z) mean(!z), numeric(1L)),
        pupil = list(
            missing_fraction = pupil_missing,
            blink_rate = blink_rate,
            interpolation_rate = interpolation_rate,
            variable_items = variable_pupil_items,
            variable_persons = variable_pupil_persons,
            nuisance = data$nuisance,
            transformation = data$pupil_transform
        ),
        device = device_table,
        statement = paste(
            "This is a conservative structural/data-support audit.",
            "Passing does not establish global identifiability, empirical construct validity,",
            "or robustness to informative pupil/gaze missingness, device artefacts, or unmeasured luminance."
        )
    )
    class(out) <- c("eye_multimodal_m3_identifiability", "list")
    out
}

print.eye_multimodal_m3_identifiability <- function(x, ...) {
    cat(
        "<eye_multimodal_m3_identifiability>\n",
        "  persons: ", x$n_person, "\n",
        "  items: ", x$n_item, "\n",
        "  supported: ", x$supported, "\n",
        "  warning-free: ", x$warning_free, "\n",
        "  missing: ", paste(names(x$missing_fraction), sprintf("%.3f", x$missing_fraction), sep = "=", collapse = ", "), "\n",
        "  pupil blink/interpolation: ", sprintf("%.3f", x$pupil$blink_rate), " / ", sprintf("%.3f", x$pupil$interpolation_rate), "\n",
        "  boundary: ", x$statement, "\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m3_ppp_channel <- function(fit, observed_name, replicate_name, channel) {
    I <- length(fit$data$item_levels)
    obs <- .ep10_m2_extract_item_draws(fit, observed_name, I)
    rep <- .ep10_m2_extract_item_draws(fit, replicate_name, I)
    ppp <- colMeans(rep >= obs)

    mask <- fit$data$observed[[channel]]
    d <- fit$data$raw
    counts <- tabulate(
        d$item_index[mask],
        nbins = I
    )
    ppp[counts == 0L] <- NA_real_

    data.frame(
        item_id = fit$data$item_levels,
        channel = channel,
        n_observed = counts,
        ppp = as.numeric(ppp),
        lower_tail = !is.na(ppp) & ppp < 0.05,
        upper_tail = !is.na(ppp) & ppp > 0.95,
        flagged = !is.na(ppp) & (ppp < 0.05 | ppp > 0.95),
        stringsAsFactors = FALSE
    )
}

#' Posterior predictive checks for the M3 four-channel model
#'
#' Extends the M2 W/L/M discrepancy family with a standardized pupil residual
#' discrepancy P. Tail flags are diagnostics, not construct validation.
#'
#' @param x An M3 fit.
#' @return An `eye_multimodal_m3_ppc`.
#' @export
multimodal_m3_ppc <- function(x) {
    if (!inherits(x, "eye_multimodal_m3_fit")) stop("`x` must be an M3 fit.", call. = FALSE)
    tab <- rbind(
        .ep10_m3_ppp_channel(x, "W_obs", "W_rep", "response"),
        .ep10_m3_ppp_channel(x, "L_obs", "L_rep", "rt"),
        .ep10_m3_ppp_channel(x, "M_obs", "M_rep", "gaze"),
        .ep10_m3_ppp_channel(x, "P_obs", "P_rep", "pupil")
    )

    pupil_rep <- posterior::as_draws_matrix(x$fit$draws("pupil_rep"))
    obs_rows <- which(x$data$observed$pupil)
    pupil_observed <- x$data$raw$pupil_model[obs_rows]
    pupil_global <- data.frame(
        statistic = c("mean", "sd", "q10", "median", "q90"),
        observed = c(mean(pupil_observed), stats::sd(pupil_observed), stats::quantile(pupil_observed, .1), stats::median(pupil_observed), stats::quantile(pupil_observed, .9)),
        replicated_median = c(
            stats::median(rowMeans(pupil_rep)),
            stats::median(apply(pupil_rep, 1L, stats::sd)),
            stats::median(apply(pupil_rep, 1L, stats::quantile, probs = .1)),
            stats::median(apply(pupil_rep, 1L, stats::median)),
            stats::median(apply(pupil_rep, 1L, stats::quantile, probs = .9))
        ),
        stringsAsFactors = FALSE
    )

    out <- list(
        table = tab,
        flag_rate = if (any(tab$n_observed > 0L)) mean(tab$flagged[tab$n_observed > 0L]) else NA_real_,
        flag_rate_by_channel = tapply(
            seq_len(nrow(tab)), tab$channel,
            function(ind) {
                usable <- ind[tab$n_observed[ind] > 0L]
                if (!length(usable)) NA_real_ else mean(tab$flagged[usable])
            }
        ),
        pupil_global = pupil_global,
        interpretation = paste(
            "PPC evaluates model-data discrepancy for the four observed channels.",
            "A well-fitting pupil channel is not evidence that it measures cognitive load, effort, or arousal."
        )
    )
    class(out) <- c("eye_multimodal_m3_ppc", "list")
    out
}

print.eye_multimodal_m3_ppc <- function(x, ...) {
    cat("<eye_multimodal_m3_ppc>\n",
        "  item-channel checks: ", nrow(x$table), "\n",
        "  tail-flag rate: ", sprintf("%.3f", x$flag_rate), "\n",
        "  by channel: ", paste(names(x$flag_rate_by_channel), sprintf("%.3f", x$flag_rate_by_channel), sep = "=", collapse = ", "), "\n",
        sep = "")
    invisible(x)
}

.ep10_m3_sampler_audit <- function(x) {
    vars <- c(
        "z_person", "sigma_person", "L_person", "z_item", "mu_item", "sigma_item", "L_item",
        "nu", "s", "sigma_pupil", "gamma_pupil", "theta", "tau", "omega", "rho", "b", "beta", "m", "kappa",
        "corr_person", "corr_item"
    )
    s <- x$fit$summary(variables = vars)
    finite_rhat <- s$rhat[is.finite(s$rhat)]
    finite_bulk <- s$ess_bulk[is.finite(s$ess_bulk)]
    finite_tail <- s$ess_tail[is.finite(s$ess_tail)]

    dg <- tryCatch(
        as.data.frame(x$fit$diagnostic_summary()),
        error = function(e) NULL
    )

    get_diag <- function(candidates) {
        if (is.null(dg)) return(NULL)
        nm <- intersect(candidates, names(dg))
        if (!length(nm)) return(NULL)
        dg[[nm[[1L]]]]
    }

    div_values <- get_diag(c("num_divergent", "num_divergences", "divergent"))
    depth_values <- get_diag(c("num_max_treedepth", "max_treedepth"))
    ebfmi_values <- get_diag(c("ebfmi", "E-BFMI"))

    div <- if (is.null(div_values)) NA_integer_ else as.integer(sum(div_values, na.rm = TRUE))
    depth <- if (is.null(depth_values)) NA_integer_ else as.integer(sum(depth_values, na.rm = TRUE))
    ebfmi <- if (is.null(ebfmi_values) || !any(is.finite(ebfmi_values))) {
        NA_real_
    } else {
        min(ebfmi_values[is.finite(ebfmi_values)])
    }

    list(
        max_rhat = if (length(finite_rhat)) max(finite_rhat) else NA_real_,
        min_ess_bulk = if (length(finite_bulk)) min(finite_bulk) else NA_real_,
        min_ess_tail = if (length(finite_tail)) min(finite_tail) else NA_real_,
        divergences = div,
        max_treedepth_hits = depth,
        min_ebfmi = ebfmi
    )
}

.ep10_m3_ablation_definitions <- function() {
    data.frame(
        model = c("R", "R_RT", "R_GAZE", "R_PUPIL", "R_RT_GAZE", "R_RT_PUPIL", "R_GAZE_PUPIL", "FULL"),
        rt = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE),
        gaze = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
        pupil = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE),
        channels = c(
            "response", "response + RT", "response + gaze", "response + pupil",
            "response + RT + gaze", "response + RT + pupil", "response + gaze + pupil", "response + RT + gaze + pupil"
        ),
        stringsAsFactors = FALSE
    )
}

.ep10_m3_subset_stan_data <- function(
    data, use_rt, use_gaze, use_pupil, prior_profile = "regularized",
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names)
) {
    d <- data$raw
    rr <- which(data$observed$response)
    tr <- if (use_rt) which(data$observed$rt) else integer()
    gr <- if (use_gaze) which(data$observed$gaze) else integer()
    pr <- if (use_pupil) which(data$observed$pupil) else integer()

    if (!length(rr)) stop("M3 ablation requires observed response values for the common target.", call. = FALSE)
    if (isTRUE(use_rt) && !length(tr)) stop("Requested M3 ablation model contains RT but no RT values are observed.", call. = FALSE)
    if (isTRUE(use_gaze) && !length(gr)) stop("Requested M3 ablation model contains gaze but no gaze values are observed.", call. = FALSE)
    if (isTRUE(use_pupil) && !length(pr)) stop("Requested M3 ablation model contains pupil but no pupil values are observed.", call. = FALSE)

    active <- c(rt = use_rt, gaze = use_gaze, pupil = use_pupil)
    active_names <- names(active)[active]
    indices <- stats::setNames(rep(0L, 3L), c("rt", "gaze", "pupil"))
    if (length(active_names)) indices[active_names] <- seq_along(active_names) + 1L
    K <- 1L + length(active_names)

    if (is.null(names(nuisance)) || !all(.ep10_m3_nuisance_names %in% names(nuisance))) {
        stop("`nuisance` must name all M3 pupil nuisance terms.", call. = FALSE)
    }
    nuisance <- as.logical(nuisance[.ep10_m3_nuisance_names])
    use_cov <- as.integer(data$nuisance$available & !data$nuisance$degenerate & nuisance)
    if (!use_pupil) use_cov[] <- 0L
    X <- if (length(pr)) data$nuisance_matrix[pr, , drop = FALSE] else matrix(numeric(), nrow = 0L, ncol = 8L)

    list(
        J = length(data$person_levels), I = length(data$item_levels), K = K,
        use_rt = as.integer(use_rt), use_gaze = as.integer(use_gaze), use_pupil = as.integer(use_pupil),
        idx_rt = indices[["rt"]], idx_gaze = indices[["gaze"]], idx_pupil = indices[["pupil"]],
        N_response = length(rr), person_response = as.integer(d$person_index[rr]), item_response = as.integer(d$item_index[rr]), y_response = as.integer(d$response[rr]),
        N_rt = length(tr), person_rt = as.integer(d$person_index[tr]), item_rt = as.integer(d$item_index[tr]), log_rt = log(as.numeric(d$rt[tr])),
        N_gaze = length(gr), person_gaze = as.integer(d$person_index[gr]), item_gaze = as.integer(d$item_index[gr]), gaze = as.integer(d$gaze[gr]),
        N_pupil = length(pr), person_pupil = as.integer(d$person_index[pr]), item_pupil = as.integer(d$item_index[pr]), pupil = as.numeric(d$pupil_model[pr]),
        X_pupil = unname(as.matrix(X)), use_pupil_covariate = use_cov,
        prior_profile = if (identical(prior_profile, "regularized")) 1L else 2L
    )
}

.ep10_m3_fit_subset <- function(
    data, definition, prior_profile = "regularized",
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
    chains = 4L, parallel_chains = chains,
    iter_warmup = 750L, iter_sampling = 750L, seed = 20260815L,
    adapt_delta = .95, max_treedepth = 12L, refresh = 100L,
    quiet_compile = TRUE, init = 0, model_object = NULL
) {
    .ep10_m2_require_backend()
    if (is.null(model_object)) model_object <- .ep10_m3_compile("ablation", quiet = quiet_compile)
    stan_data <- .ep10_m3_subset_stan_data(
        data, definition$rt, definition$gaze, definition$pupil,
        prior_profile = prior_profile, nuisance = nuisance
    )
    fit <- model_object$sample(
        data = stan_data, chains = as.integer(chains), parallel_chains = as.integer(parallel_chains),
        iter_warmup = as.integer(iter_warmup), iter_sampling = as.integer(iter_sampling), seed = as.integer(seed),
        adapt_delta = adapt_delta, max_treedepth = as.integer(max_treedepth), refresh = as.integer(refresh), init = init
    )
    structure(list(
        model = definition$model, fit = fit, data = data, stan_data = stan_data,
        channels = definition$channels,
        pupil_nuisance = stats::setNames(
            as.logical(stan_data$use_pupil_covariate),
            .ep10_m3_nuisance_names
        ),
        sampling_controls = list(chains = chains, iter_warmup = iter_warmup, iter_sampling = iter_sampling, max_treedepth = max_treedepth),
        interpretation = "Ablation fit on a common response target; omitted sensors are not causal interventions."
    ), class = c("eye_multimodal_m3_subset_fit", "list"))
}

#' Fit the complete M3 response-anchored channel-ablation lattice
#'
#' Fits all eight response-anchored combinations of RT, gaze and pupil. Models
#' use one common subset-likelihood implementation to make the target and prior
#' family explicit. This is inferential ablation, not a causal intervention on sensors.
#'
#' @param x M3-compatible data.
#' @param models Optional subset of the eight model identifiers.
#' @param nuisance Named logical vector selecting pupil nuisance terms. The same
#'   selection is applied to every ablation model containing pupil.
#' @param ... Sampling controls forwarded to the subset fitter.
#' @return An `eye_multimodal_m3_ablation`.
#' @export
multimodal_m3_ablation <- function(
    x, models = NULL,
    nuisance = stats::setNames(rep(TRUE, 8L), .ep10_m3_nuisance_names),
    ...
) {
    data <- .ep10_m3_as_data(x)
    definitions <- .ep10_m3_ablation_definitions()
    if (!is.null(models)) {
        bad <- setdiff(models, definitions$model)
        if (length(bad)) stop("Unknown M3 ablation models: ", paste(bad, collapse = ", "), call. = FALSE)
        definitions <- definitions[match(models, definitions$model), , drop = FALSE]
    }
    mod <- .ep10_m3_compile("ablation")
    fits <- lapply(
        seq_len(nrow(definitions)),
        function(k) .ep10_m3_fit_subset(
            data, definitions[k, , drop = FALSE], model_object = mod,
            nuisance = nuisance, ...
        )
    )
    names(fits) <- definitions$model
    out <- list(
        fits = fits,
        definitions = definitions,
        pupil_nuisance = stats::setNames(
            as.logical(nuisance[.ep10_m3_nuisance_names]),
            .ep10_m3_nuisance_names
        ),
        target = "held-out response cells among observed persons/items",
        interpretation = paste(
            "All models retain response as the common prediction target.",
            "Channel gains may be zero or negative; contributions are not assumed additive and ablation is not causal."
        )
    )
    class(out) <- c("eye_multimodal_m3_ablation", "list")
    out
}

print.eye_multimodal_m3_ablation <- function(x, ...) {
    cat("<eye_multimodal_m3_ablation>\n",
        "  models: ", paste(names(x$fits), collapse = ", "), "\n",
        "  target: ", x$target, "\n",
        "  boundary: ", x$interpretation, "\n", sep = "")
    invisible(x)
}

.ep10_m3_response_loo <- function(fit) {
    .ep10_m2_require_backend(require_loo = TRUE)
    ll <- as.array(fit$fit$draws("log_lik_response", format = "draws_array"))
    loo::loo(ll, r_eff = loo::relative_eff(exp(ll)))
}

.ep10_m3_theta_variance <- function(fit) {
    theta <- posterior::as_draws_matrix(fit$fit$draws("theta"))
    apply(theta, 2L, stats::var)
}

.ep10_m3_pair_delta <- function(pointwise, a, b) {
    za <- pointwise[[a]]; zb <- pointwise[[b]]
    if (is.null(za) || is.null(zb) || length(za) != length(zb)) return(c(delta = NA_real_, se = NA_real_))
    d <- za - zb
    c(delta = sum(d), se = if (length(d) > 1L) sqrt(length(d) * stats::var(d)) else NA_real_)
}

#' Quantify M3 process information, pupil increment, redundancy and sensor value
#'
#' Uses response-target PSIS-LOO and posterior ability variance across the
#' eight-channel ablation lattice. It additionally reports paired pupil gains,
#' a non-additivity/redundancy contrast, a channel-conflict screen, and optional
#' information per usable pupil observation or sensor cost. Positive values do
#' not establish construct validity or causal sensor value.
#'
#' @param x An M3 ablation object.
#' @param pupil_cost Optional positive relative pupil-sensor cost.
#' @param decisive_z Absolute delta/SE ratio used only as a descriptive evidence flag.
#' @return An `eye_multimodal_m3_information` object.
#' @export
multimodal_m3_process_information <- function(x, pupil_cost = 1, decisive_z = 2) {
    if (!inherits(x, "eye_multimodal_m3_ablation")) stop("`x` must come from `multimodal_m3_ablation()`.", call. = FALSE)
    required_models <- .ep10_m3_ablation_definitions()$model
    absent_models <- setdiff(required_models, names(x$fits))
    if (length(absent_models)) {
        stop(
            "M3 process information requires the complete eight-model ablation lattice; missing: ",
            paste(absent_models, collapse = ", "),
            call. = FALSE
        )
    }
    if (!is.numeric(pupil_cost) || length(pupil_cost) != 1L || !is.finite(pupil_cost) || pupil_cost <= 0) stop("`pupil_cost` must be one positive finite number.", call. = FALSE)
    .ep10_m2_require_backend(require_loo = TRUE)

    loo_objects <- lapply(x$fits, .ep10_m3_response_loo)
    elpd <- vapply(loo_objects, function(z) z$estimates["elpd_loo", "Estimate"], numeric(1L))
    elpd_se <- vapply(loo_objects, function(z) z$estimates["elpd_loo", "SE"], numeric(1L))
    pointwise <- lapply(loo_objects, function(z) as.numeric(z$pointwise[, "elpd_loo"]))
    kmax <- vapply(loo_objects, function(z) {
        k <- tryCatch(loo::pareto_k_values(z), error = function(e) numeric())
        if (!length(k)) NA_real_ else max(k, na.rm = TRUE)
    }, numeric(1L))
    theta_var <- lapply(x$fits, .ep10_m3_theta_variance)
    mean_var <- vapply(theta_var, mean, numeric(1L))
    response_ref <- elpd[["R"]]
    var_ref <- mean_var[["R"]]

    table <- data.frame(
        model = names(x$fits),
        channels = x$definitions$channels[match(names(x$fits), x$definitions$model)],
        response_elpd_loo = unname(elpd), response_elpd_se = unname(elpd_se),
        delta_elpd_vs_response = unname(elpd - response_ref),
        mean_theta_posterior_variance = unname(mean_var),
        theta_variance_reduction_vs_response = if (is.finite(var_ref) && var_ref > 0) unname(1 - mean_var / var_ref) else NA_real_,
        max_pareto_k = unname(kmax), stringsAsFactors = FALSE
    )

    pairs <- data.frame(
        without_pupil = c("R", "R_RT", "R_GAZE", "R_RT_GAZE"),
        with_pupil = c("R_PUPIL", "R_RT_PUPIL", "R_GAZE_PUPIL", "FULL"),
        context = c("response", "response + RT", "response + gaze", "response + RT + gaze"),
        stringsAsFactors = FALSE
    )
    pair_stats <- t(vapply(seq_len(nrow(pairs)), function(k) .ep10_m3_pair_delta(pointwise, pairs$with_pupil[[k]], pairs$without_pupil[[k]]), numeric(2L)))
    incremental_pupil <- cbind(pairs, delta_response_elpd = pair_stats[, "delta"], se = pair_stats[, "se"])
    incremental_pupil$z <- incremental_pupil$delta_response_elpd / incremental_pupil$se
    incremental_pupil$evidence <- ifelse(
        !is.finite(incremental_pupil$z), "inconclusive",
        ifelse(incremental_pupil$z >= decisive_z, "supports_incremental_pupil_information",
               ifelse(incremental_pupil$z <= -decisive_z, "supports_pupil_instability_or_harm", "no_clear_incremental_pupil_information"))
    )

    d_full <- incremental_pupil$delta_response_elpd[incremental_pupil$context == "response + RT + gaze"]
    d_r <- incremental_pupil$delta_response_elpd[incremental_pupil$context == "response"]
    d_rt <- .ep10_m3_pair_delta(pointwise, "R_RT", "R")[["delta"]]
    d_gaze <- .ep10_m3_pair_delta(pointwise, "R_GAZE", "R")[["delta"]]
    nonadditivity <- data.frame(
        metric = c("full_vs_sum_single_additions", "pupil_increment_after_rt_gaze_vs_alone"),
        value = c(elpd[["FULL"]] - response_ref - d_rt - d_gaze - d_r, d_full - d_r),
        interpretation = c("zero is additive on the response ELPD scale; nonzero indicates redundancy/synergy/dependence", "negative suggests pupil value is attenuated after RT+gaze; positive suggests conditional complementarity"),
        stringsAsFactors = FALSE
    )

    full_fit <- x$fits[["FULL"]]
    pupil_usable <- if (is.null(full_fit)) NA_integer_ else full_fit$stan_data$N_pupil
    sensor_value <- data.frame(
        metric = c("delta_elpd_full_pupil", "delta_elpd_per_usable_pupil", "delta_elpd_per_relative_cost"),
        value = c(d_full, if (is.finite(pupil_usable) && pupil_usable > 0) d_full / pupil_usable else NA_real_, d_full / pupil_cost),
        stringsAsFactors = FALSE
    )

    validation <- lapply(x$fits, function(z) {
        s <- z$fit$summary()
        c(max_rhat = max(s$rhat[is.finite(s$rhat)], na.rm = TRUE), min_bulk_ess = min(s$ess_bulk[is.finite(s$ess_bulk)], na.rm = TRUE))
    })
    validation <- do.call(rbind, validation)
    conflict <- data.frame(
        model = rownames(validation),
        max_rhat = validation[, "max_rhat"], min_bulk_ess = validation[, "min_bulk_ess"],
        response_elpd = elpd[rownames(validation)], stringsAsFactors = FALSE
    )
    rownames(conflict) <- NULL

    full_verdict <- incremental_pupil$evidence[incremental_pupil$context == "response + RT + gaze"]
    if (!length(full_verdict)) full_verdict <- "inconclusive"

    out <- list(
        table = table, incremental_pupil = incremental_pupil, nonadditivity = nonadditivity,
        sensor_value = sensor_value, channel_conflict = conflict, loo = loo_objects,
        theta_variance = theta_var, verdict = full_verdict,
        target = x$target,
        interpretation = paste(
            "Process information is response-target predictive/uncertainty evidence under the fitted models.",
            "It is not Fisher-information additivity, construct validity, causal sensor utility, or new-person/new-item transport.",
            "The package is allowed to conclude that pupil adds no clear defensible information."
        )
    )
    class(out) <- c("eye_multimodal_m3_information", "list")
    out
}

print.eye_multimodal_m3_information <- function(x, ...) {
    cat("<eye_multimodal_m3_information>\n",
        "  target: ", x$target, "\n",
        "  incremental pupil verdict after RT+gaze: ", x$verdict, "\n",
        "  non-additivity: explicitly evaluated\n\n", sep = "")
    print(x$incremental_pupil, row.names = FALSE)
    invisible(x)
}

.ep10_m3_person_summaries <- function(d) {
    persons <- sort(unique(d$person_id))
    do.call(rbind, lapply(persons, function(p) {
        z <- d[d$person_id == p, , drop = FALSE]
        safe <- function(v, transform = identity) if (all(is.na(v))) NA_real_ else mean(transform(v[!is.na(v)]))
        data.frame(person_id = p,
                   response_mean = safe(z$response), log_rt_mean = safe(z$rt, log), gaze_mean = safe(z$gaze), pupil_mean = safe(z$pupil),
                   stringsAsFactors = FALSE)
    }))
}

.ep10_m3_person_correlations <- function(d, label) {
    p <- .ep10_m3_person_summaries(d)
    pairs <- list(response_rt = c("response_mean", "log_rt_mean"), response_gaze = c("response_mean", "gaze_mean"),
                  response_pupil = c("response_mean", "pupil_mean"), rt_gaze = c("log_rt_mean", "gaze_mean"),
                  rt_pupil = c("log_rt_mean", "pupil_mean"), gaze_pupil = c("gaze_mean", "pupil_mean"))
    do.call(rbind, lapply(names(pairs), function(nm) {
        cc <- stats::complete.cases(p[, pairs[[nm]], drop = FALSE])
        data.frame(dataset = label, pair = nm,
                   correlation = if (sum(cc) >= 3L) stats::cor(p[[pairs[[nm]][1L]]][cc], p[[pairs[[nm]][2L]]][cc]) else NA_real_,
                   n = sum(cc), stringsAsFactors = FALSE)
    }))
}

.ep10_m3_permute_within <- function(dat, column, group = "item_id") {
    out <- dat
    ids <- split(seq_len(nrow(out)), out[[group]])
    for (ind in ids) {
        obs <- ind[!is.na(out[[column]][ind])]
        if (length(obs) > 1L) out[[column]][obs] <- sample(out[[column]][obs], length(obs), replace = FALSE)
    }
    out
}

.ep10_m3_phase_randomize <- function(x) {
    obs <- which(!is.na(x))
    if (length(obs) < 6L) return(sample(x, length(x), replace = FALSE))

    z <- x[obs] - mean(x[obs])
    f <- stats::fft(z)
    n <- length(f)

    # For a real series, randomize positive-frequency phases and construct the
    # negative-frequency coefficients by conjugacy. DC and the Nyquist bin
    # (when n is even) remain real. This approximately preserves the spectrum.
    upper <- if (n %% 2L == 0L) n / 2L else (n + 1L) / 2L
    positive <- if (upper >= 2L) seq.int(2L, upper) else integer()

    if (length(positive)) {
        phase <- stats::runif(length(positive), -pi, pi)
        f[positive] <- Mod(f[positive]) * exp(1i * phase)
        negative <- n - positive + 2L
        f[negative] <- Conj(f[positive])
    }

    if (n %% 2L == 0L) {
        nyquist <- n / 2L + 1L
        f[nyquist] <- Re(f[nyquist])
    }
    f[1L] <- Re(f[1L])

    out <- x
    out[obs] <- Re(stats::fft(f, inverse = TRUE) / n) + mean(x[obs])
    out
}

.ep10_m3_phase_randomize_trials <- function(d) {
    out <- d
    groups <- split(seq_len(nrow(d)), d$person_id)
    for (ind in groups) {
        ord <- if (!all(is.na(d$time_on_task[ind]))) {
            order(d$time_on_task[ind], d$source_row[ind], na.last = TRUE)
        } else {
            order(d$source_row[ind])
        }
        ordered <- ind[ord]
        out$pupil[ordered] <- .ep10_m3_phase_randomize(d$pupil[ordered])
    }
    out
}

#' Generate M3 multimodal falsification controls
#'
#' Includes within-item RT/gaze/pupil permutations, within-person pupil
#' permutation across items, pupil phase randomization, luminance-only pseudo-pupil, and an
#' irrelevant synthetic channel. Controls preserve selected marginals while
#' deliberately breaking alignment; they are not causal interventions or
#' misconduct detectors.
#'
#' @param x M3-compatible data.
#' @param seed Deterministic seed.
#' @return An `eye_multimodal_m3_negative_controls`.
#' @export
multimodal_m3_negative_controls <- function(x, seed = 20260815L) {
    data <- .ep10_m3_as_data(x)
    d <- data$raw
    set.seed(as.integer(seed))
    controls <- list(
        observed = d,
        gaze_within_item = .ep10_m3_permute_within(d, "gaze"),
        rt_within_item = .ep10_m3_permute_within(d, "rt"),
        pupil_within_item = .ep10_m3_permute_within(d, "pupil"),
        pupil_within_person = .ep10_m3_permute_within(d, "pupil", "person_id")
    )
    controls$pupil_phase_randomized <- .ep10_m3_phase_randomize_trials(d)
    lum <- d
    if (!all(is.na(lum$luminance))) {
        sc <- scale(lum$luminance); lum$pupil <- as.numeric(sc) + stats::rnorm(nrow(lum), 0, .15)
    } else {
        lum$pupil <- stats::rnorm(nrow(lum))
    }
    controls$luminance_only_pupil <- lum
    irr <- d; irr$pupil <- stats::rnorm(nrow(irr)); controls$irrelevant_pupil <- irr

    diagnostics <- do.call(rbind, Map(.ep10_m3_person_correlations, controls, names(controls)))
    provenance <- data.frame(
        control = setdiff(names(controls), "observed"),
        purpose = c(
            "break gaze-person alignment within item", "break RT-person alignment within item", "break pupil-person alignment within item",
            "break pupil-item alignment within person", "destroy within-person ordered pupil-series phase structure while retaining its spectrum approximately",
            "test measurement artefact masquerading as pupil value", "test irrelevant channel false-positive value"
        ),
        interpretation = "falsification diagnostic only; not causal and not a behavioral classifier",
        stringsAsFactors = FALSE
    )
    out <- list(datasets = controls, diagnostics = diagnostics, provenance = provenance, seed = as.integer(seed),
                interpretation = "Meaningless or misaligned pupil channels should not systematically appear psychometrically valuable.")
    class(out) <- c("eye_multimodal_m3_negative_controls", "list")
    out
}

print.eye_multimodal_m3_negative_controls <- function(x, ...) {
    cat("<eye_multimodal_m3_negative_controls>\n",
        "  controls: ", paste(setdiff(names(x$datasets), "observed"), collapse = ", "), "\n",
        "  seed: ", x$seed, "\n",
        "  boundary: ", x$interpretation, "\n", sep = "")
    invisible(x)
}

#' Bridge existing functional pupil outputs into the scalar M3 reference layer
#'
#' This bridge deliberately requires an analyst-supplied, already-derived
#' trial-level functional score. It does not silently reduce a raw time series.
#' Existing `functional_pupil_irt_spec()`, deconvolution, and confound tools
#' remain the authoritative trajectory-level machinery.
#'
#' @param data Trial-level M3 data.
#' @param score Trial-level functional/trajectory score or its column name.
#' @param pupil Name of the output pupil column.
#' @param provenance Free-text derivation/provenance note.
#' @return An `eye_multimodal_m3_functional_bridge` data object.
#' @export
multimodal_m3_functional_bridge <- function(data, score, pupil = "pupil", provenance = NULL) {
    if (!is.data.frame(data)) stop("`data` must be a trial-level data frame.", call. = FALSE)
    if (is.character(score) && length(score) == 1L && score %in% names(data)) {
        value <- data[[score]]; source <- score
    } else {
        value <- score; source <- "supplied_vector"
    }
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != nrow(data) || any(!is.na(value) & !is.finite(value))) stop("Functional score must provide one finite-or-NA value per trial.", call. = FALSE)
    out_data <- data; out_data[[pupil]] <- value
    out <- list(
        data = out_data, score_source = source, pupil_column = pupil, provenance = provenance,
        representation = "functional_score",
        boundary = paste(
            "The bridge records an externally justified scalar trajectory representation.",
            "It does not claim that the scalar preserves all functional pupil information or identify a psychological construct."
        )
    )
    class(out) <- c("eye_multimodal_m3_functional_bridge", "list")
    out
}

print.eye_multimodal_m3_functional_bridge <- function(x, ...) {
    cat("<eye_multimodal_m3_functional_bridge>\n",
        "  rows: ", nrow(x$data), "\n",
        "  score source: ", x$score_source, "\n",
        "  pupil column: ", x$pupil_column, "\n",
        "  boundary: ", x$boundary, "\n", sep = "")
    invisible(x)
}

#' Validate an M3 simulation or fitted four-channel model
#'
#' Combines structural support, sampler diagnostics, PPC, pupil-confound
#' availability, device/missingness summaries and explicit interpretive
#' boundaries. Validation of synthetic or computational behavior is not
#' empirical construct validation.
#'
#' @param x M3 simulation or fit.
#' @param include_ppc Include M3 PPC for fits.
#' @param rhat_max,ess_min,ebfmi_min Diagnostic thresholds.
#' @return An `eye_multimodal_m3_validation`.
#' @export
validate_multimodal_m3 <- function(x, include_ppc = TRUE, rhat_max = 1.05, ess_min = 50, ebfmi_min = 0.30) {
    if (inherits(x, "eye_multimodal_m3_simulation")) {
        audit <- audit_multimodal_m3_identifiability(x)
        out <- list(
            valid = isTRUE(audit$supported), type = "simulation", model = "M3", checks = audit$checks,
            identifiability = audit, diagnostics = NULL, ppc = NULL,
            pupil_truth = x$truth$pupil,
            boundary = "Synthetic consistency/support is not empirical construct validation."
        )
        class(out) <- c("eye_multimodal_m3_validation", "list")
        return(out)
    }
    if (!inherits(x, "eye_multimodal_m3_fit")) stop("`x` must be an M3 simulation or fit.", call. = FALSE)
    diagnostics <- .ep10_m3_sampler_audit(x)
    diag_checks <- data.frame(
        check = c("max_rhat", "min_ess_bulk", "min_ess_tail", "divergences", "max_treedepth_hits", "min_ebfmi"),
        value = c(diagnostics$max_rhat, diagnostics$min_ess_bulk, diagnostics$min_ess_tail, diagnostics$divergences, diagnostics$max_treedepth_hits, diagnostics$min_ebfmi),
        pass = c(
            is.finite(diagnostics$max_rhat) && diagnostics$max_rhat <= rhat_max,
            is.finite(diagnostics$min_ess_bulk) && diagnostics$min_ess_bulk >= ess_min,
            is.finite(diagnostics$min_ess_tail) && diagnostics$min_ess_tail >= ess_min,
            is.na(diagnostics$divergences) || diagnostics$divergences == 0L,
            is.na(diagnostics$max_treedepth_hits) || diagnostics$max_treedepth_hits == 0L,
            is.na(diagnostics$min_ebfmi) || diagnostics$min_ebfmi >= ebfmi_min
        ), stringsAsFactors = FALSE
    )
    ppc <- if (isTRUE(include_ppc)) multimodal_m3_ppc(x) else NULL
    ppc_ok <- is.null(ppc) || ppc$flag_rate <= .20
    ident <- x$audit
    checks <- rbind(
        data.frame(check = paste0("ident_", ident$checks$check), value = NA_real_, pass = ident$checks$pass, stringsAsFactors = FALSE),
        diag_checks,
        data.frame(check = "ppc_tail_flag_rate_le_0.20", value = if (is.null(ppc)) NA_real_ else ppc$flag_rate, pass = ppc_ok, stringsAsFactors = FALSE)
    )
    out <- list(
        valid = all(checks$pass), type = "fit", model = "M3", checks = checks, identifiability = ident,
        diagnostics = diagnostics, ppc = ppc, nuisance = x$data$nuisance,
        missingness = ident$missing_fraction, device = ident$device,
        boundary = paste(
            "Computational validation evaluates estimation and model-data fit.",
            "It does not establish cognitive-load validity, causal sensor value, MNAR robustness, or device equivalence."
        )
    )
    class(out) <- c("eye_multimodal_m3_validation", "list")
    out
}

print.eye_multimodal_m3_validation <- function(x, ...) {
    cat("<eye_multimodal_m3_validation>\n",
        "  type: ", x$type, "\n",
        "  valid: ", x$valid, "\n",
        "  checks passed: ", sum(x$checks$pass), "/", nrow(x$checks), "\n",
        "  boundary: ", x$boundary, "\n", sep = "")
    invisible(x)
}
