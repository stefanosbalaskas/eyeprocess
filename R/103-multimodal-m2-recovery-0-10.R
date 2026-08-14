# eyeprocess 0.10 — M2 simulation and recovery evidence

.ep10_m2_safe_cor_matrix <- function(x, name = "correlation") {
    x <- as.matrix(x)

    if (
        nrow(x) != ncol(x) ||
        nrow(x) < 1L ||
        any(!is.finite(x)) ||
        max(abs(x - t(x))) > 1e-8 ||
        any(abs(diag(x) - 1) > 1e-8)
    ) {
        stop(
            "`",
            name,
            "` must be a finite symmetric correlation matrix with unit diagonal.",
            call. = FALSE
        )
    }

    ev <- eigen(
        x,
        symmetric = TRUE,
        only.values = TRUE
    )$values

    if (min(ev) <= 1e-8) {
        stop(
            "`",
            name,
            "` must be positive definite.",
            call. = FALSE
        )
    }

    x
}

.ep10_m2_rmvnorm <- function(n, sd, cor) {
    sd <- as.numeric(sd)
    cor <- .ep10_m2_safe_cor_matrix(cor)
    cov <- diag(sd) %*% cor %*% diag(sd)
    L <- chol(cov)
    z <- matrix(
        stats::rnorm(n * length(sd)),
        nrow = n,
        ncol = length(sd)
    )
    z %*% L
}

.ep10_m2_rinv_gamma <- function(n, shape, scale) {
    1 / stats::rgamma(
        n,
        shape = shape,
        rate = scale
    )
}

#' Simulate from the M2 response + RT + gaze generative model
#'
#' Simulates the same level-1 likelihood used by [fit_multimodal_m2()]
#' and retains all person/item hyperparameters and realized latent
#' parameters as truth. Channel-specific dropout is applied only after
#' the complete generative data have been created.
#'
#' @param n_person Number of persons.
#' @param n_item Number of items.
#' @param mu_item Means for item difficulty, log-time intensity, and
#'   log-gaze intensity.
#' @param sd_person Person-side standard deviations for ability, speed,
#'   and gaze-process propensity.
#' @param cor_person Person-side correlation matrix.
#' @param sd_item Item-side standard deviations.
#' @param cor_item Item-side correlation matrix.
#' @param nu_range Uniform range for item time-discrimination parameters.
#' @param gaze_shape Shape/scale parameters for inverse-gamma generation
#'   of negative-binomial shape parameters.
#' @param dropout Named probabilities for response, RT, and gaze missingness.
#' @param seed Reproducibility seed.
#' @return An `eye_multimodal_m2_simulation`.
#' @export
simulate_multimodal_m2 <- function(
    n_person = 100L,
    n_item = 10L,
    mu_item = c(
        difficulty = 0,
        time_intensity = 4,
        gaze_intensity = 3.5
    ),
    sd_person = c(
        ability = 1,
        speed = 0.5,
        gaze_process = 0.5
    ),
    cor_person = matrix(
        c(
            1.00,  0.30, -0.30,
            0.30,  1.00, -0.25,
           -0.30, -0.25,  1.00
        ),
        3L,
        3L,
        byrow = TRUE
    ),
    sd_item = c(
        difficulty = 0.75,
        time_intensity = 0.35,
        gaze_intensity = 0.60
    ),
    cor_item = matrix(
        c(
            1.00, 0.25, 0.20,
            0.25, 1.00, 0.30,
            0.20, 0.30, 1.00
        ),
        3L,
        3L,
        byrow = TRUE
    ),
    nu_range = c(0.5, 0.8),
    gaze_shape = c(shape = 2, scale = 6),
    dropout = c(
        response = 0,
        rt = 0,
        gaze = 0
    ),
    seed = 20260814L
) {
    n_person <- as.integer(n_person)
    n_item <- as.integer(n_item)

    if (
        n_person < 2L ||
        n_item < 2L
    ) {
        stop(
            "`n_person` and `n_item` must both be at least 2.",
            call. = FALSE
        )
    }

    if (
        length(mu_item) != 3L ||
        length(sd_person) != 3L ||
        length(sd_item) != 3L
    ) {
        stop(
            "M2 requires three person dimensions and three correlated item location dimensions.",
            call. = FALSE
        )
    }

    if (
        any(!is.finite(sd_person)) ||
        any(sd_person <= 0) ||
        any(!is.finite(sd_item)) ||
        any(sd_item <= 0)
    ) {
        stop(
            "All person/item standard deviations must be finite and positive.",
            call. = FALSE
        )
    }

    if (
        any(!is.finite(mu_item)) ||
        any(!is.finite(sd_person)) ||
        any(!is.finite(sd_item))
    ) {
        stop(
            "`mu_item`, `sd_person`, and `sd_item` must contain finite values.",
            call. = FALSE
        )
    }

    cor_person <- .ep10_m2_safe_cor_matrix(
        cor_person,
        "cor_person"
    )

    cor_item <- .ep10_m2_safe_cor_matrix(
        cor_item,
        "cor_item"
    )

    if (
        !identical(dim(cor_person), c(3L, 3L)) ||
        !identical(dim(cor_item), c(3L, 3L))
    ) {
        stop(
            "`cor_person` and `cor_item` must both be 3 x 3 correlation matrices.",
            call. = FALSE
        )
    }

    if (
        is.null(names(gaze_shape)) ||
        !all(c("shape", "scale") %in% names(gaze_shape)) ||
        any(!is.finite(gaze_shape[c("shape", "scale")])) ||
        any(gaze_shape[c("shape", "scale")] <= 0)
    ) {
        stop(
            "`gaze_shape` must contain finite positive named `shape` and `scale` values.",
            call. = FALSE
        )
    }

    if (
        length(nu_range) != 2L ||
        any(!is.finite(nu_range)) ||
        nu_range[[1L]] <= 0 ||
        nu_range[[2L]] <= nu_range[[1L]]
    ) {
        stop(
            "`nu_range` must contain two positive increasing values.",
            call. = FALSE
        )
    }

    dropout_names <- c(
        "response",
        "rt",
        "gaze"
    )

    if (
        is.null(names(dropout)) ||
        !all(dropout_names %in% names(dropout))
    ) {
        stop(
            "`dropout` must be named with response, rt, and gaze.",
            call. = FALSE
        )
    }

    dropout <- dropout[dropout_names]

    if (
        any(!is.finite(dropout)) ||
        any(dropout < 0 | dropout >= 1)
    ) {
        stop(
            "Each dropout probability must be in [0, 1).",
            call. = FALSE
        )
    }

    set.seed(
        as.integer(seed)
    )

    person_effects <- .ep10_m2_rmvnorm(
        n_person,
        sd_person,
        cor_person
    )

    colnames(person_effects) <- c(
        "theta",
        "tau",
        "omega"
    )

    item_effects_centered <- .ep10_m2_rmvnorm(
        n_item,
        sd_item,
        cor_item
    )

    item_effects <- sweep(
        item_effects_centered,
        2L,
        as.numeric(mu_item),
        "+"
    )

    colnames(item_effects) <- c(
        "b",
        "beta",
        "m"
    )

    nu <- stats::runif(
        n_item,
        min = nu_range[[1L]],
        max = nu_range[[2L]]
    )

    s <- .ep10_m2_rinv_gamma(
        n_item,
        shape = gaze_shape[["shape"]],
        scale = gaze_shape[["scale"]]
    )

    grid <- expand.grid(
        person_index = seq_len(n_person),
        item_index = seq_len(n_item),
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )

    theta <- person_effects[
        grid$person_index,
        "theta"
    ]

    tau <- person_effects[
        grid$person_index,
        "tau"
    ]

    omega <- person_effects[
        grid$person_index,
        "omega"
    ]

    b <- item_effects[
        grid$item_index,
        "b"
    ]

    beta <- item_effects[
        grid$item_index,
        "beta"
    ]

    m <- item_effects[
        grid$item_index,
        "m"
    ]

    nu_obs <- nu[
        grid$item_index
    ]

    s_obs <- s[
        grid$item_index
    ]

    response_probability <- .ep10_m2_logit(
        theta - b
    )

    log_rt_mean <- beta - tau

    gaze_mean <- exp(
        m + omega
    )

    response_complete <- stats::rbinom(
        nrow(grid),
        size = 1L,
        prob = response_probability
    )

    log_rt_complete <- stats::rnorm(
        nrow(grid),
        mean = log_rt_mean,
        sd = 1 / nu_obs
    )

    rt_complete <- exp(
        log_rt_complete
    )

    gaze_complete <- stats::rnbinom(
        nrow(grid),
        mu = gaze_mean,
        size = s_obs
    )

    response_observed <- stats::runif(
        nrow(grid)
    ) >= dropout[["response"]]

    rt_observed <- stats::runif(
        nrow(grid)
    ) >= dropout[["rt"]]

    gaze_observed <- stats::runif(
        nrow(grid)
    ) >= dropout[["gaze"]]

    response_value <- response_complete
    response_value[!response_observed] <- NA_integer_

    rt_value <- rt_complete
    rt_value[!rt_observed] <- NA_real_

    gaze_value <- gaze_complete
    gaze_value[!gaze_observed] <- NA_integer_

    person_ids <- sprintf(
        "P%04d",
        seq_len(n_person)
    )

    item_ids <- sprintf(
        "I%03d",
        seq_len(n_item)
    )

    d <- data.frame(
        person_id = person_ids[
            grid$person_index
        ],
        item_id = item_ids[
            grid$item_index
        ],
        response = response_value,
        rt = rt_value,
        gaze = gaze_value,
        stringsAsFactors = FALSE
    )

    complete <- data.frame(
        person_id = d$person_id,
        item_id = d$item_id,
        response = response_complete,
        rt = rt_complete,
        gaze = gaze_complete,
        response_probability = response_probability,
        log_rt_mean = log_rt_mean,
        gaze_mean = gaze_mean,
        stringsAsFactors = FALSE
    )

    truth <- list(
        n_person = n_person,
        n_item = n_item,
        mu_item = setNames(
            as.numeric(mu_item),
            c(
                "b",
                "beta",
                "m"
            )
        ),
        sd_person = setNames(
            as.numeric(sd_person),
            c(
                "theta",
                "tau",
                "omega"
            )
        ),
        cor_person = cor_person,
        sd_item = setNames(
            as.numeric(sd_item),
            c(
                "b",
                "beta",
                "m"
            )
        ),
        cor_item = cor_item,
        theta = person_effects[, "theta"],
        tau = person_effects[, "tau"],
        omega = person_effects[, "omega"],
        b = item_effects[, "b"],
        beta = item_effects[, "beta"],
        m = item_effects[, "m"],
        nu = nu,
        s = s,
        dropout = dropout,
        seed = as.integer(seed),
        generating_model = .ep10_m2_reference$likelihood_fidelity
    )

    out <- list(
        data = d,
        complete_data = complete,
        truth = truth,
        reference = .ep10_m2_reference,
        interpretation = paste(
            "Synthetic truth is retained for estimator recovery.",
            "Recovery under the generating model is not empirical validation."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_simulation",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_simulation <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_simulation>\n",
        "  persons: ", x$truth$n_person, "\n",
        "  items: ", x$truth$n_item, "\n",
        "  rows: ", nrow(x$data), "\n",
        "  dropout: ",
        paste(
            names(x$truth$dropout),
            sprintf("%.3f", x$truth$dropout),
            sep = "=",
            collapse = ", "
        ),
        "\n",
        "  seed: ", x$truth$seed, "\n",
        "  generating likelihood: ", x$truth$generating_model, "\n",
        sep = ""
    )
    invisible(x)
}

.ep10_m2_truth_map <- function(sim) {
    if (!inherits(sim, "eye_multimodal_m2_simulation")) {
        stop(
            "`sim` must be an eye_multimodal_m2_simulation.",
            call. = FALSE
        )
    }

    tr <- sim$truth
    out <- numeric()

    add <- function(name, value) {
        out[[name]] <<- as.numeric(value)
    }

    for (j in seq_along(tr$theta)) {
        add(
            paste0(
                "theta[",
                j,
                "]"
            ),
            tr$theta[[j]]
        )
        add(
            paste0(
                "tau[",
                j,
                "]"
            ),
            tr$tau[[j]]
        )
        add(
            paste0(
                "omega[",
                j,
                "]"
            ),
            tr$omega[[j]]
        )
    }

    for (i in seq_along(tr$b)) {
        add(
            paste0(
                "b[",
                i,
                "]"
            ),
            tr$b[[i]]
        )
        add(
            paste0(
                "beta[",
                i,
                "]"
            ),
            tr$beta[[i]]
        )
        add(
            paste0(
                "m[",
                i,
                "]"
            ),
            tr$m[[i]]
        )
        add(
            paste0(
                "nu[",
                i,
                "]"
            ),
            tr$nu[[i]]
        )
        add(
            paste0(
                "s[",
                i,
                "]"
            ),
            tr$s[[i]]
        )
    }

    for (k in seq_len(3L)) {
        add(
            paste0(
                "mu_item[",
                k,
                "]"
            ),
            tr$mu_item[[k]]
        )
        add(
            paste0(
                "sigma_person[",
                k,
                "]"
            ),
            tr$sd_person[[k]]
        )
        add(
            paste0(
                "sigma_item[",
                k,
                "]"
            ),
            tr$sd_item[[k]]
        )
    }

    for (r in seq_len(3L)) {
        for (c in seq_len(3L)) {
            add(
                paste0(
                    "corr_person[",
                    r,
                    ",",
                    c,
                    "]"
                ),
                tr$cor_person[r, c]
            )
            add(
                paste0(
                    "corr_item[",
                    r,
                    ",",
                    c,
                    "]"
                ),
                tr$cor_item[r, c]
            )
        }
    }

    out
}

.ep10_m2_recovery_family <- function(variable) {
    if (grepl("^(theta|tau|omega)\\[", variable)) {
        return("person_latent")
    }

    if (grepl("^(b|beta|m)\\[", variable)) {
        return("item_location")
    }

    if (grepl("^(nu|s)\\[", variable)) {
        return("item_dispersion")
    }

    if (grepl("^corr_person\\[", variable)) {
        return("person_correlation")
    }

    if (grepl("^corr_item\\[", variable)) {
        return("item_correlation")
    }

    "hyperparameter"
}

.ep10_m2_recovery_one <- function(
    replicate,
    simulation,
    fit
) {
    truth <- .ep10_m2_truth_map(
        simulation
    )

    variables <- c(
        "theta",
        "tau",
        "omega",
        "b",
        "beta",
        "m",
        "nu",
        "s",
        "mu_item",
        "sigma_person",
        "sigma_item",
        "corr_person",
        "corr_item"
    )

    sm <- fit$fit$summary(
        variables = variables,
        probs = c(
            0.025,
            0.5,
            0.975
        )
    )

    q025_name <- intersect(
        c(
            "q2.5",
            "q2.5%",
            "2.5%"
        ),
        names(sm)
    )

    q975_name <- intersect(
        c(
            "q97.5",
            "q97.5%",
            "97.5%"
        ),
        names(sm)
    )

    if (
        !length(q025_name) ||
        !length(q975_name)
    ) {
        stop(
            "CmdStanR summary did not return expected 2.5%/97.5% interval columns.",
            call. = FALSE
        )
    }

    keep <- sm$variable %in% names(truth)

    sm <- sm[
        keep,
        ,
        drop = FALSE
    ]

    # Exclude fixed unit diagonals of correlation matrices from recovery
    # summaries; they are constraints rather than estimated quantities.
    corr_match <- regexec(
        "^corr_(person|item)\\[([0-9]+),([0-9]+)\\]$",
        sm$variable
    )

    corr_parts <- regmatches(
        sm$variable,
        corr_match
    )

    is_fixed_diag <- vapply(
        corr_parts,
        function(z) {
            length(z) == 4L &&
                identical(z[[3L]], z[[4L]])
        },
        logical(1)
    )

    sm <- sm[
        !is_fixed_diag,
        ,
        drop = FALSE
    ]

    truth_value <- unname(
        truth[
            sm$variable
        ]
    )

    estimate <- sm$mean

    lower <- sm[[q025_name[[1L]]]]
    upper <- sm[[q975_name[[1L]]]]

    data.frame(
        replicate = as.integer(replicate),
        family = vapply(
            sm$variable,
            .ep10_m2_recovery_family,
            character(1)
        ),
        parameter = sm$variable,
        truth = truth_value,
        estimate = estimate,
        posterior_sd = sm$sd,
        lower = lower,
        upper = upper,
        error = estimate - truth_value,
        squared_error = (
            estimate - truth_value
        ) ^ 2,
        covered95 = (
            lower <= truth_value &
            upper >= truth_value
        ),
        stringsAsFactors = FALSE
    )
}

.ep10_m2_recovery_summary <- function(raw) {
    groups <- split(
        raw,
        raw$family
    )

    do.call(
        rbind,
        lapply(
            names(groups),
            function(nm) {
                z <- groups[[nm]]

                data.frame(
                    family = nm,
                    n = nrow(z),
                    bias = mean(
                        z$error,
                        na.rm = TRUE
                    ),
                    rmse = sqrt(
                        mean(
                            z$squared_error,
                            na.rm = TRUE
                        )
                    ),
                    mean_posterior_sd = mean(
                        z$posterior_sd,
                        na.rm = TRUE
                    ),
                    coverage95 = mean(
                        z$covered95,
                        na.rm = TRUE
                    ),
                    stringsAsFactors = FALSE
                )
            }
        )
    )
}

#' Run repeated M2 estimator recovery
#'
#' Repeatedly simulates from the M2 generating model, fits the M2
#' CmdStan reference estimator, and computes bias, RMSE, posterior
#' standard deviation, and 95% interval coverage for person, item,
#' dispersion, correlation, and hyperparameter families.
#'
#' This is intentionally computationally expensive and is not executed
#' during ordinary package tests.
#'
#' @param n_rep Number of simulation/fit replications.
#' @param n_person,n_item Simulation size.
#' @param dropout Channel dropout probabilities.
#' @param base_seed Base seed.
#' @param chains,parallel_chains,iter_warmup,iter_sampling CmdStan controls.
#' @param prior_profile Prior profile.
#' @param adapt_delta,max_treedepth,refresh CmdStan controls.
#' @return An `eye_multimodal_m2_recovery`.
#' @export
multimodal_m2_recovery <- function(
    n_rep = 10L,
    n_person = 100L,
    n_item = 10L,
    dropout = c(
        response = 0,
        rt = 0,
        gaze = 0
    ),
    base_seed = 20260814L,
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    prior_profile = c(
        "regularized",
        "paper_centered"
    ),
    adapt_delta = 0.95,
    max_treedepth = 12L,
    refresh = 0L
) {
    n_rep <- as.integer(n_rep)

    if (n_rep < 1L) {
        stop(
            "`n_rep` must be at least 1.",
            call. = FALSE
        )
    }

    prior_profile <- match.arg(
        prior_profile
    )

    .ep10_m2_require_backend()

    rows <- vector(
        "list",
        n_rep
    )

    diagnostics <- vector(
        "list",
        n_rep
    )

    for (r in seq_len(n_rep)) {
        sim_seed <- as.integer(
            base_seed + r - 1L
        )

        fit_seed <- as.integer(
            base_seed + 100000L + r - 1L
        )

        message(
            "M2 recovery replication ",
            r,
            "/",
            n_rep,
            " (simulation seed ",
            sim_seed,
            ", fit seed ",
            fit_seed,
            ")"
        )

        sim <- simulate_multimodal_m2(
            n_person = n_person,
            n_item = n_item,
            dropout = dropout,
            seed = sim_seed
        )

        fit <- fit_multimodal_m2(
            sim,
            prior_profile = prior_profile,
            chains = chains,
            parallel_chains = parallel_chains,
            iter_warmup = iter_warmup,
            iter_sampling = iter_sampling,
            seed = fit_seed,
            adapt_delta = adapt_delta,
            max_treedepth = max_treedepth,
            refresh = refresh
        )

        rows[[r]] <- .ep10_m2_recovery_one(
            r,
            sim,
            fit
        )

        da <- .ep10_m2_sampler_audit(
            fit
        )

        diagnostics[[r]] <- data.frame(
            replicate = r,
            max_rhat = da$max_rhat,
            min_ess_bulk = da$min_ess_bulk,
            min_ess_tail = da$min_ess_tail,
            divergences = da$divergences,
            max_treedepth_hits = da$max_treedepth_hits,
            stringsAsFactors = FALSE
        )
    }

    raw <- do.call(
        rbind,
        rows
    )

    diag <- do.call(
        rbind,
        diagnostics
    )

    summary <- .ep10_m2_recovery_summary(
        raw
    )

    out <- list(
        raw = raw,
        summary = summary,
        diagnostics = diag,
        design = list(
            n_rep = n_rep,
            n_person = as.integer(n_person),
            n_item = as.integer(n_item),
            dropout = dropout,
            base_seed = as.integer(base_seed),
            prior_profile = prior_profile
        ),
        reference = .ep10_m2_reference,
        interpretation = paste(
            "Recovery under the package generating model tests estimator behavior.",
            "It does not demonstrate robustness to misspecification or empirical validity."
        )
    )

    class(out) <- c(
        "eye_multimodal_m2_recovery",
        "list"
    )

    out
}

# S3 method registered in the hand-maintained NAMESPACE.
print.eye_multimodal_m2_recovery <- function(x, ...) {
    cat(
        "<eye_multimodal_m2_recovery>\n",
        "  replications: ", x$design$n_rep, "\n",
        "  persons/rep: ", x$design$n_person, "\n",
        "  items/rep: ", x$design$n_item, "\n",
        "  prior profile: ", x$design$prior_profile, "\n\n",
        sep = ""
    )
    print(
        x$summary,
        row.names = FALSE
    )
    invisible(x)
}
