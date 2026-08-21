# eyeprocess 0.11 - M4 evidence, identifiability, ablation, controls, sensitivity

.ep11_m4_status_rank <- c(PASS = 1L, PASS_WITH_CAUTION = 2L, NOT_EVALUATED = 3L, REVIEW = 4L, FAIL = 5L)

.ep11_m4_worst_status <- function(x) {
    x <- as.character(x)
    x <- x[x %in% names(.ep11_m4_status_rank)]
    if (!length(x)) return("NOT_EVALUATED")
    score <- .ep11_m4_status_rank[x]
    names(score)[which.max(score)]
}

.ep11_m4_check_row <- function(domain, criterion, status, value = NA, threshold = NA, message, recommendation = "") {
    data.frame(
        domain = domain,
        criterion = criterion,
        status = status,
        severity = switch(status, PASS = "none", PASS_WITH_CAUTION = "low", NOT_EVALUATED = "none", REVIEW = "moderate", FAIL = "high", "moderate"),
        value = if (length(value) == 1L) as.character(value) else paste(value, collapse = ","),
        threshold = if (length(threshold) == 1L) as.character(threshold) else paste(threshold, collapse = ","),
        message = message,
        recommendation = recommendation,
        stringsAsFactors = FALSE
    )
}

.ep11_m4_sampler_audit <- function(x) {
    vars <- c(
        "z_person", "sigma_person", "L_person", "z_item", "mu_item", "sigma_item", "L_item",
        "nu", "s", "sigma_pupil", "gamma_pupil", "theta", "tau", "omega", "rho", "b", "beta", "m", "kappa",
        "delta_rt", "delta_gaze", "delta_pupil", "init_intercept", "trans_intercept", "init_trait", "trans_trait"
    )
    s <- x$fit$summary(variables = vars)
    finite_rhat <- s$rhat[is.finite(s$rhat)]
    finite_bulk <- s$ess_bulk[is.finite(s$ess_bulk)]
    finite_tail <- s$ess_tail[is.finite(s$ess_tail)]
    dg <- tryCatch(as.data.frame(x$fit$diagnostic_summary()), error = function(e) NULL)
    get_diag <- function(candidates) {
        if (is.null(dg)) return(NULL)
        nm <- intersect(candidates, names(dg))
        if (!length(nm)) return(NULL)
        dg[[nm[[1L]]]]
    }
    div_values <- get_diag(c("num_divergent", "num_divergences", "divergent"))
    depth_values <- get_diag(c("num_max_treedepth", "max_treedepth"))
    ebfmi_values <- get_diag(c("ebfmi", "E-BFMI"))
    list(
        max_rhat = if (length(finite_rhat)) max(finite_rhat) else NA_real_,
        min_ess_bulk = if (length(finite_bulk)) min(finite_bulk) else NA_real_,
        min_ess_tail = if (length(finite_tail)) min(finite_tail) else NA_real_,
        divergences = if (is.null(div_values)) NA_integer_ else as.integer(sum(div_values, na.rm = TRUE)),
        max_treedepth_hits = if (is.null(depth_values)) NA_integer_ else as.integer(sum(depth_values, na.rm = TRUE)),
        min_ebfmi = if (is.null(ebfmi_values) || !any(is.finite(ebfmi_values))) NA_real_ else min(ebfmi_values[is.finite(ebfmi_values)])
    )
}

.ep11_m4_state_separation <- function(fit) {
    K <- fit$spec$n_states
    if (K <= 1L) return(Inf)
    eff <- .ep11_m4_state_effect_means(fit)
    mat <- cbind(eff$rt, eff$gaze, eff$pupil)
    min(stats::dist(mat))
}

.ep11_m4_transition_summary <- function(fit) {
    K <- fit$spec$n_states
    J <- length(fit$data$person_levels)
    A <- .ep11_m4_indexed_mean(fit, "transition_prob_person", c(J, K, K))
    if (!length(A) || any(!is.finite(A))) {
        return(list(person = A, mean = matrix(NA_real_, K, K), degenerate_fraction = NA_real_))
    }
    mean_A <- apply(A, c(2L, 3L), mean)
    deg <- mean(A < 0.02 | A > 0.98)
    list(person = A, mean = mean_A, degenerate_fraction = deg)
}

#' Audit M4 structural and posterior identifiability
#'
#' Evaluates sequence information, channel support, state occupancy, assignment
#' uncertainty, state separation, transition degeneracy, and HMC diagnostics.
#' The result is deliberately multi-criterion; M4 does not collapse validity to
#' a single undocumented boolean.
#'
#' @param x Data, M4 simulation, M4 fit, or internal prepared M4 data.
#' @param spec Optional M4 specification when `x` is not a fit.
#' @param include_posterior Whether to evaluate posterior criteria for a fit.
#' @param rhat_max,ess_min,ebfmi_min Sampler thresholds.
#' @param occupancy_min Minimum mean posterior state occupancy for non-null K.
#' @param entropy_fraction_review Review threshold relative to maximum entropy.
#' @return An `eye_multimodal_m4_identifiability` with machine-readable checks.
#' @export
audit_multimodal_m4_identifiability <- function(
    x,
    spec = NULL,
    include_posterior = TRUE,
    rhat_max = 1.05,
    ess_min = 100,
    ebfmi_min = 0.30,
    occupancy_min = 0.03,
    entropy_fraction_review = 0.80
) {
    fit <- if (inherits(x, "eye_multimodal_m4_fit")) x else NULL
    if (!is.null(fit)) {
        data <- fit$data
        spec <- fit$spec
    } else if (is.list(x) && !is.data.frame(x) && !is.null(x$raw) && !is.null(x$sequence)) {
        data <- x
        if (is.null(spec)) spec <- multimodal_m4_spec()
    } else {
        if (is.null(spec)) spec <- multimodal_m4_spec()
        data <- .ep11_m4_as_data(x, min_sequence_length = spec$min_sequence_length)
    }
    if (!inherits(spec, "eye_multimodal_m4_spec")) stop("`spec` must be an M4 specification.", call. = FALSE)

    seq_len <- data$sequence$length
    checks <- list()
    add <- function(...) checks[[length(checks) + 1L]] <<- .ep11_m4_check_row(...)
    add("sequence", "sequence_count", if (length(seq_len) >= 2L) "PASS" else "REVIEW", length(seq_len), ">=2 preferred", "Number of independent ordered sequences.", "Use multiple persons/sequences for population-level state inference.")
    add("sequence", "minimum_sequence_length", if (min(seq_len) >= spec$min_sequence_length) "PASS" else "FAIL", min(seq_len), paste0(">=", spec$min_sequence_length), "Shortest sequence relative to declared fitting requirement.", "Do not silently discard short sequences; revise design or explicit threshold.")
    add("sequence", "transition_count", if (data$sequence$n_transition >= max(20L, 5L * spec$n_states)) "PASS" else "REVIEW", data$sequence$n_transition, paste0(">=", max(20L, 5L * spec$n_states)), "Available within-sequence transitions.", "Transition parameters may be prior-dominated when few transitions are observed.")
    for (ch in c("response", spec$state_channels)) {
        n <- sum(data$observed[[ch]])
        add("data", paste0(ch, "_observed"), if (n > 0L) "PASS" else "FAIL", n, ">0", paste0("Observed ", ch, " measurements."), paste0("M4 cannot identify the requested ", ch, " contribution without observed data."))
    }
    miss <- vapply(c("rt", "gaze", "pupil"), function(ch) mean(!data$observed[[ch]]), numeric(1L))
    add("data", "maximum_process_missing_fraction", if (max(miss) <= 0.50) "PASS" else "REVIEW", max(miss), "<=0.50 preferred", "Largest process-channel missing fraction.", "Study missingness sensitivity; M4 reference fitting assumes ignorable missingness.")

    # Trait-conditioned Markov dynamics are deliberately REVIEW-gated.
    # Structural counts alone cannot establish posterior identification
    # of person-conditioned initial/transition state probabilities.
    trait_markov_gate <-
        spec$n_states > 1L &&
        identical(spec$transition_structure, "markov") &&
        length(spec$trait_conditioning) > 0L

    add(
        "model_complexity",
        "trait_conditioned_markov",
        if (trait_markov_gate) "REVIEW" else "PASS",
        if (trait_markov_gate) length(spec$trait_conditioning) else 0L,
        "0 transition traits for unconditional Markov",
        "Trait-conditioned Markov dynamics add person-level process slopes whose posterior identification cannot be established from structural counts alone.",
        "Treat this specification as gated: require satisfactory posterior R-hat/ESS, stable state occupancy across chains, and separated state emissions before interpretation."
    )

    posterior <- NULL
    if (!is.null(fit) && isTRUE(include_posterior)) {
        p <- .ep11_m4_state_prob_matrix(fit)
        occ <- colMeans(p)
        ent <- apply(p, 1L, .ep11_m4_entropy)
        max_ent <- if (fit$spec$n_states > 1L) log(fit$spec$n_states) else 0
        min_occ <- min(occ)
        sep <- .ep11_m4_state_separation(fit)
        tr <- .ep11_m4_transition_summary(fit)
        sampler <- .ep11_m4_sampler_audit(fit)

        if (fit$spec$n_states == 1L) {
            add("state_occupancy", "minimum_occupancy", "PASS", min_occ, "1 for K=1", "Formal K=1 null has one occupied state.", "None.")
            add("state_separation", "minimum_multichannel_separation", "NOT_EVALUATED", NA, "not applicable for K=1", "State separation is undefined for the null model.", "Compare K=1 against K>1 predictive evidence rather than inventing separation.")
        } else {
            add("state_occupancy", "minimum_occupancy", if (min_occ >= occupancy_min) "PASS" else "REVIEW", min_occ, paste0(">=", occupancy_min), "Smallest mean posterior state occupancy.", "Near-empty states can signal overfitting or weak identification.")
            add("state_separation", "minimum_multichannel_separation", if (is.finite(sep) && sep >= 0.15) "PASS" else "REVIEW", sep, ">=0.15 descriptive", "Minimum Euclidean separation across RT/gaze/pupil state-effect profiles.", "Low separation means numeric labels may partition nearly identical process distributions.")
        }
        ent_frac <- if (max_ent > 0) mean(ent) / max_ent else 0
        add("state_uncertainty", "mean_entropy_fraction", if (ent_frac <= entropy_fraction_review) "PASS" else "REVIEW", ent_frac, paste0("<=", entropy_fraction_review), "Mean posterior assignment entropy relative to the K-state maximum.", "High entropy should be reported as uncertainty, not converted into confident hard state labels.")
        add("transition_structure", "degenerate_transition_fraction", if (!is.finite(tr$degenerate_fraction)) "NOT_EVALUATED" else if (tr$degenerate_fraction <= 0.50) "PASS" else "REVIEW", tr$degenerate_fraction, "<=0.50 preferred", "Fraction of person-level transition probabilities below .02 or above .98.", "Extreme transition probabilities may reflect true persistence, sparse transitions, or weak identification; inspect context.")
        add("sampler", "divergences", if (is.na(sampler$divergences)) "NOT_EVALUATED" else if (sampler$divergences == 0L) "PASS" else "FAIL", sampler$divergences, "0", "Hamiltonian Monte Carlo divergences.", "Do not validate M4 while divergences remain unresolved.")
        add("sampler", "treedepth_hits", if (is.na(sampler$max_treedepth_hits)) "NOT_EVALUATED" else if (sampler$max_treedepth_hits == 0L) "PASS" else "REVIEW", sampler$max_treedepth_hits, "0 preferred", "Maximum-treedepth hits.", "Inspect geometry and increase treedepth only after model diagnostics.")
        add("sampler", "max_rhat", if (!is.finite(sampler$max_rhat)) "NOT_EVALUATED" else if (sampler$max_rhat <= rhat_max) "PASS" else "FAIL", sampler$max_rhat, paste0("<=", rhat_max), "Maximum finite R-hat over core measurement/state parameters.", "Do not interpret state structure until chains mix adequately.")
        add("sampler", "min_bulk_ess", if (!is.finite(sampler$min_ess_bulk)) "NOT_EVALUATED" else if (sampler$min_ess_bulk >= ess_min) "PASS" else "FAIL", sampler$min_ess_bulk, paste0(">=", ess_min), "Minimum bulk effective sample size.", "Increase effective sampling or simplify the state model.")
        add("sampler", "min_ebfmi", if (!is.finite(sampler$min_ebfmi)) "NOT_EVALUATED" else if (sampler$min_ebfmi >= ebfmi_min) "PASS" else "FAIL", sampler$min_ebfmi, paste0(">=", ebfmi_min), "Minimum E-BFMI across chains when available.", "Poor energy exploration invalidates computational confidence.")
        posterior <- list(probability = p, occupancy = occ, entropy = ent, separation = sep, transition = tr, sampler = sampler)
    }

    table <- do.call(rbind, checks)
    evaluated_status <- table$status[table$status != "NOT_EVALUATED"]
    overall <- if (length(evaluated_status)) .ep11_m4_worst_status(evaluated_status) else "NOT_EVALUATED"
    out <- list(
        overall = overall,
        checks = table,
        supported = !identical(overall, "FAIL"),
        sequence = data$sequence,
        missingness = miss,
        posterior = posterior,
        interpretation = .ep11_m4_reference$interpretation
    )
    class(out) <- c("eye_multimodal_m4_identifiability", "list")
    out
}

print.eye_multimodal_m4_identifiability <- function(x, ...) {
    cat("<eye_multimodal_m4_identifiability>\n", "  overall: ", x$overall, "\n", sep = "")
    print(x$checks, row.names = FALSE)
    invisible(x)
}

.ep11_m4_map_runs <- function(state, sequence_id) {
    split_idx <- split(seq_along(state), sequence_id)
    rows <- lapply(names(split_idx), function(s) {
        idx <- split_idx[[s]]
        r <- rle(state[idx])
        data.frame(sequence_id = s, state = r$values, run_length = r$lengths, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

.ep11_m4_weighted_channel_profile <- function(d, prob) {
    K <- ncol(prob)
    rows <- list(); z <- 1L
    for (channel in c("rt", "gaze", "pupil")) {
        y <- d[[channel]]
        if (channel == "rt") y <- ifelse(is.na(y), NA_real_, log(y))
        for (k in seq_len(K)) {
            ok <- is.finite(y) & is.finite(prob[, k])
            w <- prob[ok, k]
            value <- if (!any(ok) || sum(w) <= 0) NA_real_ else sum(w * y[ok]) / sum(w)
            rows[[z]] <- data.frame(state = k, channel = channel, posterior_weighted_mean = value, effective_weight = sum(w), stringsAsFactors = FALSE)
            z <- z + 1L
        }
    }
    do.call(rbind, rows)
}

#' Summarize M4 latent-state uncertainty and dynamics
#'
#' Returns posterior state probabilities, entropy, occupancy, MAP-run summaries,
#' transition probabilities, and posterior-weighted process-channel profiles.
#' MAP states are explicitly secondary summaries of posterior probabilities.
#'
#' @param x M4 fit or M4 simulation. Simulation diagnostics use known synthetic
#'   truth and are labelled accordingly.
#' @return An `eye_multimodal_m4_states` object.
#' @export
multimodal_m4_state_diagnostics <- function(x) {
    if (inherits(x, "eye_multimodal_m4_simulation")) {
        K <- x$truth$n_states
        p <- matrix(0, nrow(x$data), K)
        p[cbind(seq_len(nrow(x$data)), x$truth$state)] <- 1
        source <- "synthetic_truth"
        transition <- apply(x$truth$transition_prob_person, c(2L, 3L), mean)
        d <- x$data
    } else if (inherits(x, "eye_multimodal_m4_fit")) {
        K <- x$spec$n_states
        p <- .ep11_m4_state_prob_matrix(x)
        source <- "posterior"
        transition <- .ep11_m4_transition_summary(x)$mean
        d <- x$data$raw
    } else {
        stop("`x` must be an M4 fit or M4 simulation.", call. = FALSE)
    }
    map <- max.col(p, ties.method = "first")
    entropy <- apply(p, 1L, .ep11_m4_entropy)
    occ <- colMeans(p)
    state_table <- data.frame(
        source_row = seq_len(nrow(d)),
        person_id = d$person_id,
        item_id = d$item_id,
        sequence_id = d$sequence_id,
        trial_index = d$trial_index,
        MAP_state = map,
        posterior_entropy = entropy,
        stringsAsFactors = FALSE
    )
    for (k in seq_len(K)) state_table[[paste0("state_", k, "_probability")]] <- p[, k]
    runs <- .ep11_m4_map_runs(map, d$sequence_id)
    occupancy <- data.frame(state = seq_len(K), mean_probability = occ, MAP_fraction = tabulate(map, nbins = K) / length(map))
    transition_long <- expand.grid(from = seq_len(K), to = seq_len(K))
    transition_long$probability <- as.numeric(transition)
    channel_profile <- .ep11_m4_weighted_channel_profile(d, p)
    participant <- do.call(rbind, lapply(split(seq_len(nrow(d)), d$person_id), function(idx) {
        z <- colMeans(p[idx, , drop = FALSE])
        data.frame(person_id = d$person_id[idx[[1L]]], state = seq_len(K), mean_probability = z, stringsAsFactors = FALSE)
    }))
    item <- do.call(rbind, lapply(split(seq_len(nrow(d)), d$item_id), function(idx) {
        z <- colMeans(p[idx, , drop = FALSE])
        data.frame(item_id = d$item_id[idx[[1L]]], state = seq_len(K), mean_probability = z, stringsAsFactors = FALSE)
    }))
    out <- list(
        source = source,
        probability = state_table,
        occupancy = occupancy,
        transition = transition_long,
        transition_matrix = transition,
        runs = runs,
        channel_profile = channel_profile,
        participant = participant,
        item = item,
        summary = list(
            mean_entropy = mean(entropy),
            median_entropy = stats::median(entropy),
            mean_run_length = mean(runs$run_length),
            switching_rate = if (nrow(d) > length(unique(d$sequence_id))) sum(diff(map) != 0 & d$sequence_id[-1L] == d$sequence_id[-nrow(d)]) / max(1, nrow(d) - length(unique(d$sequence_id))) else 0
        ),
        interpretation = .ep11_m4_reference$interpretation
    )
    class(out) <- c("eye_multimodal_m4_states", "list")
    out
}

print.eye_multimodal_m4_states <- function(x, ...) {
    cat(
        "<eye_multimodal_m4_states>\n",
        "  source: ", x$source, "\n",
        "  states: ", nrow(x$occupancy), "\n",
        "  mean entropy: ", format(x$summary$mean_entropy, digits = 4), "\n",
        "  mean MAP run length: ", format(x$summary$mean_run_length, digits = 4), "\n",
        "  boundary: MAP labels are secondary summaries; posterior probabilities carry uncertainty\n",
        sep = ""
    )
    invisible(x)
}

.ep11_m4_rep_mean <- function(fit, variable) {
    z <- posterior::as_draws_matrix(fit$fit$draws(variables = variable))
    if (!ncol(z)) return(numeric())
    colMeans(z)
}

.ep11_m4_lag1 <- function(x, sequence_id) {
    vals <- numeric(); z <- 1L
    for (idx in split(seq_along(x), sequence_id)) {
        y <- x[idx]
        ok <- is.finite(y)
        y <- y[ok]
        if (length(y) >= 3L && stats::sd(y) > 0) {
            vals[[z]] <- suppressWarnings(stats::cor(head(y, -1L), tail(y, -1L)))
            z <- z + 1L
        }
    }
    if (!length(vals)) NA_real_ else mean(vals, na.rm = TRUE)
}

#' Posterior predictive checks for M4 measurement and sequential behavior
#'
#' Compares observed process summaries with posterior-replicated summaries and
#' reports replicated latent-state dynamics separately. Because states are
#' latent, inferred state labels are never treated as observed ground truth.
#'
#' @param x An M4 fit.
#' @return An `eye_multimodal_m4_ppc`.
#' @export
multimodal_m4_ppc <- function(x) {
    if (!inherits(x, "eye_multimodal_m4_fit")) stop("`x` must be an M4 fit.", call. = FALSE)
    d <- x$data$raw
    rt_rep <- exp(.ep11_m4_rep_mean(x, "log_rt_rep"))
    gaze_rep <- .ep11_m4_rep_mean(x, "gaze_rep")
    pupil_rep_model <- .ep11_m4_rep_mean(x, "pupil_rep")
    pupil_rep <- if (identical(x$data$pupil_transform$mode, "z")) {
        x$data$pupil_transform$center + x$data$pupil_transform$scale * pupil_rep_model
    } else pupil_rep_model
    response_rep <- .ep11_m4_rep_mean(x, "y_rep")

    summarise <- function(obs, rep, channel) {
        ok <- is.finite(obs) & is.finite(rep)
        data.frame(
            channel = channel,
            observed_mean = if (any(ok)) mean(obs[ok]) else NA_real_,
            replicated_mean = if (any(ok)) mean(rep[ok]) else NA_real_,
            observed_sd = if (sum(ok) > 1L) stats::sd(obs[ok]) else NA_real_,
            replicated_sd = if (sum(ok) > 1L) stats::sd(rep[ok]) else NA_real_,
            mean_difference = if (any(ok)) mean(rep[ok]) - mean(obs[ok]) else NA_real_,
            stringsAsFactors = FALSE
        )
    }
    channels <- rbind(
        summarise(d$response, response_rep, "response"),
        summarise(d$rt, rt_rep, "rt"),
        summarise(d$gaze, gaze_rep, "gaze"),
        summarise(d$pupil, pupil_rep, "pupil")
    )
    dynamics <- data.frame(
        channel = c("log_rt", "gaze", "pupil"),
        observed_lag1 = c(.ep11_m4_lag1(log(d$rt), d$sequence_id), .ep11_m4_lag1(d$gaze, d$sequence_id), .ep11_m4_lag1(d$pupil, d$sequence_id)),
        replicated_lag1 = c(.ep11_m4_lag1(log(rt_rep), d$sequence_id), .ep11_m4_lag1(gaze_rep, d$sequence_id), .ep11_m4_lag1(pupil_rep, d$sequence_id)),
        stringsAsFactors = FALSE
    )
    state_draws <- posterior::as_draws_matrix(x$fit$draws(variables = "state_rep"))
    # Preserve trial index order explicitly rather than averaging numeric state labels.
    idx <- as.integer(sub("^state_rep\\[([0-9]+)\\]$", "\\1", colnames(state_draws)))
    state_draws <- state_draws[, order(idx), drop = FALSE]
    keep <- unique(round(seq(1, nrow(state_draws), length.out = min(200L, nrow(state_draws)))))
    dyn_rep <- t(vapply(keep, function(r) {
        state_rep <- as.integer(state_draws[r, ])
        rep_runs <- .ep11_m4_map_runs(state_rep, d$sequence_id)
        denom <- max(1, nrow(d) - length(unique(d$sequence_id)))
        c(
            mean_run_length = mean(rep_runs$run_length),
            switching_rate = sum(diff(state_rep) != 0 & d$sequence_id[-1L] == d$sequence_id[-nrow(d)]) / denom
        )
    }, numeric(2L)))
    inferred <- multimodal_m4_state_diagnostics(x)
    state_dynamics <- data.frame(
        metric = c("mean_run_length", "switching_rate"),
        inferred_posterior_summary = c(inferred$summary$mean_run_length, inferred$summary$switching_rate),
        replicated_trajectory_summary = colMeans(dyn_rep),
        stringsAsFactors = FALSE
    )
    out <- list(
        channels = channels,
        dynamics = dynamics,
        state_dynamics = state_dynamics,
        interpretation = paste(
            "PPC evaluates whether replicated measurement/process summaries resemble observed summaries.",
            "Latent-state trajectories are model-generated quantities, not observed states."
        )
    )
    class(out) <- c("eye_multimodal_m4_ppc", "list")
    out
}

print.eye_multimodal_m4_ppc <- function(x, ...) {
    cat("<eye_multimodal_m4_ppc>\n  measurement/process checks:\n")
    print(x$channels, row.names = FALSE)
    cat("\n  sequential checks:\n")
    print(x$dynamics, row.names = FALSE)
    invisible(x)
}

.ep11_m4_ablation_definitions <- function(include_channel_ablations = FALSE) {
    base <- data.frame(
        model = c("M3", "M4_K1", "M4_K2", "M4_K2_NO_TRAIT", "M4_K2_IID"),
        K = c(NA, 1L, 2L, 2L, 2L),
        transition = c(NA, "markov", "markov", "markov", "iid"),
        traits = c(NA, "none", "theta+tau", "none", "theta+tau"),
        state_channels = c(NA, "rt+gaze+pupil", "rt+gaze+pupil", "rt+gaze+pupil", "rt+gaze+pupil"),
        question = c(
            "validated M3 baseline", "formal K=1 null", "reference M4 increment",
            "does trait conditioning matter?", "does sequential dependence matter?"
        ),
        stringsAsFactors = FALSE
    )
    if (isTRUE(include_channel_ablations)) {
        extra <- data.frame(
            model = c("M4_RT_ONLY", "M4_NO_GAZE", "M4_NO_PUPIL"),
            K = 2L, transition = "markov", traits = "theta+tau",
            state_channels = c("rt", "rt+pupil", "rt+gaze"),
            question = c("state evidence from RT only", "state evidence without gaze", "state evidence without pupil"),
            stringsAsFactors = FALSE
        )
        base <- rbind(base, extra)
    }
    base
}

#' Plan or fit the focused M3-to-M4 ablation set
#'
#' The default is plan-only. The essential lattice compares M3, formal K=1,
#' reference K=2, K=2 without trait conditioning, and K=2 with iid states.
#' Optional RT-anchored channel ablations are available but are not part of the default
#' development burden. A no-RT K>1 model is intentionally excluded because the
#' reference label-identification policy orders RT state deviations.
#'
#' @param x Data or M4 simulation.
#' @param run Whether to fit models. Default `FALSE`.
#' @param include_channel_ablations Add RT-only, no-gaze, and no-pupil state models.
#' @param m3_fit,m4_fit Optional already fitted baseline/reference objects to reuse.
#' @param fit_args Sampling arguments shared across new fits.
#' @return An `eye_multimodal_m4_ablation`.
#' @export
multimodal_m4_ablation <- function(x, run = FALSE, include_channel_ablations = FALSE, m3_fit = NULL, m4_fit = NULL, fit_args = list()) {
    defs <- .ep11_m4_ablation_definitions(include_channel_ablations)
    if (!isTRUE(run)) {
        out <- list(definitions = defs, fits = NULL, executed = FALSE, target = "response-target predictive evidence")
        class(out) <- c("eye_multimodal_m4_ablation", "list")
        return(out)
    }
    dat <- .ep11_m4_unwrap_data(x)
    fits <- list()
    if (!is.null(m3_fit)) {
        if (!inherits(m3_fit, "eye_multimodal_m3_fit")) stop("`m3_fit` must be an M3 fit.", call. = FALSE)
        fits$M3 <- m3_fit
    } else {
        fits$M3 <- do.call(fit_multimodal_m3, utils::modifyList(list(x = dat), fit_args))
    }
    fit_variant <- function(K, transition = "markov", traits = c("theta", "tau"), channels = c("rt", "gaze", "pupil")) {
        spec <- multimodal_m4_spec(n_states = K, transition_structure = transition, trait_conditioning = traits, state_channels = channels)
        do.call(fit_multimodal_m4, utils::modifyList(list(x = dat, spec = spec), fit_args))
    }
    fits$M4_K1 <- fit_variant(1L, traits = character())
    if (!is.null(m4_fit)) {
        if (!inherits(m4_fit, "eye_multimodal_m4_fit")) stop("`m4_fit` must be an M4 fit.", call. = FALSE)
        fits$M4_K2 <- m4_fit
    } else fits$M4_K2 <- fit_variant(2L)
    fits$M4_K2_NO_TRAIT <- fit_variant(2L, traits = character())
    fits$M4_K2_IID <- fit_variant(2L, transition = "iid")
    if (isTRUE(include_channel_ablations)) {
        fits$M4_RT_ONLY <- fit_variant(2L, channels = c("rt"))
        fits$M4_NO_GAZE <- fit_variant(2L, channels = c("rt", "pupil"))
        fits$M4_NO_PUPIL <- fit_variant(2L, channels = c("rt", "gaze"))
    }
    out <- list(definitions = defs, fits = fits, executed = TRUE, target = "response-target predictive evidence")
    class(out) <- c("eye_multimodal_m4_ablation", "list")
    out
}

print.eye_multimodal_m4_ablation <- function(x, ...) {
    cat("<eye_multimodal_m4_ablation>\n", "  executed: ", isTRUE(x$executed), "\n", "  target: ", x$target, "\n\n", sep = "")
    print(x$definitions, row.names = FALSE)
    invisible(x)
}

.ep11_m4_response_loo <- function(fit) {
    .ep10_m2_require_backend(require_loo = TRUE)
    ll <- as.array(fit$fit$draws("log_lik_response", format = "draws_array"))
    loo::loo(ll, r_eff = loo::relative_eff(exp(ll)))
}

.ep11_m4_theta_variance <- function(fit) {
    z <- posterior::as_draws_matrix(fit$fit$draws(variables = "theta"))
    apply(z, 2L, stats::var)
}

.ep11_m4_pair_delta <- function(a, b) {
    if (length(a) != length(b)) stop("Pointwise LOO vectors are not commensurate.", call. = FALSE)
    d <- a - b
    c(delta = sum(d), se = sqrt(length(d) * stats::var(d)))
}

#' Quantify incremental response-target information supplied by M4 state structure
#'
#' Uses commensurate response-target PSIS-LOO and ability-posterior uncertainty to
#' compare M3 with focused M4 variants. It explicitly allows no benefit,
#' redundancy, or destabilization and does not sum channel Fisher information.
#'
#' @param x Executed M4 ablation object.
#' @param decisive_z Descriptive absolute delta/SE threshold.
#' @return An `eye_multimodal_m4_information`.
#' @export
multimodal_m4_process_information <- function(x, decisive_z = 2) {
    if (!inherits(x, "eye_multimodal_m4_ablation") || !isTRUE(x$executed)) {
        stop("`x` must be an executed `multimodal_m4_ablation()` result.", call. = FALSE)
    }
    required <- c("M3", "M4_K1", "M4_K2", "M4_K2_NO_TRAIT", "M4_K2_IID")
    missing <- setdiff(required, names(x$fits))
    if (length(missing)) stop("Focused M4 information comparison is missing fits: ", paste(missing, collapse = ", "), call. = FALSE)

    loos <- lapply(x$fits[required], function(f) if (inherits(f, "eye_multimodal_m3_fit")) .ep10_m3_response_loo(f) else .ep11_m4_response_loo(f))
    point <- lapply(loos, function(z) as.numeric(z$pointwise[, "elpd_loo"]))
    elpd <- vapply(loos, function(z) z$estimates["elpd_loo", "Estimate"], numeric(1L))
    se <- vapply(loos, function(z) z$estimates["elpd_loo", "SE"], numeric(1L))
    kmax <- vapply(loos, function(z) {
        k <- tryCatch(loo::pareto_k_values(z), error = function(e) numeric())
        if (!length(k) || !any(is.finite(k))) NA_real_ else max(k[is.finite(k)])
    }, numeric(1L))
    theta_var <- lapply(x$fits[required], function(f) if (inherits(f, "eye_multimodal_m3_fit")) .ep10_m3_theta_variance(f) else .ep11_m4_theta_variance(f))
    mean_var <- vapply(theta_var, mean, numeric(1L))
    tab <- data.frame(
        model = required,
        response_elpd_loo = unname(elpd),
        response_elpd_se = unname(se),
        delta_elpd_vs_M3 = unname(elpd - elpd[["M3"]]),
        mean_theta_posterior_variance = unname(mean_var),
        theta_variance_reduction_vs_M3 = 1 - unname(mean_var / mean_var[["M3"]]),
        max_pareto_k = unname(kmax),
        stringsAsFactors = FALSE
    )
    comparisons <- data.frame(
        contrast = c("M3_to_M4_K1", "M3_to_M4_K2", "K1_to_K2", "NO_TRAIT_to_K2", "IID_to_K2"),
        from = c("M3", "M3", "M4_K1", "M4_K2_NO_TRAIT", "M4_K2_IID"),
        to = c("M4_K1", "M4_K2", "M4_K2", "M4_K2", "M4_K2"),
        stringsAsFactors = FALSE
    )
    stats <- t(vapply(seq_len(nrow(comparisons)), function(i) .ep11_m4_pair_delta(point[[comparisons$to[[i]]]], point[[comparisons$from[[i]]]]), numeric(2L)))
    comparisons$delta_response_elpd <- stats[, "delta"]
    comparisons$se <- stats[, "se"]
    comparisons$z <- comparisons$delta_response_elpd / comparisons$se
    comparisons$evidence <- ifelse(
        !is.finite(comparisons$z), "insufficient_evidence",
        ifelse(comparisons$z >= decisive_z, "supports_incremental_state_information",
            ifelse(comparisons$z <= -decisive_z, "state_structure_destabilizing", "no_clear_incremental_state_information"))
    )
    main <- comparisons[comparisons$contrast == "M3_to_M4_K2", , drop = FALSE]
    verdict <- main$evidence[[1L]]
    if (identical(verdict, "no_clear_incremental_state_information")) {
        k1 <- comparisons$evidence[comparisons$contrast == "K1_to_K2"]
        if (length(k1) && identical(k1, "no_clear_incremental_state_information")) verdict <- "state_structure_redundant"
    }
    state_uncertainty <- lapply(x$fits[c("M4_K1", "M4_K2", "M4_K2_NO_TRAIT", "M4_K2_IID")], function(f) multimodal_m4_state_diagnostics(f)$summary)
    out <- list(
        table = tab,
        comparisons = comparisons,
        verdict = verdict,
        loo = loos,
        theta_variance = theta_var,
        state_uncertainty = state_uncertainty,
        target = "scored-response log predictive density and ability uncertainty",
        interpretation = paste(
            "M4 process information is incremental predictive/uncertainty evidence under fitted models.",
            "It is noncausal, non-additive across channels, and does not establish psychological state validity."
        )
    )
    class(out) <- c("eye_multimodal_m4_information", "list")
    out
}

print.eye_multimodal_m4_information <- function(x, ...) {
    cat("<eye_multimodal_m4_information>\n", "  target: ", x$target, "\n", "  M3 -> M4 verdict: ", x$verdict, "\n\n", sep = "")
    print(x$comparisons, row.names = FALSE)
    invisible(x)
}

.ep11_m4_shuffle_joint_within <- function(d, group, columns) {
    out <- d
    for (idx in split(seq_len(nrow(d)), d[[group]])) {
        if (length(idx) > 1L) {
            p <- sample(idx, length(idx), replace = FALSE)
            out[idx, columns] <- d[p, columns, drop = FALSE]
        }
    }
    out
}

.ep11_m4_shuffle_independent_within <- function(d, group, columns) {
    out <- d
    for (idx in split(seq_len(nrow(d)), d[[group]])) {
        if (length(idx) > 1L) {
            for (nm in columns) out[idx, nm] <- sample(d[idx, nm], length(idx), replace = FALSE)
        }
    }
    out
}

#' Construct M4 temporal, nuisance, device, and overfitting negative controls
#'
#' Controls are transformations/stress designs by default and therefore do not
#' incur backend fitting. Set `run = TRUE` only when explicit fitted comparisons
#' are needed.
#'
#' @param x Data or M4 simulation.
#' @param controls Control names.
#' @param seed Deterministic seed.
#' @param run Whether to fit the generated controls.
#' @param fit_args Arguments for optional M4 fits.
#' @return An `eye_multimodal_m4_negative_controls`.
#' @export
multimodal_m4_negative_controls <- function(
    x,
    controls = c("order_shuffle", "process_shuffle", "state_independent", "nuisance_pseudostate", "device_session_pseudostate", "overfit_state_count"),
    seed = 20260820L,
    run = FALSE,
    fit_args = list()
) {
    allowed <- c("order_shuffle", "process_shuffle", "state_independent", "nuisance_pseudostate", "device_session_pseudostate", "overfit_state_count")
    if (any(!controls %in% allowed)) stop("Unknown M4 negative control.", call. = FALSE)
    set.seed(as.integer(seed))
    base <- .ep11_m4_unwrap_data(x)
    required <- c("person_id", "sequence_id", "trial_index", "rt", "gaze", "pupil")
    if (length(setdiff(required, names(base)))) stop("M4 negative controls require standard M4 sequence/process columns.", call. = FALSE)
    data_sets <- list(); metadata <- list()
    for (ctrl in controls) {
        if (ctrl == "order_shuffle") {
            data_sets[[ctrl]] <- .ep11_m4_shuffle_joint_within(base, "sequence_id", c("rt", "gaze", "pupil"))
            metadata[[ctrl]] <- "Destroys temporal alignment while preserving within-sequence joint process values."
        } else if (ctrl == "process_shuffle") {
            data_sets[[ctrl]] <- .ep11_m4_shuffle_independent_within(base, "person_id", c("rt", "gaze", "pupil"))
            metadata[[ctrl]] <- "Destroys cross-channel and temporal alignment while preserving person-level marginals."
        } else if (ctrl == "state_independent") {
            data_sets[[ctrl]] <- .ep11_m4_shuffle_joint_within(base, "person_id", c("rt", "gaze", "pupil"))
            metadata[[ctrl]] <- "Removes ordered state dependence while retaining person-level multichannel bundles."
        } else if (ctrl == "nuisance_pseudostate") {
            z <- base
            block <- ave(seq_len(nrow(z)), z$sequence_id, FUN = function(q) rep(c(-1, 1), length.out = length(q)))
            z$luminance <- as.numeric(block) + stats::rnorm(nrow(z), 0, 0.1)
            z$pupil <- z$pupil - 0.7 * z$luminance
            data_sets[[ctrl]] <- z
            metadata[[ctrl]] <- "Creates block-like pupil structure fully explainable by recorded luminance nuisance."
        } else if (ctrl == "device_session_pseudostate") {
            z <- base
            if (!"device" %in% names(z)) z$device <- rep(c("device_A", "device_B"), length.out = nrow(z))
            shift <- ifelse(z$device == "device_B", 0.65, -0.2)
            z$pupil <- z$pupil + shift
            z$gaze <- as.integer(pmax(0, round(z$gaze * exp(ifelse(z$device == "device_B", 0.15, -0.05)))))
            data_sets[[ctrl]] <- z
            metadata[[ctrl]] <- "Creates device-linked process structure that should trigger context/confounding review."
        } else {
            data_sets[[ctrl]] <- base
            metadata[[ctrl]] <- "Fits one more state than the reference candidate to test state proliferation/overfitting."
        }
    }
    fits <- NULL
    if (isTRUE(run)) {
        fits <- list()
        for (ctrl in names(data_sets)) {
            K <- if (ctrl == "overfit_state_count") 3L else 2L
            spec <- multimodal_m4_spec(n_states = K)
            fits[[ctrl]] <- do.call(fit_multimodal_m4, utils::modifyList(list(x = data_sets[[ctrl]], spec = spec), fit_args))
        }
    }
    out <- list(controls = controls, data = data_sets, rationale = metadata, fits = fits, executed = isTRUE(run), seed = as.integer(seed), interpretation = .ep11_m4_reference$interpretation)
    class(out) <- c("eye_multimodal_m4_negative_controls", "list")
    out
}

print.eye_multimodal_m4_negative_controls <- function(x, ...) {
    cat("<eye_multimodal_m4_negative_controls>\n", "  controls: ", length(x$controls), "\n", "  fitted: ", isTRUE(x$executed), "\n", sep = "")
    for (nm in x$controls) cat("  - ", nm, ": ", x$rationale[[nm]], "\n", sep = "")
    invisible(x)
}

#' Plan or run M4 state-count and modelling sensitivity analyses
#'
#' The default returns a transparent sensitivity design without fitting. It
#' covers K, priors, trait conditioning, transition structure, state channels,
#' nuisance adjustment, and sequence-length threshold. No automatic `best_K`
#' is declared.
#'
#' @param x Data or simulation.
#' @param n_states Candidate state counts, default 1:4.
#' @param run Whether to execute K-sensitivity fits. Other sensitivity dimensions
#'   remain explicit design rows rather than an automatic combinatorial grid.
#' @param fit_args Optional fitting arguments.
#' @return An `eye_multimodal_m4_sensitivity`.
#' @export
multimodal_m4_sensitivity <- function(x, n_states = 1:4, run = FALSE, fit_args = list()) {
    n_states <- sort(unique(as.integer(n_states)))
    if (!length(n_states) || any(is.na(n_states) | n_states < 1L | n_states > 4L)) stop("Candidate `n_states` must lie in 1:4.", call. = FALSE)
    design <- rbind(
        data.frame(dimension = "state_count", setting = paste0("K=", n_states), run_by_default = TRUE, stringsAsFactors = FALSE),
        data.frame(dimension = c("prior", "trait_conditioning", "transition_structure", "state_channels", "nuisance", "min_sequence_length"),
                   setting = c("regularized vs paper_centered", "theta+tau vs none/extended", "markov vs iid", "full vs channel omission", "adjusted vs justified exclusions", "2 vs stricter threshold"),
                   run_by_default = FALSE, stringsAsFactors = FALSE)
    )
    fits <- NULL; diagnostics <- NULL
    if (isTRUE(run)) {
        fits <- list(); rows <- list()
        for (i in seq_along(n_states)) {
            K <- n_states[[i]]
            spec <- multimodal_m4_spec(n_states = K)
            fits[[paste0("K", K)]] <- do.call(fit_multimodal_m4, utils::modifyList(list(x = x, spec = spec), fit_args))
            id <- audit_multimodal_m4_identifiability(fits[[paste0("K", K)]])
            st <- multimodal_m4_state_diagnostics(fits[[paste0("K", K)]])
            rows[[i]] <- data.frame(
                K = K,
                convergence = id$overall,
                occupancy_min = min(st$occupancy$mean_probability),
                occupancy_max = max(st$occupancy$mean_probability),
                mean_assignment_entropy = st$summary$mean_entropy,
                state_separation = .ep11_m4_state_separation(fits[[paste0("K", K)]]),
                transition_degeneracy = .ep11_m4_transition_summary(fits[[paste0("K", K)]])$degenerate_fraction,
                stringsAsFactors = FALSE
            )
        }
        diagnostics <- do.call(rbind, rows)
    }
    out <- list(design = design, fits = fits, diagnostics = diagnostics, executed = isTRUE(run), evidence_preference = "NOT_EVALUATED", interpretation = paste(.ep11_m4_reference$interpretation, "State count is a sensitivity dimension, not a known truth in empirical data."))
    class(out) <- c("eye_multimodal_m4_sensitivity", "list")
    out
}

print.eye_multimodal_m4_sensitivity <- function(x, ...) {
    cat("<eye_multimodal_m4_sensitivity>\n", "  executed K fits: ", isTRUE(x$executed), "\n", "  evidence preference: ", x$evidence_preference, "\n\n", sep = "")
    if (isTRUE(x$executed)) print(x$diagnostics, row.names = FALSE) else print(x$design, row.names = FALSE)
    invisible(x)
}

#' Validate M4 data, computation, state behavior, and evidence
#'
#' Combines structural identifiability, sampler diagnostics, state uncertainty,
#' PPC, and optionally supplied information/negative-control/sensitivity/recovery
#' evidence into domain-specific statuses. `PASS` means the declared checks pass;
#' it never means that state labels have substantive psychological validity.
#'
#' @param x M4 fit.
#' @param information Optional M4 information object.
#' @param negative_controls Optional M4 negative-controls object.
#' @param sensitivity Optional M4 sensitivity object.
#' @param recovery Optional M4 recovery object.
#' @param include_ppc Whether to compute PPC summaries.
#' @return An `eye_multimodal_m4_validation`.
#' @export
validate_multimodal_m4 <- function(x, information = NULL, negative_controls = NULL, sensitivity = NULL, recovery = NULL, include_ppc = TRUE) {
    if (!inherits(x, "eye_multimodal_m4_fit")) stop("`x` must be an M4 fit.", call. = FALSE)
    ident <- audit_multimodal_m4_identifiability(x)
    states <- multimodal_m4_state_diagnostics(x)
    ppc <- if (isTRUE(include_ppc)) multimodal_m4_ppc(x) else NULL
    domains <- data.frame(
        domain = c("data_sequence", "identification_sampler", "state_uncertainty", "ppc", "incremental_information", "negative_controls", "sensitivity", "recovery"),
        status = c(
            .ep11_m4_worst_status(ident$checks$status[ident$checks$domain %in% c("data", "sequence")]),
            .ep11_m4_worst_status(ident$checks$status[ident$checks$domain %in% c("sampler", "state_occupancy", "state_separation", "transition_structure")]),
            if (is.finite(states$summary$mean_entropy)) "PASS" else "REVIEW",
            if (is.null(ppc)) "NOT_EVALUATED" else "PASS_WITH_CAUTION",
            if (is.null(information)) "NOT_EVALUATED" else if (information$verdict == "state_structure_destabilizing") "REVIEW" else "PASS_WITH_CAUTION",
            if (is.null(negative_controls) || !isTRUE(negative_controls$executed)) "NOT_EVALUATED" else "PASS_WITH_CAUTION",
            if (is.null(sensitivity) || !isTRUE(sensitivity$executed)) "NOT_EVALUATED" else "PASS_WITH_CAUTION",
            if (is.null(recovery) || !isTRUE(recovery$executed)) "NOT_EVALUATED" else "PASS_WITH_CAUTION"
        ),
        stringsAsFactors = FALSE
    )
    overall <- .ep11_m4_worst_status(domains$status[domains$status != "NOT_EVALUATED"])
    out <- list(
        overall = overall,
        domains = domains,
        identifiability = ident,
        states = states,
        ppc = ppc,
        information = information,
        negative_controls = negative_controls,
        sensitivity = sensitivity,
        recovery = recovery,
        interpretation = paste(
            .ep11_m4_reference$interpretation,
            "Validation categories concern software/computational/model behavior; state detection is not construct validation."
        )
    )
    class(out) <- c("eye_multimodal_m4_validation", "list")
    out
}

print.eye_multimodal_m4_validation <- function(x, ...) {
    cat("<eye_multimodal_m4_validation>\n", "  overall: ", x$overall, "\n\n", sep = "")
    print(x$domains, row.names = FALSE)
    cat("\n  boundary: PASS does not confer psychological meaning on latent states.\n")
    invisible(x)
}
