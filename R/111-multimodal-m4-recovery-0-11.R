# eyeprocess 0.11 - M4 simulation, state alignment, and deterministic recovery

.ep11_m4_softmax <- function(x) {
    x <- as.numeric(x)
    z <- exp(x - max(x))
    z / sum(z)
}

.ep11_m4_corr_draw <- function(n, mu, sigma, corr, names = NULL) {
    p <- length(mu)
    S <- diag(sigma, p) %*% corr %*% diag(sigma, p)
    L <- chol(S + diag(1e-10, p))
    z <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
    out <- sweep(z %*% L, 2L, mu, "+")
    if (!is.null(names)) colnames(out) <- names
    out
}

.ep11_m4_transition_array <- function(person_traits, K, scenario, trait_strength = 0.45) {
    J <- nrow(person_traits)
    A <- array(0, dim = c(J, K, K))
    pi <- matrix(1 / K, nrow = J, ncol = K)
    if (K == 1L) {
        A[, 1L, 1L] <- 1
        pi[, 1L] <- 1
        return(list(initial = pi, transition = A))
    }

    persistent <- if (scenario %in% c("persistent", "clear", "trait_conditioned")) 2.0 else if (scenario == "rapid_switch") -0.6 else 1.0
    use_trait <- scenario %in% c("trait_conditioned", "clear", "persistent", "weak")
    for (j in seq_len(J)) {
        theta <- person_traits[j, "theta"]
        tau <- person_traits[j, "tau"]
        init_eta <- c(seq(-0.35, 0.35, length.out = K - 1L), 0)
        if (use_trait) {
            init_eta[seq_len(K - 1L)] <- init_eta[seq_len(K - 1L)] + trait_strength * theta - 0.25 * tau
        }
        pi[j, ] <- .ep11_m4_softmax(init_eta)

        for (h in seq_len(K)) {
            eta <- rep(-0.4, K)
            eta[h] <- persistent
            if (use_trait) {
                gradient <- seq(-0.5, 0.5, length.out = K)
                eta <- eta + trait_strength * theta * gradient - 0.2 * tau * rev(gradient)
            }
            A[j, h, ] <- .ep11_m4_softmax(eta)
        }
    }
    list(initial = pi, transition = A)
}

.ep11_m4_simulate_states <- function(person_index, sequence_id, initial, transition, K) {
    N <- length(person_index)
    state <- integer(N)
    blocks <- rle(sequence_id)
    starts <- cumsum(c(1L, head(blocks$lengths, -1L)))
    for (s in seq_along(blocks$lengths)) {
        idx <- starts[[s]]:(starts[[s]] + blocks$lengths[[s]] - 1L)
        j <- person_index[idx[[1L]]]
        if (K == 1L) {
            state[idx] <- 1L
        } else {
            state[idx[[1L]]] <- sample.int(K, 1L, prob = initial[j, ])
            if (length(idx) > 1L) {
                for (tt in 2:length(idx)) {
                    prev <- state[idx[[tt - 1L]]]
                    state[idx[[tt]]] <- sample.int(K, 1L, prob = transition[j, prev, ])
                }
            }
        }
    }
    state
}

.ep11_m4_apply_missingness <- function(d, mechanism, rate, true_state, K) {
    N <- nrow(d)
    rate <- max(0, min(0.8, as.numeric(rate)))
    if (mechanism == "none" || rate <= 0) return(d)

    base <- rep(rate, N)
    p_rt <- p_gaze <- p_pupil <- base
    if (mechanism == "quality") {
        p_pupil <- stats::plogis(stats::qlogis(pmax(rate, 0.01)) + 1.2 * (-d$pupil_quality))
    } else if (mechanism == "gaze") {
        z <- as.numeric(scale(log1p(d$gaze)))
        p_gaze <- stats::plogis(stats::qlogis(pmax(rate, 0.01)) + 0.8 * z)
        p_pupil <- stats::plogis(stats::qlogis(pmax(rate, 0.01)) + 0.5 * z)
    } else if (mechanism == "pupil_quality") {
        p_pupil <- stats::plogis(stats::qlogis(pmax(rate, 0.01)) + 1.4 * (d$pupil_quality < -0.5))
    } else if (mechanism == "device") {
        p_pupil <- pmin(0.9, base + ifelse(d$device == "device_B", 0.18, 0))
        p_gaze <- pmin(0.9, base + ifelse(d$device == "device_B", 0.08, 0))
    } else if (mechanism == "state_dependent") {
        if (K <= 1L) stop("State-dependent missingness requires more than one true state.", call. = FALSE)
        high <- true_state == K
        p_rt <- pmin(0.9, base + 0.10 * high)
        p_gaze <- pmin(0.9, base + 0.15 * high)
        p_pupil <- pmin(0.9, base + 0.25 * high)
    } else if (mechanism != "mcar") {
        stop("Unknown M4 missingness mechanism: ", mechanism, call. = FALSE)
    }

    d$rt[stats::runif(N) < p_rt] <- NA_real_
    d$gaze[stats::runif(N) < p_gaze] <- NA_integer_
    d$pupil[stats::runif(N) < p_pupil] <- NA_real_
    d
}

#' Simulate M4 multimodal sequential measurement data
#'
#' Generates deterministic synthetic response, RT, gaze, pupil, nuisance, and
#' ordered latent-state data for software validation and methodological stress
#' testing. Synthetic state labels are known truth for recovery only and are not
#' psychological constructs.
#'
#' @param n_person,n_item Number of persons and total item trials per person.
#' @param n_session Number of non-overlapping ordered sessions per person.
#' @param n_states True latent-state count. May be 1 through 4.
#' @param scenario State/data-generating scenario: `clear`, `weak`, `null`,
#'   `persistent`, `rapid_switch`, `trait_conditioned`, `rt_redundant`,
#'   `gaze_redundant`, `pupil_redundant`, `nuisance_confounded`, or
#'   `device_confounded`.
#' @param missingness Missingness stress mechanism.
#' @param missing_rate Base channel-missingness probability.
#' @param seed Deterministic random seed.
#' @return An `eye_multimodal_m4_simulation` containing `data`, full generating
#'   `truth`, scenario metadata, and interpretation boundary.
#' @export
simulate_multimodal_m4 <- function(
    n_person = 80L,
    n_item = 12L,
    n_session = 1L,
    n_states = 2L,
    scenario = c(
        "clear", "weak", "null", "persistent", "rapid_switch",
        "trait_conditioned", "rt_redundant", "gaze_redundant",
        "pupil_redundant", "nuisance_confounded", "device_confounded"
    ),
    missingness = c("none", "mcar", "quality", "gaze", "pupil_quality", "device", "state_dependent"),
    missing_rate = 0.08,
    seed = 20260820L
) {
    scenario <- match.arg(scenario)
    missingness <- match.arg(missingness)
    n_person <- as.integer(n_person)
    n_item <- as.integer(n_item)
    n_session <- as.integer(n_session)
    n_states <- as.integer(n_states)
    if (n_person < 2L || n_item < 3L || n_session < 1L || n_session > n_item) {
        stop("Use at least 2 persons, 3 items, and 1..n_item sessions.", call. = FALSE)
    }
    if (n_states < 1L || n_states > 4L) stop("`n_states` must be 1 through 4.", call. = FALSE)
    if (scenario %in% c("null", "nuisance_confounded", "device_confounded")) n_states <- 1L
    if (missingness == "state_dependent" && n_states == 1L) {
        stop("`state_dependent` missingness requires a data-generating state process with K > 1.", call. = FALSE)
    }

    set.seed(as.integer(seed))
    person_corr <- matrix(c(
        1, -.25, .20, .15,
        -.25, 1, -.10, -.10,
        .20, -.10, 1, .30,
        .15, -.10, .30, 1
    ), 4L, 4L, byrow = TRUE)
    item_corr <- matrix(c(
        1, .20, .10, .05,
        .20, 1, .20, .10,
        .10, .20, 1, .25,
        .05, .10, .25, 1
    ), 4L, 4L, byrow = TRUE)
    persons <- .ep11_m4_corr_draw(
        n_person, c(0, 0, 0, 0), c(0.85, 0.65, 0.55, 0.50), person_corr,
        names = c("theta", "tau", "omega", "rho")
    )
    items <- .ep11_m4_corr_draw(
        n_item, c(0, 4.2, 2.2, 0), c(0.75, 0.45, 0.45, 0.35), item_corr,
        names = c("b", "beta", "m", "kappa")
    )

    person_id <- rep(sprintf("P%03d", seq_len(n_person)), each = n_item)
    item_index <- rep(seq_len(n_item), times = n_person)
    person_index <- rep(seq_len(n_person), each = n_item)
    session_index_item <- pmin(n_session, floor((seq_len(n_item) - 1L) * n_session / n_item) + 1L)
    session_index <- rep(session_index_item, times = n_person)
    sequence_id <- paste(person_id, sprintf("S%02d", session_index), sep = "_")
    trial_index <- ave(item_index, sequence_id, FUN = seq_along)
    N <- length(person_index)

    dyn_scenario <- if (scenario %in% c("rt_redundant", "gaze_redundant", "pupil_redundant")) "clear" else scenario
    dyn <- .ep11_m4_transition_array(persons, n_states, dyn_scenario)
    true_state <- .ep11_m4_simulate_states(person_index, sequence_id, dyn$initial, dyn$transition, n_states)

    separation <- if (scenario == "weak") 0.18 else 0.55
    if (n_states == 1L) {
        delta_rt <- delta_gaze <- delta_pupil <- 0
    } else {
        anchor <- seq(-1, 1, length.out = n_states)
        delta_rt <- separation * anchor
        delta_gaze <- 0.55 * separation * anchor
        delta_pupil <- 0.75 * separation * anchor
        if (scenario == "rt_redundant") {
            delta_gaze <- rep(0, n_states); delta_pupil <- rep(0, n_states)
        } else if (scenario == "gaze_redundant") {
            delta_rt <- 0.18 * anchor; delta_pupil <- rep(0, n_states); delta_gaze <- 0.8 * separation * anchor
        } else if (scenario == "pupil_redundant") {
            delta_rt <- 0.18 * anchor; delta_gaze <- rep(0, n_states); delta_pupil <- separation * anchor
        }
    }

    baseline <- stats::rnorm(N, 0, 1)
    luminance <- stats::rnorm(N, 0, 1)
    gaze_x <- stats::rnorm(N, 0, 1)
    gaze_y <- stats::rnorm(N, 0, 1)
    quality <- pmax(-2, pmin(2, stats::rnorm(N, 0.4, 0.7)))
    blink <- stats::rbinom(N, 1, 0.05)
    interpolated <- stats::rbinom(N, 1, 0.08)
    time_on_task <- ave(seq_len(N), person_id, FUN = function(z) seq(0, 1, length.out = length(z)))
    device_person <- sample(c("device_A", "device_B"), n_person, replace = TRUE)
    device <- device_person[person_index]

    gamma <- c(
        baseline = 0.10, luminance = -0.28, gaze_x = 0.05, gaze_y = -0.05,
        quality = 0.12, blink = -0.20, interpolated = -0.08, time_on_task = 0.10
    )
    gamma_generating <- gamma
    nuisance <- gamma_generating[["baseline"]] * baseline + gamma_generating[["luminance"]] * luminance +
        gamma_generating[["gaze_x"]] * gaze_x + gamma_generating[["gaze_y"]] * gaze_y +
        gamma_generating[["quality"]] * quality + gamma_generating[["blink"]] * blink +
        gamma_generating[["interpolated"]] * interpolated + gamma_generating[["time_on_task"]] * time_on_task

    state_rt <- delta_rt[true_state]
    state_gaze <- delta_gaze[true_state]
    state_pupil <- delta_pupil[true_state]
    if (scenario %in% c("null", "nuisance_confounded", "device_confounded")) {
        state_rt <- state_gaze <- state_pupil <- rep(0, N)
    }

    eta_response <- persons[person_index, "theta"] - items[item_index, "b"]
    response <- stats::rbinom(N, 1, stats::plogis(eta_response))
    log_rt <- items[item_index, "beta"] - persons[person_index, "tau"] + state_rt + stats::rnorm(N, 0, 0.32)
    gaze_eta <- items[item_index, "m"] + persons[person_index, "omega"] + state_gaze
    gaze <- stats::rnbinom(N, mu = exp(gaze_eta), size = 7)
    pupil_mu <- items[item_index, "kappa"] + persons[person_index, "rho"] + nuisance + state_pupil

    device_shift <- rep(0, N)
    if (scenario == "nuisance_confounded") {
        # Block-like luminance structure can mimic states unless nuisance adjustment is respected.
        block <- ave(seq_len(N), sequence_id, FUN = function(z) rep(c(-1, 1), length.out = length(z)))
        luminance <- as.numeric(block) + stats::rnorm(N, 0, 0.15)
        gamma_generating <- c(
            baseline = 0.10, luminance = -0.65, gaze_x = 0, gaze_y = 0,
            quality = 0.12, blink = 0, interpolated = 0, time_on_task = 0.10
        )
        nuisance <- gamma_generating[["baseline"]] * baseline + gamma_generating[["luminance"]] * luminance +
            gamma_generating[["quality"]] * quality + gamma_generating[["time_on_task"]] * time_on_task
        pupil_mu <- items[item_index, "kappa"] + persons[person_index, "rho"] + nuisance
    }
    if (scenario == "device_confounded") {
        device_shift <- ifelse(device == "device_B", 0.75, -0.25)
        shift <- device_shift
        pupil_mu <- pupil_mu + shift
        gaze_eta <- gaze_eta + ifelse(device == "device_B", 0.18, -0.05)
        gaze <- stats::rnbinom(N, mu = exp(gaze_eta), size = 7)
    }
    pupil <- pupil_mu + stats::rnorm(N, 0, 0.42)

    d <- data.frame(
        person_id = person_id,
        item_id = sprintf("I%03d", item_index),
        sequence_id = sequence_id,
        trial_index = as.numeric(trial_index),
        session = sprintf("S%02d", session_index),
        response = response,
        rt = exp(log_rt),
        gaze = as.integer(gaze),
        pupil = pupil,
        pupil_baseline = baseline,
        luminance = luminance,
        gaze_x = gaze_x,
        gaze_y = gaze_y,
        pupil_quality = quality,
        pupil_blink = blink,
        pupil_interpolated = interpolated,
        time_on_task = time_on_task,
        device = device,
        sampling_rate_hz = 60,
        stringsAsFactors = FALSE
    )
    d <- .ep11_m4_apply_missingness(d, missingness, missing_rate, true_state, n_states)

    truth <- list(
        n_states = n_states,
        state = true_state,
        initial_prob_person = dyn$initial,
        transition_prob_person = dyn$transition,
        delta_rt = as.numeric(delta_rt),
        delta_gaze = as.numeric(delta_gaze),
        delta_pupil = as.numeric(delta_pupil),
        theta = persons[, "theta"], tau = persons[, "tau"], omega = persons[, "omega"], rho = persons[, "rho"],
        b = items[, "b"], beta = items[, "beta"], m = items[, "m"], kappa = items[, "kappa"],
        gamma_pupil = gamma_generating,
        device_shift = device_shift,
        person_levels = sprintf("P%03d", seq_len(n_person)),
        item_levels = sprintf("I%03d", seq_len(n_item))
    )

    out <- list(
        data = d,
        truth = truth,
        scenario = scenario,
        missingness = missingness,
        missing_rate = missing_rate,
        seed = as.integer(seed),
        interpretation = .ep11_m4_reference$interpretation,
        call = match.call()
    )
    class(out) <- c("eye_multimodal_m4_simulation", "list")
    out
}

print.eye_multimodal_m4_simulation <- function(x, ...) {
    cat(
        "<eye_multimodal_m4_simulation>\n",
        "  rows: ", nrow(x$data), "\n",
        "  persons: ", length(unique(x$data$person_id)), "\n",
        "  items: ", length(unique(x$data$item_id)), "\n",
        "  sequences: ", length(unique(x$data$sequence_id)), "\n",
        "  true states: ", x$truth$n_states, "\n",
        "  scenario: ", x$scenario, "\n",
        "  missingness: ", x$missingness, "\n",
        "  boundary: synthetic state labels are recovery truth, not psychological constructs\n",
        sep = ""
    )
    invisible(x)
}

.ep11_m4_permutations <- function(x) {
    x <- as.integer(x)
    if (length(x) <= 1L) return(list(x))
    out <- list()
    z <- 1L
    for (i in seq_along(x)) {
        rest <- x[-i]
        for (p in .ep11_m4_permutations(rest)) {
            out[[z]] <- c(x[[i]], p)
            z <- z + 1L
        }
    }
    out
}

.ep11_m4_state_effect_means <- function(fit) {
    list(
        rt = as.numeric(.ep11_m4_draw_mean(fit, "delta_rt")),
        gaze = as.numeric(.ep11_m4_draw_mean(fit, "delta_gaze")),
        pupil = as.numeric(.ep11_m4_draw_mean(fit, "delta_pupil"))
    )
}

.ep11_m4_alignment <- function(sim, fit) {
    Kt <- sim$truth$n_states
    Kf <- fit$spec$n_states
    if (Kt != Kf) {
        return(list(permutation = seq_len(Kf), comparable = FALSE, loss = NA_real_))
    }
    if (Kt == 1L) return(list(permutation = 1L, comparable = TRUE, loss = 0))
    est <- .ep11_m4_state_effect_means(fit)
    truth <- rbind(sim$truth$delta_rt, sim$truth$delta_gaze, sim$truth$delta_pupil)
    estm <- rbind(est$rt, est$gaze, est$pupil)
    prob <- .ep11_m4_state_prob_matrix(fit)
    occ_est <- colMeans(prob)
    occ_truth <- tabulate(sim$truth$state, nbins = Kt) / length(sim$truth$state)
    A_est_person <- .ep11_m4_indexed_mean(fit, "transition_prob_person", c(length(fit$data$person_levels), Kt, Kt))
    A_est <- apply(A_est_person, c(2L, 3L), mean)
    A_truth <- apply(sim$truth$transition_prob_person, c(2L, 3L), mean)
    perms <- .ep11_m4_permutations(seq_len(Kt))
    loss <- vapply(perms, function(p) {
        effect_loss <- sum((estm[, p, drop = FALSE] - truth)^2)
        occupancy_loss <- sum((occ_est[p] - occ_truth)^2)
        transition_loss <- sum((A_est[p, p, drop = FALSE] - A_truth)^2)
        effect_loss + 0.5 * occupancy_loss + 0.5 * transition_loss
    }, numeric(1L))
    best <- which.min(loss)
    list(permutation = perms[[best]], comparable = TRUE, loss = loss[[best]], components = c(state_effect = 1, occupancy = 0.5, transition = 0.5))
}

.ep11_m4_vector_draw_summary <- function(fit, variable) {
    d <- posterior::as_draws_matrix(fit$fit$draws(variables = variable))
    if (!ncol(d)) return(data.frame())
    data.frame(
        variable = colnames(d),
        estimate = colMeans(d),
        q025 = apply(d, 2L, stats::quantile, probs = 0.025, names = FALSE),
        q975 = apply(d, 2L, stats::quantile, probs = 0.975, names = FALSE),
        stringsAsFactors = FALSE
    )
}

.ep11_m4_recovery_variable <- function(fit, variable, truth) {
    tab <- .ep11_m4_vector_draw_summary(fit, variable)
    if (!nrow(tab)) return(data.frame())
    if (length(truth) != nrow(tab)) return(data.frame())
    tab$truth <- as.numeric(truth)
    tab$bias <- tab$estimate - tab$truth
    tab$sq_error <- tab$bias^2
    tab$covered <- tab$q025 <= tab$truth & tab$q975 >= tab$truth
    tab$family <- variable
    tab
}

.ep11_m4_brier <- function(prob, state) {
    K <- ncol(prob)
    truth <- matrix(0, nrow(prob), K)
    truth[cbind(seq_len(nrow(prob)), state)] <- 1
    mean(rowSums((prob - truth)^2))
}

.ep11_m4_balanced_accuracy <- function(pred, truth, K) {
    recall <- vapply(seq_len(K), function(k) {
        idx <- truth == k
        if (!any(idx)) return(NA_real_)
        mean(pred[idx] == k)
    }, numeric(1L))
    mean(recall, na.rm = TRUE)
}

.ep11_m4_recovery_one <- function(sim, fit) {
    if (!inherits(sim, "eye_multimodal_m4_simulation") || !inherits(fit, "eye_multimodal_m4_fit")) {
        stop("Recovery evaluation requires an M4 simulation and M4 fit.", call. = FALSE)
    }
    align <- .ep11_m4_alignment(sim, fit)
    p <- .ep11_m4_state_prob_matrix(fit)
    if (isTRUE(align$comparable)) {
        p <- p[, align$permutation, drop = FALSE]
    }
    map <- max.col(p, ties.method = "first")
    K <- sim$truth$n_states

    parameter <- do.call(rbind, Filter(Negate(is.null), list(
        .ep11_m4_recovery_variable(fit, "theta", sim$truth$theta),
        .ep11_m4_recovery_variable(fit, "tau", sim$truth$tau),
        .ep11_m4_recovery_variable(fit, "omega", sim$truth$omega),
        .ep11_m4_recovery_variable(fit, "rho", sim$truth$rho),
        .ep11_m4_recovery_variable(fit, "b", sim$truth$b),
        .ep11_m4_recovery_variable(fit, "beta", sim$truth$beta),
        .ep11_m4_recovery_variable(fit, "m", sim$truth$m),
        .ep11_m4_recovery_variable(fit, "kappa", sim$truth$kappa)
    )))

    eff <- .ep11_m4_state_effect_means(fit)
    if (isTRUE(align$comparable)) {
        eff <- lapply(eff, function(z) z[align$permutation])
    }
    state_effect <- data.frame(
        family = rep(c("delta_rt", "delta_gaze", "delta_pupil"), each = K),
        state = rep(seq_len(K), 3L),
        estimate = c(eff$rt, eff$gaze, eff$pupil),
        truth = c(sim$truth$delta_rt, sim$truth$delta_gaze, sim$truth$delta_pupil),
        stringsAsFactors = FALSE
    )
    state_effect$bias <- state_effect$estimate - state_effect$truth

    occ_truth <- tabulate(sim$truth$state, nbins = K) / length(sim$truth$state)
    occ_est <- colMeans(p)
    transition_rmse <- NA_real_
    if (isTRUE(align$comparable)) {
        Aest <- .ep11_m4_indexed_mean(fit, "transition_prob_person", c(length(fit$data$person_levels), K, K))
        if (length(Aest) && all(is.finite(Aest))) {
            Aest <- Aest[, align$permutation, align$permutation, drop = FALSE]
            transition_rmse <- sqrt(mean((Aest - sim$truth$transition_prob_person)^2))
        }
    }

    list(
        alignment = align,
        parameter = parameter,
        state_effect = state_effect,
        state_metrics = data.frame(
            metric = c("brier", "MAP_accuracy", "balanced_accuracy", "occupancy_RMSE", "transition_RMSE"),
            value = c(
                if (isTRUE(align$comparable)) .ep11_m4_brier(p, sim$truth$state) else NA_real_,
                if (isTRUE(align$comparable)) mean(map == sim$truth$state) else NA_real_,
                if (isTRUE(align$comparable)) .ep11_m4_balanced_accuracy(map, sim$truth$state, K) else NA_real_,
                if (isTRUE(align$comparable)) sqrt(mean((occ_est - occ_truth)^2)) else NA_real_,
                transition_rmse
            ),
            stringsAsFactors = FALSE
        ),
        occupancy = data.frame(state = seq_len(K), truth = occ_truth, estimate = occ_est)
    )
}

.ep11_m4_recovery_summary <- function(raw) {
    if (!length(raw)) return(data.frame())
    rows <- lapply(names(raw), function(nm) {
        z <- raw[[nm]]
        p <- z$parameter
        data.frame(
            scenario = nm,
            parameter_rmse = if (nrow(p)) sqrt(mean(p$sq_error, na.rm = TRUE)) else NA_real_,
            parameter_coverage = if (nrow(p)) mean(p$covered, na.rm = TRUE) else NA_real_,
            state_effect_rmse = sqrt(mean(z$state_effect$bias^2, na.rm = TRUE)),
            brier = z$state_metrics$value[z$state_metrics$metric == "brier"],
            balanced_accuracy = z$state_metrics$value[z$state_metrics$metric == "balanced_accuracy"],
            occupancy_rmse = z$state_metrics$value[z$state_metrics$metric == "occupancy_RMSE"],
            transition_rmse = z$state_metrics$value[z$state_metrics$metric == "transition_RMSE"],
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

#' Evaluate deterministic M4 parameter and state recovery
#'
#' Recovery aligns latent-state labels before scoring multichannel state effects,
#' and emphasizes posterior-probability calibration, occupancy, and transition
#' recovery rather than treating MAP classification accuracy as the primary
#' criterion. By default the function returns a small five-scenario design and
#' does not launch expensive fitting.
#'
#' @param simulation Optional single M4 simulation.
#' @param fit Optional already-fitted M4 model corresponding to `simulation`.
#' @param scenarios Deterministic development battery used when `run = TRUE`.
#' @param run Whether to execute the small recovery battery. Default `FALSE`.
#' @param simulation_args Named arguments forwarded to simulation.
#' @param fit_args Named arguments forwarded to `fit_multimodal_m4()`.
#' @return An `eye_multimodal_m4_recovery`.
#' @export
multimodal_m4_recovery <- function(
    simulation = NULL,
    fit = NULL,
    scenarios = c("clear", "weak", "null", "trait_conditioned", "nuisance_confounded"),
    run = FALSE,
    simulation_args = list(n_person = 60L, n_item = 10L),
    fit_args = list()
) {
    if (!is.null(simulation) || !is.null(fit)) {
        if (is.null(simulation) || is.null(fit)) {
            stop("Supply both `simulation` and `fit` for single-fit recovery evaluation.", call. = FALSE)
        }
        raw <- list(single = .ep11_m4_recovery_one(simulation, fit))
        out <- list(design = "single", raw = raw, summary = .ep11_m4_recovery_summary(raw), executed = TRUE)
    } else if (!isTRUE(run)) {
        design <- data.frame(
            scenario = scenarios,
            true_K = ifelse(scenarios %in% c("null", "nuisance_confounded", "device_confounded"), 1L, 2L),
            purpose = c(
                clear = "basic state/transition recovery",
                weak = "uncertainty rather than false certainty",
                null = "formal K=1 behavior",
                trait_conditioned = "transition-conditioning recovery",
                nuisance_confounded = "confounding should be exposed"
            )[scenarios],
            stringsAsFactors = FALSE
        )
        out <- list(design = design, raw = NULL, summary = NULL, executed = FALSE)
    } else {
        raw <- list()
        for (i in seq_along(scenarios)) {
            sc <- scenarios[[i]]
            K <- if (sc %in% c("null", "nuisance_confounded", "device_confounded")) 1L else 2L
            sargs <- utils::modifyList(
                list(n_states = K, scenario = sc, seed = 20260820L + i),
                simulation_args
            )
            sim <- do.call(simulate_multimodal_m4, sargs)
            spec <- multimodal_m4_spec(n_states = K)
            fargs <- utils::modifyList(list(x = sim, spec = spec), fit_args)
            fitted <- do.call(fit_multimodal_m4, fargs)
            raw[[sc]] <- .ep11_m4_recovery_one(sim, fitted)
        }
        out <- list(design = scenarios, raw = raw, summary = .ep11_m4_recovery_summary(raw), executed = TRUE)
    }
    out$interpretation <- .ep11_m4_reference$interpretation
    class(out) <- c("eye_multimodal_m4_recovery", "list")
    out
}

print.eye_multimodal_m4_recovery <- function(x, ...) {
    cat("<eye_multimodal_m4_recovery>\n")
    if (!isTRUE(x$executed)) {
        cat("  status: DESIGN ONLY - no backend fits executed\n", "  scenarios: ", nrow(x$design), "\n", sep = "")
        print(x$design, row.names = FALSE)
    } else {
        cat("  status: EXECUTED\n")
        print(x$summary, row.names = FALSE)
    }
    cat("  boundary: state recovery validates model behavior under synthetic truth, not substantive construct validity\n")
    invisible(x)
}
