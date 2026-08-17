# eyeprocess 0.10 - M3 simulation, missingness stress, and recovery

.ep10_m3_named_vector <- function(x, required, name) {
    if (is.null(names(x)) || !all(required %in% names(x))) {
        stop("`", name, "` must be named with: ", paste(required, collapse = ", "), call. = FALSE)
    }
    x <- as.numeric(x[required]); names(x) <- required
    if (any(!is.finite(x))) stop("`", name, "` must be finite.", call. = FALSE)
    x
}

.ep10_m3_dropout_probability <- function(d, mechanism, base_rate, pupil_signal) {
    mechanism <- match.arg(mechanism, c("mcar", "quality", "gaze", "ability", "device", "none"))
    if (identical(mechanism, "none") || base_rate <= 0) return(rep(0, nrow(d)))
    target <- stats::qlogis(min(max(base_rate, .001), .95))
    z <- switch(
        mechanism,
        mcar = rep(0, nrow(d)),
        quality = -as.numeric(scale(d$pupil_quality)),
        gaze = as.numeric(scale(d$gaze_mean_latent)),
        ability = as.numeric(scale(d$theta_truth)),
        device = ifelse(d$device == "device_B", 1, -1)
    )
    z[!is.finite(z)] <- 0
    stats::plogis(target + .8 * z)
}

#' Simulate the M3 response + RT + gaze + pupil generative model
#'
#' Generates a four-dimensional correlated person process and four-dimensional
#' correlated item process, explicit pupil nuisance variables, complete-data
#' truth, device/session metadata, blink/interpolation indicators and pupil
#' dropout. Pupil scenarios include informative, weak, null, redundant and
#' confound-only conditions so that validation includes cases where pupil
#' should add no defensible psychometric information.
#'
#' @param n_person,n_item Design size.
#' @param pupil_signal Pupil signal scenario.
#' @param pupil_missingness Missingness stress mechanism.
#' @param dropout Named base dropout probabilities for response, RT, gaze, pupil.
#' @param pupil_noise Residual SD for the pupil channel.
#' @param confound_strength Named standardized nuisance coefficients.
#' @param device_effect,session_effect Additive pupil measurement shifts.
#' @param seed Reproducibility seed.
#' @param mu_item Named numeric vector of population means for item
#'   difficulty, response-time intensity, gaze intensity, and pupil
#'   intensity.
#' @param sd_person Named positive numeric vector of population standard
#'   deviations for person ability, speed, gaze-process propensity, and
#'   pupil responsivity.
#' @param cor_person A 4 x 4 correlation matrix for person ability, speed,
#'   gaze-process, and pupil-responsivity effects, in that order.
#' @param sd_item Named positive numeric vector of population standard
#'   deviations for item difficulty, response-time intensity, gaze
#'   intensity, and pupil intensity.
#' @param cor_item A 4 x 4 correlation matrix for item difficulty,
#'   response-time intensity, gaze intensity, and pupil intensity, in
#'   that order.
#' @param nu_range Length-two positive increasing numeric vector giving
#'   the lower and upper bounds for the item-specific response-time
#'   inverse-scale parameter `nu`; the log-response-time residual
#'   standard deviation is `1 / nu`.
#' @param gaze_shape Named positive numeric vector with elements `shape`
#'   and `scale` defining the inverse-gamma generator for item-specific
#'   negative-binomial gaze dispersion.
#' @return An `eye_multimodal_m3_simulation` retaining complete truth.
#' @export
simulate_multimodal_m3 <- function(
    n_person = 120L,
    n_item = 12L,
    pupil_signal = c("informative", "weak", "null", "redundant", "confounded"),
    pupil_missingness = c("mcar", "quality", "gaze", "ability", "device", "none"),
    mu_item = c(difficulty = 0, time_intensity = 4, gaze_intensity = 3.5, pupil_intensity = 0),
    sd_person = c(ability = 1, speed = .5, gaze_process = .5, pupil_responsivity = .55),
    cor_person = matrix(c(
        1, .30, -.30, .20,
        .30, 1, -.25, -.15,
        -.30, -.25, 1, .25,
        .20, -.15, .25, 1
    ), 4L, 4L, byrow = TRUE),
    sd_item = c(difficulty = .75, time_intensity = .35, gaze_intensity = .60, pupil_intensity = .40),
    cor_item = matrix(c(
        1, .25, .20, .10,
        .25, 1, .30, .15,
        .20, .30, 1, .20,
        .10, .15, .20, 1
    ), 4L, 4L, byrow = TRUE),
    nu_range = c(.5, .8),
    gaze_shape = c(shape = 2, scale = 6),
    pupil_noise = .65,
    confound_strength = c(
        baseline = .25, luminance = -.35, gaze_x = .12, gaze_y = -.10,
        quality = .20, blink = -.18, interpolated = -.12, time_on_task = .15
    ),
    dropout = c(response = 0, rt = 0, gaze = .05, pupil = .12),
    device_effect = 0,
    session_effect = 0,
    seed = 20260815L
) {
    pupil_signal <- match.arg(pupil_signal)
    pupil_missingness <- match.arg(pupil_missingness)
    n_person <- as.integer(n_person); n_item <- as.integer(n_item)
    if (n_person < 2L || n_item < 2L) stop("`n_person` and `n_item` must be at least 2.", call. = FALSE)

    pnames <- c("ability", "speed", "gaze_process", "pupil_responsivity")
    inames <- c("difficulty", "time_intensity", "gaze_intensity", "pupil_intensity")
    mu_item <- .ep10_m3_named_vector(mu_item, inames, "mu_item")
    sd_person <- .ep10_m3_named_vector(sd_person, pnames, "sd_person")
    sd_item <- .ep10_m3_named_vector(sd_item, inames, "sd_item")
    if (any(sd_person <= 0) || any(sd_item <= 0)) stop("Standard deviations must be positive.", call. = FALSE)
    cor_person <- .ep10_m2_safe_cor_matrix(cor_person, "cor_person")
    cor_item <- .ep10_m2_safe_cor_matrix(cor_item, "cor_item")
    if (!identical(dim(cor_person), c(4L, 4L)) || !identical(dim(cor_item), c(4L, 4L))) stop("M3 correlation matrices must be 4 x 4.", call. = FALSE)
    if (length(nu_range) != 2L || any(!is.finite(nu_range)) || nu_range[1] <= 0 || nu_range[2] <= nu_range[1]) stop("Invalid `nu_range`.", call. = FALSE)
    if (is.null(names(gaze_shape)) || !all(c("shape", "scale") %in% names(gaze_shape)) || any(gaze_shape <= 0)) stop("Invalid `gaze_shape`.", call. = FALSE)
    if (!is.finite(pupil_noise) || pupil_noise <= 0) stop("`pupil_noise` must be positive.", call. = FALSE)
    confound_strength <- .ep10_m3_named_vector(confound_strength, .ep10_m3_nuisance_names, "confound_strength")
    dropout <- .ep10_m3_named_vector(dropout, c("response", "rt", "gaze", "pupil"), "dropout")
    if (any(dropout < 0 | dropout >= 1)) stop("Dropout probabilities must be in [0,1).", call. = FALSE)

    set.seed(as.integer(seed))
    person_effects <- .ep10_m2_rmvnorm(n_person, sd_person, cor_person)
    colnames(person_effects) <- c("theta", "tau", "omega", "rho")
    item_centered <- .ep10_m2_rmvnorm(n_item, sd_item, cor_item)
    item_effects <- sweep(item_centered, 2L, mu_item, "+")
    colnames(item_effects) <- c("b", "beta", "m", "kappa")

    effective_sd_person <- sd_person
    effective_sd_item <- sd_item
    effective_cor_person <- cor_person
    effective_cor_item <- cor_item

    if (identical(pupil_signal, "weak")) {
        person_effects[, "rho"] <- person_effects[, "rho"] * .20
        item_effects[, "kappa"] <- mu_item[["pupil_intensity"]] +
            (item_effects[, "kappa"] - mu_item[["pupil_intensity"]]) * .20
        effective_sd_person[["pupil_responsivity"]] <- effective_sd_person[["pupil_responsivity"]] * .20
        effective_sd_item[["pupil_intensity"]] <- effective_sd_item[["pupil_intensity"]] * .20
    }
    if (identical(pupil_signal, "null")) {
        person_effects[, "rho"] <- stats::rnorm(n_person, 0, sd_person[["pupil_responsivity"]])
        item_effects[, "kappa"] <- mu_item[["pupil_intensity"]] +
            stats::rnorm(n_item, 0, sd_item[["pupil_intensity"]])
        effective_cor_person[4, 1:3] <- 0
        effective_cor_person[1:3, 4] <- 0
        effective_cor_item[4, 1:3] <- 0
        effective_cor_item[1:3, 4] <- 0
    }
    if (identical(pupil_signal, "redundant")) {
        r <- .85
        person_effects[, "rho"] <- (
            r * as.numeric(scale(person_effects[, "omega"])) +
            sqrt(1 - r^2) * stats::rnorm(n_person)
        ) * sd_person[["pupil_responsivity"]]
        item_effects[, "kappa"] <- mu_item[["pupil_intensity"]] + (
            r * as.numeric(scale(item_effects[, "m"])) +
            sqrt(1 - r^2) * stats::rnorm(n_item)
        ) * sd_item[["pupil_intensity"]]
        effective_cor_person[4, 1:3] <- r * cor_person[3, 1:3]
        effective_cor_person[1:3, 4] <- effective_cor_person[4, 1:3]
        effective_cor_person[4, 4] <- 1
        effective_cor_item[4, 1:3] <- r * cor_item[3, 1:3]
        effective_cor_item[1:3, 4] <- effective_cor_item[4, 1:3]
        effective_cor_item[4, 4] <- 1
    }
    if (identical(pupil_signal, "confounded")) {
        person_effects[, "rho"] <- 0
        item_effects[, "kappa"] <- 0
        effective_sd_person[["pupil_responsivity"]] <- 0
        effective_sd_item[["pupil_intensity"]] <- 0
        effective_cor_person[4, ] <- NA_real_
        effective_cor_person[, 4] <- NA_real_
        effective_cor_item[4, ] <- NA_real_
        effective_cor_item[, 4] <- NA_real_
    }

    nu <- stats::runif(n_item, nu_range[1], nu_range[2])
    s <- .ep10_m2_rinv_gamma(n_item, gaze_shape[["shape"]], gaze_shape[["scale"]])
    grid <- expand.grid(person_index = seq_len(n_person), item_index = seq_len(n_item), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    J <- grid$person_index; I <- grid$item_index

    theta <- person_effects[J, "theta"]; tau <- person_effects[J, "tau"]; omega <- person_effects[J, "omega"]; rho <- person_effects[J, "rho"]
    b <- item_effects[I, "b"]; beta <- item_effects[I, "beta"]; m <- item_effects[I, "m"]; kappa <- item_effects[I, "kappa"]
    p_response <- stats::plogis(theta - b)
    log_rt_mu <- beta - tau
    gaze_mu <- exp(m + omega)

    response_complete <- stats::rbinom(nrow(grid), 1L, p_response)
    rt_complete <- exp(stats::rnorm(nrow(grid), log_rt_mu, 1 / nu[I]))
    gaze_complete <- stats::rnbinom(nrow(grid), mu = gaze_mu, size = s[I])

    person_base <- stats::rnorm(n_person, 3.4, .35)
    item_luminance <- stats::runif(n_item, -.8, .8)
    pupil_baseline <- person_base[J] + stats::rnorm(nrow(grid), 0, .12)
    luminance <- item_luminance[I] + stats::rnorm(nrow(grid), 0, .08)
    gaze_x <- pmin(pmax(stats::rnorm(nrow(grid), .5 + .05 * omega, .18), 0), 1)
    gaze_y <- pmin(pmax(stats::rnorm(nrow(grid), .5 - .04 * omega, .16), 0), 1)
    time_on_task <- ave(seq_len(nrow(grid)), J, FUN = function(z) seq_along(z)) / n_item
    device_person <- rep(c("device_A", "device_B"), length.out = n_person)
    device <- device_person[J]
    session <- ifelse(I <= ceiling(n_item / 2), "session_1", "session_2")
    sampling_rate_hz <- ifelse(device == "device_A", 60, 120)
    blink_probability <- stats::plogis(-2.7 + .5 * time_on_task + .25 * abs(gaze_x - .5))
    pupil_blink <- stats::rbinom(nrow(grid), 1L, blink_probability)
    pupil_interpolated <- stats::rbinom(nrow(grid), 1L, stats::plogis(-3 + 1.1 * pupil_blink))
    pupil_quality <- pmax(0, pmin(1, .96 - .45 * pupil_blink - .20 * pupil_interpolated + stats::rnorm(nrow(grid), 0, .04)))

    raw_nuisance <- cbind(
        baseline = pupil_baseline,
        luminance = luminance,
        gaze_x = gaze_x,
        gaze_y = gaze_y,
        quality = pupil_quality,
        blink = pupil_blink,
        interpolated = pupil_interpolated,
        time_on_task = time_on_task
    )
    nuisance_center <- colMeans(raw_nuisance)
    nuisance_scale <- apply(raw_nuisance, 2L, stats::sd)
    bad_scale <- !is.finite(nuisance_scale) | nuisance_scale <= sqrt(.Machine$double.eps)
    nuisance_scale[bad_scale] <- 1
    X <- sweep(raw_nuisance, 2L, nuisance_center, "-")
    X <- sweep(X, 2L, nuisance_scale, "/")
    X[, bad_scale] <- 0
    attr(X, "scaled:center") <- nuisance_center
    attr(X, "scaled:scale") <- nuisance_scale
    pupil_latent <- kappa + rho
    nuisance_effect <- as.numeric(X %*% confound_strength)
    device_shift <- ifelse(device == "device_B", device_effect, -device_effect)
    session_shift <- ifelse(session == "session_2", session_effect, 0)

    if (identical(pupil_signal, "confounded")) pupil_latent <- 0
    pupil_complete <- pupil_latent + nuisance_effect + device_shift + session_shift + stats::rnorm(nrow(grid), 0, pupil_noise)

    dd <- data.frame(
        theta_truth = theta, gaze_mean_latent = gaze_mu, pupil_quality = pupil_quality, device = device,
        stringsAsFactors = FALSE
    )
    pupil_prob <- .ep10_m3_dropout_probability(dd, pupil_missingness, dropout[["pupil"]], pupil_signal)

    # `none` is an explicit no-missingness condition. Likewise, a
    # zero pupil dropout rate must remain zero. Blink/interpolation
    # remain observed nuisance variables, but they increase channel
    # dropout only when a pupil-missingness stress condition is enabled.
    if (
        identical(pupil_missingness, "none") ||
        dropout[["pupil"]] <= 0
    ) {
        pupil_prob <- rep(0, nrow(grid))
    } else {
        pupil_prob <- pmin(
            .95,
            pmax(
                0,
                pupil_prob +
                    .22 * pupil_blink +
                    .10 * pupil_interpolated
            )
        )
    }

    observed <- list(
        response = stats::runif(nrow(grid)) >= dropout[["response"]],
        rt = stats::runif(nrow(grid)) >= dropout[["rt"]],
        gaze = stats::runif(nrow(grid)) >= dropout[["gaze"]],
        pupil = stats::runif(nrow(grid)) >= pupil_prob
    )
    response <- response_complete; response[!observed$response] <- NA_integer_
    rt <- rt_complete; rt[!observed$rt] <- NA_real_
    gaze <- gaze_complete; gaze[!observed$gaze] <- NA_integer_
    pupil <- pupil_complete; pupil[!observed$pupil] <- NA_real_

    person_ids <- sprintf("P%04d", seq_len(n_person)); item_ids <- sprintf("I%03d", seq_len(n_item))
    data <- data.frame(
        person_id = person_ids[J], item_id = item_ids[I], response = response, rt = rt, gaze = gaze, pupil = pupil,
        pupil_baseline = pupil_baseline, luminance = luminance, gaze_x = gaze_x, gaze_y = gaze_y,
        pupil_quality = pupil_quality, time_on_task = time_on_task,
        pupil_blink = pupil_blink, pupil_interpolated = pupil_interpolated,
        device = device, session = session, sampling_rate_hz = sampling_rate_hz,
        stringsAsFactors = FALSE
    )
    complete_data <- transform(data, response = response_complete, rt = rt_complete, gaze = gaze_complete, pupil = pupil_complete)
    complete_data$response_probability <- p_response; complete_data$log_rt_mean <- log_rt_mu; complete_data$gaze_mean <- gaze_mu
    complete_data$pupil_latent_mean <- pupil_latent; complete_data$pupil_nuisance_effect <- nuisance_effect
    complete_data$pupil_device_shift <- device_shift; complete_data$pupil_session_shift <- session_shift; complete_data$pupil_dropout_probability <- pupil_prob

    truth <- list(
        n_person = n_person, n_item = n_item, pupil_signal = pupil_signal, pupil_missingness = pupil_missingness,
        mu_item = stats::setNames(mu_item, c("b", "beta", "m", "kappa")),
        sd_person = stats::setNames(effective_sd_person, c("theta", "tau", "omega", "rho")),
        cor_person = effective_cor_person,
        sd_item = stats::setNames(effective_sd_item, c("b", "beta", "m", "kappa")),
        cor_item = effective_cor_item,
        theta = person_effects[, "theta"], tau = person_effects[, "tau"], omega = person_effects[, "omega"], rho = person_effects[, "rho"],
        b = item_effects[, "b"], beta = item_effects[, "beta"], m = item_effects[, "m"], kappa = item_effects[, "kappa"],
        nu = nu, s = s, pupil = list(
            sigma = pupil_noise, gamma = confound_strength,
            nuisance_center = attr(X, "scaled:center"), nuisance_scale = attr(X, "scaled:scale"),
            device_effect = device_effect, session_effect = session_effect
        ),
        dropout = dropout, seed = as.integer(seed),
        generating_model = "M3 Rasch + lognormal RT + NB gaze + Gaussian pupil with nuisance/device/session measurement terms"
    )
    out <- list(
        data = data, complete_data = complete_data, truth = truth, reference = .ep10_m3_reference,
        interpretation = paste(
            "Synthetic truth is retained before channel corruption and dropout.",
            "The confounded/null scenarios are deliberate negative scientific cases; recovery under the generator is not empirical validity."
        )
    )
    class(out) <- c("eye_multimodal_m3_simulation", "list")
    out
}

print.eye_multimodal_m3_simulation <- function(x, ...) {
    cat("<eye_multimodal_m3_simulation>\n",
        "  persons/items/rows: ", x$truth$n_person, "/", x$truth$n_item, "/", nrow(x$data), "\n",
        "  pupil scenario: ", x$truth$pupil_signal, "\n",
        "  pupil missingness: ", x$truth$pupil_missingness, "\n",
        "  observed pupil fraction: ", sprintf("%.3f", mean(!is.na(x$data$pupil))), "\n",
        "  seed: ", x$truth$seed, "\n", sep = "")
    invisible(x)
}

.ep10_m3_truth_map <- function(sim, fit = NULL) {
    tr <- sim$truth
    gamma <- unname(tr$pupil$gamma)
    if (!is.null(fit)) {
        fit_scale <- fit$data$nuisance$scale
        names(fit_scale) <- fit$data$nuisance$covariate
        sim_scale <- tr$pupil$nuisance_scale[.ep10_m3_nuisance_names]
        gamma <- gamma * unname(fit_scale[.ep10_m3_nuisance_names] / sim_scale)
        gamma[!is.finite(gamma)] <- 0
    }
    list(
        theta = tr$theta, tau = tr$tau, omega = tr$omega, rho = tr$rho,
        b = tr$b, beta = tr$beta, m = tr$m, kappa = tr$kappa,
        nu = tr$nu, s = tr$s,
        sigma_person = unname(tr$sd_person), sigma_item = unname(tr$sd_item),
        corr_person = as.numeric(tr$cor_person), corr_item = as.numeric(tr$cor_item),
        sigma_pupil = tr$pupil$sigma, gamma_pupil = gamma
    )
}

.ep10_m3_draw_summary_vector <- function(fit, variable) {
    s <- fit$fit$summary(
        variables = variable
    )

    draws <- fit$fit$draws(
        variables = variable,
        format = "draws_matrix"
    )

    draws <- as.matrix(draws)

    if (!ncol(draws) || is.null(colnames(draws))) {
        stop(
            "No posterior draws were returned for `",
            variable,
            "`.",
            call. = FALSE
        )
    }

    summary_index <- match(
        s$variable,
        colnames(draws)
    )

    if (anyNA(summary_index)) {
        stop(
            "CmdStan summary and posterior draw names disagree for `",
            variable,
            "`.",
            call. = FALSE
        )
    }

    qs <- vapply(
        seq_len(ncol(draws)),
        function(j) {
            stats::quantile(
                draws[, j],
                probs = c(.025, .5, .975),
                names = FALSE,
                na.rm = TRUE
            )
        },
        numeric(3L)
    )

    s[["q2.5"]] <-
        qs[1L, summary_index]

    s[["q50"]] <-
        qs[2L, summary_index]

    s[["q97.5"]] <-
        qs[3L, summary_index]
    data.frame(variable = s$variable, estimate = s$median, sd = s$sd, lower = s$q2.5, upper = s$q97.5, rhat = s$rhat, ess_bulk = s$ess_bulk, stringsAsFactors = FALSE)
}

.ep10_m3_truth_for_rows <- function(variable_names, truth) {
    root <- sub("\\[.*$", "", variable_names)
    index_text <- sub("^[^[]*\\[", "", variable_names)
    index_text <- sub("\\]$", "", index_text)
    out <- rep(NA_real_, length(variable_names))
    for (k in seq_along(variable_names)) {
        nm <- root[[k]]
        if (!nm %in% names(truth)) next
        z <- truth[[nm]]
        if (length(z) == 1L && !grepl("\\[", variable_names[[k]])) out[[k]] <- z
        else if (grepl("\\[", variable_names[[k]])) {
            idx <- as.integer(strsplit(index_text[[k]], ",", fixed = TRUE)[[1L]])
            if (length(idx) == 1L && idx <= length(z)) out[[k]] <- z[[idx]]
            else if (length(idx) == 2L) {
                mat <- if (grepl("corr_", nm)) matrix(z, nrow = 4L, ncol = 4L) else NULL
                if (!is.null(mat)) out[[k]] <- mat[idx[1L], idx[2L]]
            }
        }
    }
    out
}

.ep10_m3_recovery_one <- function(sim, fit_args = list()) {
    if (is.null(fit_args$pupil_scale)) fit_args$pupil_scale <- "raw"
    fit <- do.call(fit_multimodal_m3, c(list(x = sim), fit_args))
    vars <- c("theta", "tau", "omega", "rho", "b", "beta", "m", "kappa", "nu", "s", "sigma_person", "sigma_item", "corr_person", "corr_item", "sigma_pupil", "gamma_pupil")
    rows <- do.call(rbind, lapply(vars, function(v) .ep10_m3_draw_summary_vector(fit, v)))
    truth <- .ep10_m3_truth_map(sim, fit = fit)
    rows$truth <- .ep10_m3_truth_for_rows(rows$variable, truth)
    rows <- rows[is.finite(rows$truth), , drop = FALSE]
    rows$error <- rows$estimate - rows$truth
    rows$squared_error <- rows$error^2
    rows$covered95 <- rows$truth >= rows$lower & rows$truth <= rows$upper
    rows$family <- sub("\\[.*$", "", rows$variable)
    rows$scenario <- sim$truth$pupil_signal
    rows$missingness <- sim$truth$pupil_missingness
    rows
}

.ep10_m3_recovery_summary <- function(raw) {
    keys <- interaction(raw$scenario, raw$missingness, raw$family, drop = TRUE, lex.order = TRUE)
    do.call(rbind, lapply(split(raw, keys), function(z) {
        data.frame(
            scenario = z$scenario[[1L]], missingness = z$missingness[[1L]], family = z$family[[1L]], n = nrow(z),
            bias = mean(z$error), rmse = sqrt(mean(z$squared_error)), mean_posterior_sd = mean(z$sd), coverage95 = mean(z$covered95),
            max_rhat = max(z$rhat[is.finite(z$rhat)], na.rm = TRUE), min_bulk_ess = min(z$ess_bulk[is.finite(z$ess_bulk)], na.rm = TRUE),
            stringsAsFactors = FALSE
        )
    }))
}

#' Run M3 parameter-recovery and stress evidence
#'
#' Repeatedly simulates and fits M3 across pupil-signal and pupil-missingness
#' scenarios, summarizing bias, RMSE, posterior SD, interval coverage and MCMC
#' diagnostics. Small `reps` values are smoke tests; scientific promotion
#' requires a predeclared larger grid.
#'
#' @param reps Replications per design cell.
#' @param pupil_signal,pupil_missingness Scenario vectors.
#' @param n_person,n_item Design size.
#' @param seed Base seed.
#' @param fit_args Named sampling arguments for `fit_multimodal_m3()`.
#' @return An `eye_multimodal_m3_recovery` object.
#' @export
multimodal_m3_recovery <- function(
    reps = 3L,
    pupil_signal = c("informative", "weak", "null", "redundant", "confounded"),
    pupil_missingness = c("mcar", "quality", "device"),
    n_person = 80L,
    n_item = 10L,
    seed = 20260815L,
    fit_args = list(chains = 2L, parallel_chains = 2L, iter_warmup = 500L, iter_sampling = 300L, refresh = 0L, init = 0)
) {
    reps <- as.integer(reps)
    if (reps < 1L) stop("`reps` must be >= 1.", call. = FALSE)
    pupil_signal <- match.arg(pupil_signal, c("informative", "weak", "null", "redundant", "confounded"), several.ok = TRUE)
    pupil_missingness <- match.arg(pupil_missingness, c("mcar", "quality", "gaze", "ability", "device", "none"), several.ok = TRUE)
    design <- expand.grid(signal = pupil_signal, missingness = pupil_missingness, rep = seq_len(reps), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    raw <- vector("list", nrow(design))
    for (k in seq_len(nrow(design))) {
        sim <- simulate_multimodal_m3(n_person = n_person, n_item = n_item, pupil_signal = design$signal[[k]], pupil_missingness = design$missingness[[k]], seed = as.integer(seed + k - 1L))
        raw[[k]] <- .ep10_m3_recovery_one(sim, fit_args = c(fit_args, list(seed = as.integer(seed + 10000L + k))))
        raw[[k]]$rep <- design$rep[[k]]
    }
    raw <- do.call(rbind, raw)
    summary <- .ep10_m3_recovery_summary(raw)
    null_pupil <- summary[summary$scenario %in% c("null", "confounded") & summary$family %in% c("rho", "kappa"), , drop = FALSE]
    out <- list(
        raw = raw, summary = summary, design = design, null_pupil = null_pupil, seed = as.integer(seed),
        interpretation = paste(
            "Recovery evaluates estimator calibration under explicit generators.",
            "Null/confounded pupil scenarios are essential tests against false process-value claims.",
            "Recovery under the generating model is not empirical construct validity."
        )
    )
    class(out) <- c("eye_multimodal_m3_recovery", "list")
    out
}

print.eye_multimodal_m3_recovery <- function(x, ...) {
    cat("<eye_multimodal_m3_recovery>\n",
        "  design cells: ", nrow(x$design), "\n",
        "  parameter rows: ", nrow(x$raw), "\n",
        "  summary rows: ", nrow(x$summary), "\n",
        "  scenarios: ", paste(unique(x$design$signal), collapse = ", "), "\n",
        "  boundary: ", x$interpretation, "\n", sep = "")
    invisible(x)
}
