# eyeprocess 0.11 - M4 publication-quality S3 plotting layer

# ggplot2 data-mask variables used by the M4 S3 plotting layer.
# Declared explicitly for R CMD check; this has no runtime model effect.
utils::globalVariables(c(
    "K",
    "MAP_fraction",
    "MAP_state",
    "brier",
    "channel",
    "contrast",
    "control",
    "criterion",
    "delta_elpd_vs_M3",
    "delta_response_elpd",
    "dimension",
    "domain",
    "entropy",
    "from",
    "index",
    "item_id",
    "max_pareto_k",
    "mean_entropy",
    "mean_probability",
    "metric",
    "model",
    "occupancy_rmse",
    "parameter_rmse",
    "person_id",
    "posterior_entropy",
    "posterior_weighted_mean",
    "probability",
    "run_length",
    "scenario",
    "se",
    "setting",
    "state",
    "status",
    "theta_variance_reduction_vs_M3",
    "to",
    "transition_rmse",
    "trial_index",
    "value"
))


.ep11_m4_require_ggplot <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("M4 plotting requires the optional `ggplot2` package.", call. = FALSE)
    }
}

.ep11_m4_prob_long <- function(states) {
    pcols <- grep("^state_[0-9]+_probability$", names(states$probability), value = TRUE)
    rows <- lapply(pcols, function(nm) {
        k <- as.integer(sub("^state_([0-9]+)_probability$", "\\1", nm))
        data.frame(
            source_row = states$probability$source_row,
            person_id = states$probability$person_id,
            sequence_id = states$probability$sequence_id,
            trial_index = states$probability$trial_index,
            state = paste0("state_", k),
            probability = states$probability[[nm]],
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

plot.eye_multimodal_m4_simulation <- function(x, type = c("state_sequence", "channel_profile", "transition", "missingness"), person = NULL, ...) {
    .ep11_m4_require_ggplot()
    type <- match.arg(type)
    st <- multimodal_m4_state_diagnostics(x)
    if (type == "state_sequence") {
        d <- st$probability
        if (is.null(person)) person <- unique(d$person_id)[[1L]]
        d <- d[d$person_id == person, , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(x = trial_index, y = MAP_state)) +
            ggplot2::geom_step() + ggplot2::geom_point() +
            ggplot2::facet_wrap(~sequence_id, scales = "free_x") +
            ggplot2::labs(x = "Within-sequence trial", y = "Synthetic state", title = "M4 synthetic state sequence", subtitle = "Known simulation truth; no psychological meaning implied") +
            ggplot2::theme_minimal())
    }
    if (type == "channel_profile") {
        return(ggplot2::ggplot(st$channel_profile, ggplot2::aes(x = factor(state), y = posterior_weighted_mean, group = channel)) +
            ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::facet_wrap(~channel, scales = "free_y") +
            ggplot2::labs(x = "Synthetic state", y = "Mean", title = "Synthetic multichannel state profiles") + ggplot2::theme_minimal())
    }
    if (type == "transition") {
        return(ggplot2::ggplot(st$transition, ggplot2::aes(x = factor(to), y = factor(from), fill = probability)) +
            ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", probability))) +
            ggplot2::labs(x = "To state", y = "From state", fill = "Probability", title = "Synthetic transition probabilities") + ggplot2::theme_minimal())
    }
    d <- data.frame(channel = c("response", "rt", "gaze", "pupil"), missing = c(mean(is.na(x$data$response)), mean(is.na(x$data$rt)), mean(is.na(x$data$gaze)), mean(is.na(x$data$pupil))))
    ggplot2::ggplot(d, ggplot2::aes(x = channel, y = missing)) + ggplot2::geom_col() +
        ggplot2::labs(x = NULL, y = "Missing fraction", title = "M4 simulation missingness") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m4_fit <- function(x, type = c("state_probability", "state_sequence", "occupancy", "transition", "entropy", "channel_profile", "trait_transition"), person = NULL, ...) {
    .ep11_m4_require_ggplot()
    type <- match.arg(type)
    st <- multimodal_m4_state_diagnostics(x)
    if (type == "state_probability") {
        d <- .ep11_m4_prob_long(st)
        if (is.null(person)) person <- unique(d$person_id)[[1L]]
        d <- d[d$person_id == person, , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(x = trial_index, y = probability, linetype = state, group = state)) +
            ggplot2::geom_line() + ggplot2::facet_wrap(~sequence_id, scales = "free_x") +
            ggplot2::labs(x = "Within-sequence trial", y = "Posterior state probability", linetype = "State", title = "M4 posterior state probabilities", subtitle = "Probabilities retain latent-state uncertainty") + ggplot2::theme_minimal())
    }
    if (type == "state_sequence") {
        d <- st$probability
        if (is.null(person)) person <- unique(d$person_id)[[1L]]
        d <- d[d$person_id == person, , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(x = trial_index, y = MAP_state, alpha = 1 / (1 + posterior_entropy))) +
            ggplot2::geom_step() + ggplot2::geom_point() + ggplot2::facet_wrap(~sequence_id, scales = "free_x") +
            ggplot2::labs(x = "Within-sequence trial", y = "MAP state", alpha = "Relative certainty", title = "M4 secondary MAP state path", subtitle = "Hard labels are secondary to posterior probabilities") + ggplot2::theme_minimal())
    }
    if (type == "occupancy") {
        return(ggplot2::ggplot(st$occupancy, ggplot2::aes(x = factor(state), y = mean_probability)) +
            ggplot2::geom_col() + ggplot2::geom_point(ggplot2::aes(y = MAP_fraction)) +
            ggplot2::labs(x = "State", y = "Occupancy", title = "M4 posterior state occupancy", subtitle = "Bars: mean posterior probability; points: MAP fraction") + ggplot2::theme_minimal())
    }
    if (type == "transition") {
        return(ggplot2::ggplot(st$transition, ggplot2::aes(x = factor(to), y = factor(from), fill = probability)) +
            ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", probability))) +
            ggplot2::labs(x = "To state", y = "From state", fill = "Probability", title = "M4 mean posterior transition matrix") + ggplot2::theme_minimal())
    }
    if (type == "entropy") {
        return(ggplot2::ggplot(st$probability, ggplot2::aes(x = posterior_entropy)) + ggplot2::geom_histogram(bins = 30) +
            ggplot2::labs(x = "Posterior state entropy", y = "Trials", title = "M4 assignment uncertainty") + ggplot2::theme_minimal())
    }
    if (type == "channel_profile") {
        return(ggplot2::ggplot(st$channel_profile, ggplot2::aes(x = factor(state), y = posterior_weighted_mean, group = channel)) +
            ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::facet_wrap(~channel, scales = "free_y") +
            ggplot2::labs(x = "State", y = "Posterior-weighted mean", title = "M4 multichannel state profiles", subtitle = "Descriptive process profiles; no construct label implied") + ggplot2::theme_minimal())
    }
    K <- x$spec$n_states
    if (K <= 1L) stop("Trait-transition plots require K > 1.", call. = FALSE)
    theta <- .ep11_m4_draw_mean(x, "theta")
    A <- .ep11_m4_indexed_mean(x, "transition_prob_person", c(length(theta), K, K))
    persistence <- as.numeric(vapply(seq_along(theta), function(j) diag(A[j, , ]), numeric(K)))
    d <- data.frame(theta = rep(theta, each = K), from = rep(seq_len(K), times = length(theta)), persistence = persistence)
    ggplot2::ggplot(d, ggplot2::aes(x = theta, y = persistence)) + ggplot2::geom_point(alpha = 0.5) + ggplot2::geom_smooth(method = "lm", se = TRUE) +
        ggplot2::facet_wrap(~from) + ggplot2::labs(x = "Posterior mean ability trait", y = "Posterior mean state persistence", title = "Trait-conditioned transition relationship", subtitle = "Association under the fitted model; not causal") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m4_states <- function(x, type = c("probability", "occupancy", "transition", "dwell", "entropy", "participant", "item", "channel_profile"), ...) {
    .ep11_m4_require_ggplot()
    type <- match.arg(type)
    if (type == "probability") {
        d <- .ep11_m4_prob_long(x)
        person <- unique(d$person_id)[[1L]]; d <- d[d$person_id == person, , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(trial_index, probability, linetype = state, group = state)) + ggplot2::geom_line() + ggplot2::facet_wrap(~sequence_id, scales = "free_x") + ggplot2::theme_minimal() + ggplot2::labs(x = "Trial", y = "State probability", title = "Latent-state probabilities"))
    }
    if (type == "occupancy") return(ggplot2::ggplot(x$occupancy, ggplot2::aes(factor(state), mean_probability)) + ggplot2::geom_col() + ggplot2::theme_minimal() + ggplot2::labs(x = "State", y = "Mean posterior probability", title = "State occupancy"))
    if (type == "transition") return(ggplot2::ggplot(x$transition, ggplot2::aes(factor(to), factor(from), fill = probability)) + ggplot2::geom_tile() + ggplot2::theme_minimal() + ggplot2::labs(x = "To", y = "From", fill = "Probability", title = "Transition matrix"))
    if (type == "dwell") return(ggplot2::ggplot(x$runs, ggplot2::aes(run_length)) + ggplot2::geom_histogram(binwidth = 1) + ggplot2::facet_wrap(~state) + ggplot2::theme_minimal() + ggplot2::labs(x = "MAP run length", y = "Runs", title = "State dwell/run lengths"))
    if (type == "entropy") return(ggplot2::ggplot(x$probability, ggplot2::aes(posterior_entropy)) + ggplot2::geom_histogram(bins = 30) + ggplot2::theme_minimal() + ggplot2::labs(x = "Posterior entropy", y = "Trials", title = "State-assignment uncertainty"))
    if (type == "participant") return(ggplot2::ggplot(x$participant, ggplot2::aes(factor(state), mean_probability, group = person_id)) + ggplot2::geom_line(alpha = 0.25) + ggplot2::theme_minimal() + ggplot2::labs(x = "State", y = "Mean probability", title = "Participant-level state composition"))
    if (type == "item") return(ggplot2::ggplot(x$item, ggplot2::aes(factor(state), mean_probability, group = item_id)) + ggplot2::geom_line(alpha = 0.25) + ggplot2::theme_minimal() + ggplot2::labs(x = "State", y = "Mean probability", title = "Item-level state composition"))
    ggplot2::ggplot(x$channel_profile, ggplot2::aes(factor(state), posterior_weighted_mean, group = channel)) + ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::facet_wrap(~channel, scales = "free_y") + ggplot2::theme_minimal() + ggplot2::labs(x = "State", y = "Posterior-weighted mean", title = "Multimodal state profile")
}

plot.eye_multimodal_m4_identifiability <- function(x, type = c("checks", "missingness", "entropy"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (type == "checks") {
        d <- x$checks; d$criterion <- factor(d$criterion, levels = rev(unique(d$criterion)))
        return(ggplot2::ggplot(d, ggplot2::aes(x = status, y = criterion)) + ggplot2::geom_point(size = 3) + ggplot2::facet_wrap(~domain, scales = "free_y") + ggplot2::theme_minimal() + ggplot2::labs(x = "Status", y = NULL, title = paste("M4 identifiability audit:", x$overall)))
    }
    if (type == "missingness") {
        d <- data.frame(channel = names(x$missingness), missing = as.numeric(x$missingness))
        return(ggplot2::ggplot(d, ggplot2::aes(channel, missing)) + ggplot2::geom_col() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Missing fraction", title = "M4 process-channel missingness"))
    }
    if (is.null(x$posterior)) stop("Posterior entropy is unavailable in a pre-fit audit.", call. = FALSE)
    d <- data.frame(entropy = x$posterior$entropy)
    ggplot2::ggplot(d, ggplot2::aes(entropy)) + ggplot2::geom_histogram(bins = 30) + ggplot2::theme_minimal() + ggplot2::labs(x = "Posterior entropy", y = "Trials", title = "M4 state uncertainty")
}

plot.eye_multimodal_m4_ppc <- function(x, type = c("measurement", "dynamics", "state_dynamics"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (type == "measurement") {
        d <- rbind(data.frame(channel = x$channels$channel, source = "observed", value = x$channels$observed_mean), data.frame(channel = x$channels$channel, source = "replicated", value = x$channels$replicated_mean))
        return(ggplot2::ggplot(d, ggplot2::aes(channel, value, shape = source, group = source)) + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Mean", title = "M4 posterior predictive measurement means"))
    }
    if (type == "dynamics") {
        d <- rbind(data.frame(channel = x$dynamics$channel, source = "observed", value = x$dynamics$observed_lag1), data.frame(channel = x$dynamics$channel, source = "replicated", value = x$dynamics$replicated_lag1))
        return(ggplot2::ggplot(d, ggplot2::aes(channel, value, shape = source, group = source)) + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Mean lag-1 correlation", title = "Observed vs replicated process dynamics"))
    }
    d <- x$state_dynamics
    long <- rbind(data.frame(metric = d$metric, source = "inferred", value = d$inferred_posterior_summary), data.frame(metric = d$metric, source = "replicated", value = d$replicated_trajectory_summary))
    ggplot2::ggplot(long, ggplot2::aes(metric, value, shape = source, group = source)) + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Summary", title = "Latent-state dynamic summaries")
}

plot.eye_multimodal_m4_information <- function(x, type = c("elpd", "contrasts", "theta_uncertainty", "pareto"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (type == "elpd") return(ggplot2::ggplot(x$table, ggplot2::aes(model, delta_elpd_vs_M3)) + ggplot2::geom_col() + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Delta response-target ELPD vs M3", title = paste("M4 incremental information:", x$verdict)))
    if (type == "contrasts") return(ggplot2::ggplot(x$comparisons, ggplot2::aes(contrast, delta_response_elpd, ymin = delta_response_elpd - 2 * se, ymax = delta_response_elpd + 2 * se)) + ggplot2::geom_pointrange() + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Delta response-target ELPD", title = "Focused M4 evidence contrasts"))
    if (type == "theta_uncertainty") return(ggplot2::ggplot(x$table, ggplot2::aes(model, theta_variance_reduction_vs_M3)) + ggplot2::geom_col() + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Ability variance reduction vs M3", title = "M4 measurement-uncertainty change"))
    ggplot2::ggplot(x$table, ggplot2::aes(model, max_pareto_k)) + ggplot2::geom_point(size = 3) + ggplot2::geom_hline(yintercept = 0.7, linetype = 2) + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Maximum Pareto k", title = "M4 PSIS-LOO diagnostics")
}

plot.eye_multimodal_m4_negative_controls <- function(x, type = c("design", "fitted_entropy"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (type == "design" || !isTRUE(x$executed)) {
        d <- data.frame(control = x$controls, index = seq_along(x$controls))
        return(ggplot2::ggplot(d, ggplot2::aes(index, reorder(control, index))) + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = "Control index", y = NULL, title = "M4 negative-control battery"))
    }
    d <- do.call(rbind, lapply(names(x$fits), function(nm) data.frame(control = nm, mean_entropy = multimodal_m4_state_diagnostics(x$fits[[nm]])$summary$mean_entropy)))
    ggplot2::ggplot(d, ggplot2::aes(control, mean_entropy)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Mean state entropy", title = "Negative-control state uncertainty")
}

plot.eye_multimodal_m4_sensitivity <- function(x, type = c("state_count", "occupancy", "entropy", "separation"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (!isTRUE(x$executed)) {
        d <- x$design; d$index <- seq_len(nrow(d))
        return(ggplot2::ggplot(d, ggplot2::aes(index, reorder(paste(dimension, setting, sep = ": "), index))) + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = "Sensitivity-design row", y = NULL, title = "M4 sensitivity design (not executed)"))
    }
    d <- x$diagnostics
    y <- switch(type, state_count = "state_separation", occupancy = "occupancy_min", entropy = "mean_assignment_entropy", separation = "state_separation")
    d$value <- d[[y]]
    ggplot2::ggplot(d, ggplot2::aes(x = K, y = value)) + ggplot2::geom_line() + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = "Number of states K", y = y, title = "M4 state-count sensitivity")
}

plot.eye_multimodal_m4_recovery <- function(x, type = c("parameter_rmse", "state_effect", "probability_calibration", "occupancy", "transition"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (!isTRUE(x$executed)) {
        d <- x$design; d$index <- seq_len(nrow(d))
        return(ggplot2::ggplot(d, ggplot2::aes(index, reorder(scenario, index))) + ggplot2::geom_point(size = 3) + ggplot2::theme_minimal() + ggplot2::labs(x = "Scenario", y = NULL, title = "M4 deterministic recovery battery (design only)"))
    }
    if (type == "parameter_rmse") return(ggplot2::ggplot(x$summary, ggplot2::aes(scenario, parameter_rmse)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "RMSE", title = "M4 person/item parameter recovery"))
    if (type == "probability_calibration") return(ggplot2::ggplot(x$summary, ggplot2::aes(scenario, brier)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Multiclass Brier score", title = "M4 state-probability recovery"))
    if (type == "transition") return(ggplot2::ggplot(x$summary, ggplot2::aes(scenario, transition_rmse)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Transition RMSE", title = "M4 transition recovery"))
    if (type == "occupancy") return(ggplot2::ggplot(x$summary, ggplot2::aes(scenario, occupancy_rmse)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "Occupancy RMSE", title = "M4 occupancy recovery"))
    d <- do.call(rbind, lapply(names(x$raw), function(nm) transform(x$raw[[nm]]$state_effect, scenario = nm)))
    ggplot2::ggplot(d, ggplot2::aes(truth, estimate, shape = factor(state))) + ggplot2::geom_point(size = 3) + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) + ggplot2::facet_wrap(~family, scales = "free") + ggplot2::theme_minimal() + ggplot2::labs(x = "Truth", y = "Posterior mean", shape = "State", title = "M4 state-effect recovery")
}

plot.eye_multimodal_m4_validation <- function(x, type = c("domains", "checks"), ...) {
    .ep11_m4_require_ggplot(); type <- match.arg(type)
    if (type == "domains") {
        d <- x$domains; d$domain <- factor(d$domain, levels = rev(d$domain))
        return(ggplot2::ggplot(d, ggplot2::aes(status, domain)) + ggplot2::geom_point(size = 4) + ggplot2::theme_minimal() + ggplot2::labs(x = "Status", y = NULL, title = paste("M4 validation:", x$overall), subtitle = "Validation does not confer psychological meaning on state labels"))
    }
    plot(x$identifiability, type = "checks")
}
