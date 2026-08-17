# eyeprocess 0.10 - M3 publication-oriented visual diagnostics
# Multiple evidence views are consolidated behind S3 `plot(..., type=)` methods.

# Register the ggplot2 data pronoun for R CMD check without creating a new
# package dependency or global plotting API.
if (getRversion() >= "2.15.1") utils::globalVariables(".data")

.ep10_m3_require_ggplot <- function() {
    if (!requireNamespace("ggplot2", quietly = TRUE)) stop("`ggplot2` is required for M3 plots.", call. = FALSE)
}

.ep10_m3_corr_summary <- function(x, which = c("person", "item")) {
    which <- match.arg(which)
    variable <- if (which == "person") "corr_person" else "corr_item"
    labels <- if (which == "person") c("ability", "speed", "gaze process", "pupil responsivity") else c("difficulty", "time intensity", "gaze intensity", "pupil intensity")
    sm <- x$fit$summary(variables = variable)
    parts <- regexec(paste0("^", variable, "\\[([0-9]+),([0-9]+)\\]$"), sm$variable)
    parsed <- regmatches(sm$variable, parts); keep <- lengths(parsed) == 3L
    parsed <- parsed[keep]; sm <- sm[keep, , drop = FALSE]
    rr <- as.integer(vapply(parsed, `[[`, character(1L), 2L)); cc <- as.integer(vapply(parsed, `[[`, character(1L), 3L))
    data.frame(row = factor(labels[rr], levels = labels), col = factor(labels[cc], levels = rev(labels)), correlation = sm$mean, stringsAsFactors = FALSE)
}

.ep10_m3_item_summary <- function(x) {
    vars <- c("b", "beta", "m", "kappa")
    out <- do.call(rbind, lapply(vars, function(v) {
        sm <- x$fit$summary(variables = v, probs = c(.05, .95))
        idx <- suppressWarnings(as.integer(sub(paste0("^", v, "\\[([0-9]+)\\]$"), "\\1", sm$variable)))
        keep <- is.finite(idx)
        data.frame(item_index = idx[keep], parameter = v, estimate = sm$mean[keep], lower = sm$q5[keep], upper = sm$q95[keep], stringsAsFactors = FALSE)
    }))
    out$item_id <- x$data$item_levels[out$item_index]
    out
}

plot.eye_multimodal_m3_simulation <- function(
    x,
    type = c("channels", "pupil_confounds", "missingness", "person_correlations", "item_correlations", "device", "pupil_truth"),
    ...
) {
    type <- match.arg(type); .ep10_m3_require_ggplot()
    if (type == "channels") {
        d <- rbind(
            data.frame(channel = "response", value = as.numeric(x$data$response)),
            data.frame(channel = "log RT", value = log(as.numeric(x$data$rt))),
            data.frame(channel = "gaze count", value = as.numeric(x$data$gaze)),
            data.frame(channel = "pupil", value = as.numeric(x$data$pupil))
        ); d <- d[is.finite(d$value), , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(x = .data$value)) + ggplot2::geom_histogram(bins = 30) + ggplot2::facet_wrap(~ channel, scales = "free") + ggplot2::labs(x = "Observed value", y = "Count", title = "Simulated M3 observed channels") + ggplot2::theme_minimal())
    }
    if (type == "pupil_confounds") {
        cols <- c(
            "pupil_baseline", "luminance", "gaze_x", "gaze_y",
            "pupil_quality", "pupil_blink", "pupil_interpolated", "time_on_task"
        )
        d <- do.call(rbind, lapply(cols, function(v) data.frame(covariate = v, value = x$data[[v]], pupil = x$data$pupil)))
        d <- d[stats::complete.cases(d), , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(x = .data$value, y = .data$pupil)) + ggplot2::geom_point(alpha = .25) + ggplot2::geom_smooth(method = "lm", se = FALSE) + ggplot2::facet_wrap(~ covariate, scales = "free_x") + ggplot2::labs(title = "Pupil measurement versus simulated nuisance variables", subtitle = "Associations are measurement diagnostics, not psychological constructs") + ggplot2::theme_minimal())
    }
    if (type == "missingness") {
        d <- data.frame(channel = c("response", "RT", "gaze", "pupil"), missing = c(mean(is.na(x$data$response)), mean(is.na(x$data$rt)), mean(is.na(x$data$gaze)), mean(is.na(x$data$pupil))))
        return(ggplot2::ggplot(d, ggplot2::aes(x = .data$channel, y = .data$missing)) + ggplot2::geom_col() + ggplot2::labs(x = NULL, y = "Missing fraction", title = paste("M3 missingness -", x$truth$pupil_missingness)) + ggplot2::theme_minimal())
    }
    if (type %in% c("person_correlations", "item_correlations")) {
        mat <- if (type == "person_correlations") x$truth$cor_person else x$truth$cor_item
        labels <- if (type == "person_correlations") c("ability", "speed", "gaze process", "pupil responsivity") else c("difficulty", "time intensity", "gaze intensity", "pupil intensity")
        d <- expand.grid(row = labels, col = labels, stringsAsFactors = FALSE); d$correlation <- as.numeric(mat)
        d$row <- factor(d$row, levels = labels); d$col <- factor(d$col, levels = rev(labels))
        return(ggplot2::ggplot(d, ggplot2::aes(.data$row, .data$col, fill = .data$correlation)) + ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$correlation))) + ggplot2::labs(x = NULL, y = NULL, fill = "Correlation", title = paste("Generating", ifelse(type == "person_correlations", "person", "item"), "correlations")) + ggplot2::theme_minimal())
    }
    if (type == "device") {
        d <- x$data[!is.na(x$data$device), c("device", "pupil"), drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(.data$device, .data$pupil)) + ggplot2::geom_boxplot() + ggplot2::labs(x = "Device", y = "Pupil", title = "Simulated device-dependent pupil measurement", subtitle = "A device shift is not measurement equivalence evidence") + ggplot2::theme_minimal())
    }
    d <- x$complete_data[, c("pupil_latent_mean", "pupil_nuisance_effect", "pupil")]
    ggplot2::ggplot(d, ggplot2::aes(.data$pupil_latent_mean, .data$pupil)) + ggplot2::geom_point(alpha = .25) + ggplot2::geom_smooth(method = "lm", se = FALSE) + ggplot2::labs(x = "Generating pupil process mean", y = "Complete pupil measurement", title = paste("M3 pupil truth -", x$truth$pupil_signal)) + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_fit <- function(
    x,
    type = c("person_correlations", "item_correlations", "item_parameters", "pupil_nuisance", "fitted_pupil", "diagnostics"),
    ...
) {
    type <- match.arg(type); .ep10_m3_require_ggplot()
    if (type %in% c("person_correlations", "item_correlations")) {
        d <- .ep10_m3_corr_summary(x, ifelse(type == "person_correlations", "person", "item"))
        return(ggplot2::ggplot(d, ggplot2::aes(.data$row, .data$col, fill = .data$correlation)) + ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$correlation))) + ggplot2::labs(x = NULL, y = NULL, fill = "Posterior mean", title = gsub("_", " ", type), subtitle = "Latent correlations are model parameters; construct interpretation requires external evidence") + ggplot2::theme_minimal())
    }
    if (type == "item_parameters") {
        d <- .ep10_m3_item_summary(x)
        return(ggplot2::ggplot(d, ggplot2::aes(.data$item_id, .data$estimate, group = .data$parameter)) + ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$lower, ymax = .data$upper)) + ggplot2::facet_wrap(~ parameter, scales = "free_y") + ggplot2::labs(x = "Item", y = "Posterior estimate", title = "M3 item parameter profiles") + ggplot2::theme_minimal())
    }
    if (type == "pupil_nuisance") {
        s <- x$fit$summary("gamma_pupil", probs = c(.05, .95)); s$covariate <- .ep10_m3_nuisance_names
        return(ggplot2::ggplot(s, ggplot2::aes(.data$covariate, .data$mean)) + ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$q5, ymax = .data$q95)) + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Standardized nuisance coefficient", title = "M3 pupil measurement nuisance terms") + ggplot2::theme_minimal())
    }
    if (type == "fitted_pupil") {
        f <- fitted(x); d <- data.frame(observed = x$data$raw$pupil, fitted = f$pupil_mean); d <- d[stats::complete.cases(d), , drop = FALSE]
        return(ggplot2::ggplot(d, ggplot2::aes(.data$fitted, .data$observed)) + ggplot2::geom_point(alpha = .35) + ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) + ggplot2::labs(x = "Posterior mean fitted pupil", y = "Observed pupil", title = "M3 pupil measurement fit") + ggplot2::theme_minimal())
    }
    da <- .ep10_m3_sampler_audit(x)
    d <- data.frame(diagnostic = c("max R-hat", "min bulk ESS", "min tail ESS", "divergences", "treedepth hits", "min E-BFMI"), value = unlist(da, use.names = FALSE))
    ggplot2::ggplot(d, ggplot2::aes(.data$diagnostic, .data$value)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Value", title = "M3 sampling diagnostics") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_ppc <- function(x, type = c("item_channel", "pupil_global", "flag_rate"), ...) {
    type <- match.arg(type); .ep10_m3_require_ggplot()
    if (type == "item_channel") return(ggplot2::ggplot(x$table, ggplot2::aes(.data$item_id, .data$ppp, group = .data$channel)) + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = c(.05, .95), linetype = 2) + ggplot2::facet_wrap(~ channel) + ggplot2::labs(x = "Item", y = "Posterior predictive p-value", title = "M3 item-by-channel PPC") + ggplot2::theme_minimal())
    if (type == "pupil_global") {
        d <- stats::reshape(x$pupil_global[, c("statistic", "observed", "replicated_median")], direction = "long", varying = c("observed", "replicated_median"), v.names = "value", timevar = "source", times = c("observed", "replicated median"))
        return(ggplot2::ggplot(d, ggplot2::aes(.data$statistic, .data$value, group = .data$source, shape = .data$source)) + ggplot2::geom_point(position = ggplot2::position_dodge(width = .25)) + ggplot2::labs(x = NULL, y = "Value", title = "Observed versus replicated pupil summaries") + ggplot2::theme_minimal())
    }
    d <- data.frame(channel = names(x$flag_rate_by_channel), rate = as.numeric(x$flag_rate_by_channel))
    ggplot2::ggplot(d, ggplot2::aes(.data$channel, .data$rate)) + ggplot2::geom_col() + ggplot2::labs(x = NULL, y = "Tail flag rate", title = "M3 PPC tail flags") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_information <- function(x, type = c("ablation", "incremental_pupil", "redundancy", "sensor_value", "conflict"), ...) {
    type <- match.arg(type); .ep10_m3_require_ggplot()
    if (type == "ablation") return(ggplot2::ggplot(x$table, ggplot2::aes(.data$model, .data$delta_elpd_vs_response)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Delta response ELPD vs response-only", title = "M3 response-target channel ablation") + ggplot2::theme_minimal())
    if (type == "incremental_pupil") return(ggplot2::ggplot(x$incremental_pupil, ggplot2::aes(.data$context, .data$delta_response_elpd)) + ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$delta_response_elpd - 1.96 * .data$se, ymax = .data$delta_response_elpd + 1.96 * .data$se)) + ggplot2::coord_flip() + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::labs(x = NULL, y = "Paired delta response ELPD", title = paste("Incremental pupil information -", x$verdict)) + ggplot2::theme_minimal())
    if (type == "redundancy") return(ggplot2::ggplot(x$nonadditivity, ggplot2::aes(.data$metric, .data$value)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::labs(x = NULL, y = "Non-additivity contrast", title = "M3 channel redundancy / complementarity") + ggplot2::theme_minimal())
    if (type == "sensor_value") return(ggplot2::ggplot(x$sensor_value, ggplot2::aes(.data$metric, .data$value)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Response-target value", title = "Pupil sensor value-of-information screen", subtitle = "Descriptive, model-conditional and not a causal cost-effectiveness estimate") + ggplot2::theme_minimal())
    ggplot2::ggplot(x$channel_conflict, ggplot2::aes(.data$max_rhat, .data$response_elpd, label = .data$model)) + ggplot2::geom_point() + ggplot2::geom_text(nudge_y = .1) + ggplot2::labs(x = "Maximum R-hat", y = "Response ELPD", title = "Channel conflict: predictive gain versus computational stability") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_validation <- function(x, type = c("checks", "missingness", "pupil_nuisance", "device"), ...) {
    type <- match.arg(type); .ep10_m3_require_ggplot()
    if (type == "checks") {
        d <- x$checks; d$pass_numeric <- as.numeric(d$pass)
        return(ggplot2::ggplot(d, ggplot2::aes(.data$check, .data$pass_numeric)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::scale_y_continuous(breaks = c(0, 1), labels = c("fail", "pass")) + ggplot2::labs(x = NULL, y = NULL, title = paste("M3 validation -", ifelse(x$valid, "PASS", "REVIEW"))) + ggplot2::theme_minimal())
    }
    if (type == "missingness") {
        d <- data.frame(channel = names(x$identifiability$missing_fraction), missing = as.numeric(x$identifiability$missing_fraction))
        return(ggplot2::ggplot(d, ggplot2::aes(.data$channel, .data$missing)) + ggplot2::geom_col() + ggplot2::labs(x = NULL, y = "Missing fraction", title = "M3 channel availability") + ggplot2::theme_minimal())
    }
    if (type == "pupil_nuisance") {
        d <- x$identifiability$pupil$nuisance
        return(ggplot2::ggplot(d, ggplot2::aes(.data$covariate, as.numeric(.data$available))) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Available", title = "Pupil nuisance measurement contract") + ggplot2::theme_minimal())
    }
    d <- x$identifiability$device
    ggplot2::ggplot(d, ggplot2::aes(.data$device, .data$pupil_missing)) + ggplot2::geom_col() + ggplot2::labs(x = "Device", y = "Pupil missing fraction", title = "Device-specific pupil availability") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_recovery <- function(x, type = c("rmse", "coverage", "pupil", "stress"), ...) {
    type <- match.arg(type); .ep10_m3_require_ggplot(); d <- x$summary
    if (type == "rmse") return(ggplot2::ggplot(d, ggplot2::aes(.data$family, .data$rmse, shape = .data$scenario)) + ggplot2::geom_point() + ggplot2::facet_wrap(~ missingness) + ggplot2::coord_flip() + ggplot2::labs(title = "M3 parameter recovery RMSE") + ggplot2::theme_minimal())
    if (type == "coverage") return(ggplot2::ggplot(d, ggplot2::aes(.data$family, .data$coverage95, shape = .data$scenario)) + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = .95, linetype = 2) + ggplot2::facet_wrap(~ missingness) + ggplot2::coord_flip() + ggplot2::labs(title = "M3 95% interval coverage") + ggplot2::theme_minimal())
    if (type == "pupil") {
        z <- d[d$family %in% c("rho", "kappa", "sigma_pupil", "gamma_pupil"), , drop = FALSE]
        return(ggplot2::ggplot(z, ggplot2::aes(.data$scenario, .data$rmse, shape = .data$missingness)) + ggplot2::geom_point() + ggplot2::facet_wrap(~ family, scales = "free_y") + ggplot2::labs(x = NULL, y = "RMSE", title = "Pupil-parameter recovery across signal scenarios") + ggplot2::theme_minimal())
    }
    ggplot2::ggplot(d, ggplot2::aes(.data$rmse, .data$coverage95, shape = .data$scenario)) + ggplot2::geom_point() + ggplot2::facet_wrap(~ missingness) + ggplot2::geom_hline(yintercept = .95, linetype = 2) + ggplot2::labs(x = "RMSE", y = "95% coverage", title = "M3 recovery stress map") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_negative_controls <- function(x, type = c("person_correlations", "pupil_alignment"), ...) {
    type <- match.arg(type); .ep10_m3_require_ggplot(); d <- x$diagnostics
    if (type == "pupil_alignment") d <- d[grepl("pupil", d$pair), , drop = FALSE]
    ggplot2::ggplot(d, ggplot2::aes(.data$dataset, .data$correlation, shape = .data$pair)) + ggplot2::geom_point() + ggplot2::coord_flip() + ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::labs(x = NULL, y = "Person-summary correlation", title = "M3 falsification-control alignment diagnostics", subtitle = "Controls are not causal interventions or behavioral classifiers") + ggplot2::theme_minimal()
}

plot.eye_multimodal_m3_identifiability <- function(x, type = c("checks", "missingness", "device"), ...) {
    type <- match.arg(type); .ep10_m3_require_ggplot()
    if (type == "checks") {
        d <- x$checks; d$pass_numeric <- as.numeric(d$pass)
        return(ggplot2::ggplot(d, ggplot2::aes(.data$check, .data$pass_numeric)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = "Pass", title = "M3 identifiability/support audit") + ggplot2::theme_minimal())
    }
    if (type == "missingness") {
        d <- data.frame(channel = names(x$missing_fraction), missing = as.numeric(x$missing_fraction))
        return(ggplot2::ggplot(d, ggplot2::aes(.data$channel, .data$missing)) + ggplot2::geom_col() + ggplot2::labs(x = NULL, y = "Missing fraction", title = "M3 channel missingness") + ggplot2::theme_minimal())
    }
    ggplot2::ggplot(x$device, ggplot2::aes(.data$device, .data$pupil_missing)) + ggplot2::geom_col() + ggplot2::labs(x = "Device", y = "Pupil missing fraction", title = "M3 device support audit") + ggplot2::theme_minimal()
}
