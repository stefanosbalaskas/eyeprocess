# eyeprocess 0.10 — M2 visual diagnostics
#
# Plot methods intentionally consolidate multiple evidence views behind
# `type=` rather than adding many exported plot helper names.

.ep10_m2_corr_summary <- function(x, which = c("person", "item")) {
    which <- match.arg(which)

    variable <- if (identical(which, "person")) {
        "corr_person"
    } else {
        "corr_item"
    }

    sm <- x$fit$summary(
        variables = variable
    )

    parts <- regexec(
        paste0(
            "^",
            variable,
            "\\[([0-9]+),([0-9]+)\\]$"
        ),
        sm$variable
    )

    parsed <- regmatches(
        sm$variable,
        parts
    )

    keep <- lengths(parsed) == 3L

    parsed <- parsed[keep]
    sm <- sm[keep, , drop = FALSE]

    row <- vapply(
        parsed,
        `[[`,
        character(1),
        2L
    )

    col <- vapply(
        parsed,
        `[[`,
        character(1),
        3L
    )

    labels <- if (identical(which, "person")) {
        c(
            "ability",
            "speed",
            "gaze process"
        )
    } else {
        c(
            "difficulty",
            "time intensity",
            "gaze intensity"
        )
    }

    data.frame(
        row = factor(
            labels[as.integer(row)],
            levels = labels
        ),
        col = factor(
            labels[as.integer(col)],
            levels = rev(labels)
        ),
        correlation = sm$mean,
        stringsAsFactors = FALSE
    )
}

.ep10_m2_item_summary <- function(x) {
    if (!identical(x$model, "M2")) {
        stop(
            "Item profile plotting currently requires an M2 fit.",
            call. = FALSE
        )
    }

    vars <- c(
        "b",
        "beta",
        "m"
    )

    tabs <- lapply(
        vars,
        function(v) {
            sm <- x$fit$summary(
                variables = v
            )

            idx <- suppressWarnings(
                as.integer(
                    sub(
                        paste0(
                            "^",
                            v,
                            "\\[([0-9]+)\\]$"
                        ),
                        "\\1",
                        sm$variable
                    )
                )
            )

            keep <- is.finite(idx)

            data.frame(
                item_index = idx[keep],
                parameter = v,
                estimate = sm$mean[keep],
                lower = if ("q5" %in% names(sm)) {
                    sm$q5[keep]
                } else {
                    NA_real_
                },
                upper = if ("q95" %in% names(sm)) {
                    sm$q95[keep]
                } else {
                    NA_real_
                },
                stringsAsFactors = FALSE
            )
        }
    )

    d <- do.call(
        rbind,
        tabs
    )

    d$item_id <- x$data$item_levels[
        d$item_index
    ]

    d
}


# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_simulation <- function(
    x,
    type = c(
        "channel_distributions",
        "person_latent_correlations",
        "item_correlations",
        "item_truth"
    ),
    ...
) {
    type <- match.arg(type)

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    if (identical(type, "channel_distributions")) {
        d <- rbind(
            data.frame(
                channel = "response",
                value = as.numeric(x$data$response),
                stringsAsFactors = FALSE
            ),
            data.frame(
                channel = "log RT",
                value = log(as.numeric(x$data$rt)),
                stringsAsFactors = FALSE
            ),
            data.frame(
                channel = "gaze count",
                value = as.numeric(x$data$gaze),
                stringsAsFactors = FALSE
            )
        )

        d <- d[
            is.finite(d$value),
            ,
            drop = FALSE
        ]

        return(
            ggplot2::ggplot(
                d,
                ggplot2::aes(
                    x = d[["value"]]
                )
            ) +
                ggplot2::geom_histogram(
                    bins = 25
                ) +
                ggplot2::facet_wrap(
                    stats::as.formula("~ channel"),
                    scales = "free"
                ) +
                ggplot2::labs(
                    x = "Observed value",
                    y = "Count",
                    title = "Simulated M2 channel distributions"
                ) +
                ggplot2::theme_minimal()
        )
    }

    if (
        type %in% c(
            "person_latent_correlations",
            "item_correlations"
        )
    ) {
        mat <- if (
            identical(
                type,
                "person_latent_correlations"
            )
        ) {
            x$truth$cor_person
        } else {
            x$truth$cor_item
        }

        labels <- if (
            identical(
                type,
                "person_latent_correlations"
            )
        ) {
            c(
                "ability",
                "speed",
                "gaze process"
            )
        } else {
            c(
                "difficulty",
                "time intensity",
                "gaze intensity"
            )
        }

        d <- expand.grid(
            row = labels,
            col = labels,
            KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = FALSE
        )

        d$correlation <- as.numeric(
            mat
        )

        d$row <- factor(
            d$row,
            levels = labels
        )

        d$col <- factor(
            d$col,
            levels = rev(labels)
        )

        return(
            ggplot2::ggplot(
                d,
                ggplot2::aes(
                    x = d[["row"]],
                    y = d[["col"]],
                    fill = d[["correlation"]]
                )
            ) +
                ggplot2::geom_tile() +
                ggplot2::geom_text(
                    ggplot2::aes(
                        label = sprintf(
                            "%.2f",
                            d[["correlation"]]
                        )
                    )
                ) +
                ggplot2::labs(
                    x = NULL,
                    y = NULL,
                    fill = "Correlation",
                    title = if (
                        identical(
                            type,
                            "person_latent_correlations"
                        )
                    ) {
                        "Generating person-side correlation"
                    } else {
                        "Generating item-side correlation"
                    }
                ) +
                ggplot2::theme_minimal()
        )
    }

    I <- x$truth$n_item

    d <- rbind(
        data.frame(
            item = seq_len(I),
            parameter = "difficulty",
            value = x$truth$b,
            stringsAsFactors = FALSE
        ),
        data.frame(
            item = seq_len(I),
            parameter = "time intensity",
            value = x$truth$beta,
            stringsAsFactors = FALSE
        ),
        data.frame(
            item = seq_len(I),
            parameter = "gaze intensity",
            value = x$truth$m,
            stringsAsFactors = FALSE
        )
    )

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["item"]],
            y = d[["value"]]
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(
            stats::as.formula("~ parameter"),
            scales = "free_y"
        ) +
        ggplot2::labs(
            x = "Item",
            y = "Generating value",
            title = "Simulated M2 item truth"
        ) +
        ggplot2::theme_minimal()
}

# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_fit <- function(
    x,
    type = c(
        "person_correlations",
        "item_correlations",
        "item_parameters",
        "diagnostics"
    ),
    ...
) {
    type <- match.arg(type)

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    if (type %in% c(
        "person_correlations",
        "item_correlations"
    )) {
        which <- if (
            identical(
                type,
                "person_correlations"
            )
        ) {
            "person"
        } else {
            "item"
        }

        d <- .ep10_m2_corr_summary(
            x,
            which = which
        )

        return(
            ggplot2::ggplot(
                d,
                ggplot2::aes(
                    x = d[["row"]],
                    y = d[["col"]],
                    fill = d[["correlation"]]
                )
            ) +
                ggplot2::geom_tile() +
                ggplot2::geom_text(
                    ggplot2::aes(
                        label = sprintf(
                            "%.2f",
                            d[["correlation"]]
                        )
                    )
                ) +
                ggplot2::labs(
                    x = NULL,
                    y = NULL,
                    fill = "Posterior mean",
                    title = if (
                        identical(
                            which,
                            "person"
                        )
                    ) {
                        "Person-side latent correlation"
                    } else {
                        "Item-side parameter correlation"
                    },
                    subtitle = paste(
                        "Correlations are model parameters;",
                        "construct interpretation requires external evidence."
                    )
                ) +
                ggplot2::theme_minimal()
        )
    }

    if (identical(type, "item_parameters")) {
        d <- .ep10_m2_item_summary(x)

        return(
            ggplot2::ggplot(
                d,
                ggplot2::aes(
                    x = d[["item_id"]],
                    y = d[["estimate"]],
                    group = d[["parameter"]]
                )
            ) +
                ggplot2::geom_point() +
                ggplot2::geom_line() +
                ggplot2::facet_wrap(
                    stats::as.formula("~ parameter"),
                    scales = "free_y"
                ) +
                ggplot2::labs(
                    x = "Item",
                    y = "Posterior mean",
                    title = "M2 item parameter profiles",
                    subtitle = "b = difficulty, beta = log-time intensity, m = log-gaze intensity"
                ) +
                ggplot2::theme_minimal()
        )
    }

    da <- .ep10_m2_sampler_audit(x)

    d <- data.frame(
        diagnostic = c(
            "max R-hat",
            "min bulk ESS",
            "min tail ESS",
            "divergences",
            "max treedepth hits"
        ),
        value = c(
            da$max_rhat,
            da$min_ess_bulk,
            da$min_ess_tail,
            da$divergences,
            da$max_treedepth_hits
        ),
        stringsAsFactors = FALSE
    )

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["diagnostic"]],
            y = d[["value"]]
        )
    ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::labs(
            x = NULL,
            y = "Value",
            title = "M2 sampling diagnostics"
        ) +
        ggplot2::theme_minimal()
}

# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_ppc <- function(
    x,
    type = "ppp",
    ...
) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    if (!identical(type, "ppp")) {
        stop(
            "Supported PPC plot type is `ppp`.",
            call. = FALSE
        )
    }

    d <- x$table

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["item_id"]],
            y = d[["ppp"]],
            group = d[["channel"]]
        )
    ) +
        ggplot2::geom_hline(
            yintercept = c(
                0.05,
                0.5,
                0.95
            ),
            linetype = c(
                2,
                1,
                2
            )
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(
            stats::as.formula("~ channel")
        ) +
        ggplot2::coord_cartesian(
            ylim = c(
                0,
                1
            )
        ) +
        ggplot2::labs(
            x = "Item",
            y = "Posterior predictive p-value",
            title = "M2 channel-specific posterior predictive checks",
            subtitle = "W response, L response-time, and M fixation-count discrepancies"
        ) +
        ggplot2::theme_minimal()
}

# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_information <- function(
    x,
    type = c(
        "theta_variance",
        "response_elpd"
    ),
    ...
) {
    type <- match.arg(type)

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    d <- x$table

    if (identical(type, "theta_variance")) {
        return(
            ggplot2::ggplot(
                d,
                ggplot2::aes(
                    x = d[["model"]],
                    y = d[["mean_theta_posterior_variance"]]
                )
            ) +
                ggplot2::geom_col() +
                ggplot2::labs(
                    x = "Model",
                    y = "Mean posterior variance of ability",
                    title = "Response-trait uncertainty across channel ablations",
                    subtitle = "M0 response; M1 + response time; M2 + gaze"
                ) +
                ggplot2::theme_minimal()
        )
    }

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["model"]],
            y = d[["delta_response_elpd_vs_M0"]]
        )
    ) +
        ggplot2::geom_col() +
        ggplot2::geom_errorbar(
            ggplot2::aes(
                ymin = d[["delta_response_elpd_vs_M0"]] -
                    d[["delta_response_elpd_se_vs_M0"]],
                ymax = d[["delta_response_elpd_vs_M0"]] +
                    d[["delta_response_elpd_se_vs_M0"]]
            ),
            width = 0.15
        ) +
        ggplot2::geom_hline(
            yintercept = 0
        ) +
        ggplot2::labs(
            x = "Model",
            y = "Response-target ELPD difference vs M0",
            title = "Held-out response information from process channels",
            subtitle = "Held-out response cells for observed persons/items; channel information is not assumed additive."
        ) +
        ggplot2::theme_minimal()
}

# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_validation <- function(
    x,
    type = "checks",
    ...
) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    if (!identical(type, "checks")) {
        stop(
            "Supported validation plot type is `checks`.",
            call. = FALSE
        )
    }

    d <- x$checks
    d$score <- as.integer(d$pass)

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["check"]],
            y = d[["score"]]
        )
    ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
            breaks = c(
                0,
                1
            ),
            labels = c(
                "fail",
                "pass"
            )
        ) +
        ggplot2::labs(
            x = NULL,
            y = NULL,
            title = "M2 validation contract"
        ) +
        ggplot2::theme_minimal()
}

# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_recovery <- function(
    x,
    type = c(
        "truth_vs_estimate",
        "coverage"
    ),
    family = NULL,
    ...
) {
    type <- match.arg(type)

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    if (identical(type, "truth_vs_estimate")) {
        d <- x$raw

        if (!is.null(family)) {
            d <- d[
                d$family %in% family,
                ,
                drop = FALSE
            ]
        }

        return(
            ggplot2::ggplot(
                d,
                ggplot2::aes(
                    x = d[["truth"]],
                    y = d[["estimate"]]
                )
            ) +
                ggplot2::geom_abline(
                    slope = 1,
                    intercept = 0
                ) +
                ggplot2::geom_point() +
                ggplot2::facet_wrap(
                    stats::as.formula("~ family"),
                    scales = "free"
                ) +
                ggplot2::labs(
                    x = "Generating value",
                    y = "Posterior mean",
                    title = "M2 parameter recovery",
                    subtitle = "Recovery under the generating model is not empirical construct validation."
                ) +
                ggplot2::theme_minimal()
        )
    }

    d <- x$summary

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["family"]],
            y = d[["coverage95"]]
        )
    ) +
        ggplot2::geom_hline(
            yintercept = 0.95,
            linetype = 2
        ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(
            limits = c(
                0,
                1
            )
        ) +
        ggplot2::labs(
            x = NULL,
            y = "95% interval coverage",
            title = "M2 recovery coverage"
        ) +
        ggplot2::theme_minimal()
}

# S3 method registered in the hand-maintained NAMESPACE.
plot.eye_multimodal_m2_negative_controls <- function(
    x,
    type = "person_correlations",
    ...
) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop(
            "`ggplot2` is required for M2 plots.",
            call. = FALSE
        )
    }

    if (!identical(type, "person_correlations")) {
        stop(
            "Supported negative-control plot type is `person_correlations`.",
            call. = FALSE
        )
    }

    d <- x$diagnostics

    ggplot2::ggplot(
        d,
        ggplot2::aes(
            x = d[["dataset"]],
            y = d[["correlation"]],
            group = d[["pair"]]
        )
    ) +
        ggplot2::geom_hline(
            yintercept = 0
        ) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(
            stats::as.formula("~ pair")
        ) +
        ggplot2::labs(
            x = "Dataset / negative control",
            y = "Person-level aggregate correlation",
            title = "M2 alignment negative controls",
            subtitle = "Within-item shuffles preserve marginal values while breaking named person-level alignment."
        ) +
        ggplot2::theme_minimal()
}
