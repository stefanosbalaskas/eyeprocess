.multimodal_stan_file <- function(model) {
    fname <- switch(model,
        M2 = "three-way-response-rt-gaze.stan",
        M3 = "four-channel-response-rt-gaze-pupil.stan",
        stop("No bundled Stan programme for model ", model, ".", call.=FALSE)
    )
    p <- system.file("stan", fname, package="eyeprocess")
    if (!nzchar(p) || !file.exists(p))
        stop("Bundled Stan programme not found: ", fname, call.=FALSE)
    p
}

#' Report multimodal backend availability
#'
#' @return A data.frame describing optional backend availability.
#' @export
multimodal_backend_status <- function() {
    pkgs <- c("mirt","cmdstanr","posterior","loo","TMB","brms")
    out <- data.frame(
        backend=pkgs,
        installed=vapply(pkgs, requireNamespace, logical(1), quietly=TRUE),
        version=vapply(pkgs, function(p) if (requireNamespace(p, quietly=TRUE))
            as.character(utils::packageVersion(p)) else NA_character_, character(1)),
        stringsAsFactors=FALSE
    )
    if ("cmdstanr" %in% out$backend && out$installed[out$backend=="cmdstanr"]) {
        ok <- tryCatch({
            p <- cmdstanr::cmdstan_path()
            nzchar(p) && dir.exists(p)
        }, error=function(e) FALSE)
        out$engine_ready <- NA
        out$engine_ready[out$backend=="cmdstanr"] <- ok
    } else out$engine_ready <- NA
    out
}

#' Internal CmdStan development fitter for staged multimodal IRT
#'
#' M2 and M3 use bundled CmdStan development programmes. They are deliberately
#' classified as gated/experimental until recovery, SBC, misspecification, and
#' empirical reproduction evidence is frozen.
#'
#' @param data An `eye_multimodal_measurement`.
#' @param spec An `eye_multimodal_irt_spec`.
#' @param chains,parallel_chains,iter_warmup,iter_sampling CmdStan controls.
#' @param seed Random seed.
#' @param ... Additional arguments passed to `CmdStanModel$sample()`.
#' @return An `eye_multimodal_irt_fit`.
#' @noRd
.fit_multimodal_irt_cmdstan_0_10 <- function(
    data,
    spec,
    chains = 4L,
    parallel_chains = min(chains, 4L),
    iter_warmup = 500L,
    iter_sampling = 500L,
    seed = 1234L,
    ...
) {
    stopifnot(inherits(data, "eye_multimodal_measurement"),
              inherits(spec, "eye_multimodal_irt_spec"))
    audit <- audit_multimodal_measurement(data)
    if (!isTRUE(audit$valid))
        stop("Multimodal measurement audit failed: ", paste(audit$issues, collapse=", "), call.=FALSE)

    if (spec$model %in% c("M0","M1")) {
        stop("M0/M1 should use the package's established response/RT estimators during 0.10 development; no duplicate estimator is created here.", call.=FALSE)
    }
    if (spec$backend != "cmdstanr")
        stop("M2/M3 development engines currently require backend='cmdstanr'. No fallback is allowed.", call.=FALSE)
    if (!requireNamespace("cmdstanr", quietly=TRUE))
        stop("cmdstanr is not installed. Install the optional backend; no fallback estimator will be used.", call.=FALSE)
    cmdstan_ok <- tryCatch({
        p <- cmdstanr::cmdstan_path()
        nzchar(p) && dir.exists(p)
    }, error=function(e) FALSE)
    if (!cmdstan_ok) stop("CmdStan is not configured. Run cmdstanr::check_cmdstan_toolchain() and cmdstanr::install_cmdstan().", call.=FALSE)

    d <- data$data
    ch <- data$channels
    req <- if (spec$model=="M2") c("response","rt","gaze") else c("response","rt","gaze","pupil")
    absent <- setdiff(req, names(ch))
    if (length(absent)) stop("Missing required channels: ", paste(absent, collapse=", "), call.=FALSE)

    complete <- stats::complete.cases(d[vapply(req, function(z) ch[[z]], character(1))])
    dd <- d[complete,,drop=FALSE]
    if (!nrow(dd)) stop("No complete rows for requested development model.", call.=FALSE)

    person <- as.integer(factor(dd[[data$keys$person]]))
    item <- as.integer(factor(dd[[data$keys$item]]))
    stan_data <- list(
        N=nrow(dd), P=max(person), I=max(item),
        person=person, item=item,
        y=as.integer(dd[[ch$response]]),
        log_rt=log(as.numeric(dd[[ch$rt]])),
        gaze=as.integer(round(dd[[ch$gaze]]))
    )
    if (spec$model=="M3") {
        stan_data$pupil <- as.numeric(dd[[ch$pupil]])
        q <- data$quality
        getq <- function(pattern) {
            hit <- q[grepl(pattern, q, ignore.case=TRUE)]
            if (length(hit)) as.numeric(dd[[hit[1L]]]) else rep(0, nrow(dd))
        }
        stan_data$luminance <- getq("lumin")
        stan_data$gaze_x <- getq("gaze.*x|x.*gaze")
        stan_data$gaze_y <- getq("gaze.*y|y.*gaze")
    }

    sf <- .multimodal_stan_file(spec$model)
    mod <- cmdstanr::cmdstan_model(sf, quiet=TRUE)
    fit <- mod$sample(
        data=stan_data, chains=as.integer(chains),
        parallel_chains=as.integer(parallel_chains),
        iter_warmup=as.integer(iter_warmup),
        iter_sampling=as.integer(iter_sampling),
        seed=as.integer(seed), refresh=0, ...
    )

    out <- list(
        call=match.call(), specification=spec, measurement=data,
        backend="cmdstanr", backend_version=as.character(utils::packageVersion("cmdstanr")),
        model_file=sf, fit=fit, complete_rows=which(complete),
        validation_status="experimental_unvalidated",
        interpretation=c(
            "This development engine is not promoted for unrestricted confirmatory use.",
            "Gaze and pupil latent dimensions are neutral process dimensions."
        )
    )
    class(out) <- c("eye_multimodal_irt_fit_0_10_dev","list")
    out
}

#' @noRd
print.eye_multimodal_irt_fit_0_10_dev <- function(x, ...) {
    cat("<eye_multimodal_irt_fit_0_10_dev>\n")
    cat(" model      :", x$specification$model, "\n")
    cat(" backend    :", x$backend, "\n")
    cat(" rows fitted:", length(x$complete_rows), "\n")
    cat(" validation :", x$validation_status, "\n")
    invisible(x)
}

#' @noRd
summary.eye_multimodal_irt_fit_0_10_dev <- function(object, ...) {
    if (identical(object$backend, "cmdstanr")) return(object$fit$summary())
    list(validation_status=object$validation_status)
}

#' Posterior predictive checks for multimodal development fits
#'
#' @param object An `eye_multimodal_irt_fit`.
#' @param variables Optional generated-quantity variable names.
#' @return An `eye_multimodal_ppc`.
#' @export
multimodal_ppc <- function(object, variables = NULL) {
    stopifnot(inherits(object, "eye_multimodal_irt_fit_0_10_dev"))
    if (!identical(object$backend,"cmdstanr"))
        stop("PPC currently implemented for bundled CmdStan development engines.", call.=FALSE)
    vars <- variables
    if (is.null(vars)) vars <- c("mean_y_rep","mean_log_rt_rep","mean_gaze_rep",
                                 if (object$specification$model=="M3") "mean_pupil_rep")
    sm <- object$fit$summary(variables=vars)
    out <- list(summary=sm, variables=vars,
                note="Generated-quantity checks are computational diagnostics, not construct validation.")
    class(out) <- c("eye_multimodal_ppc","list")
    out
}
