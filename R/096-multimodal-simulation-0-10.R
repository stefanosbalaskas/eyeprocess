#' Simulate multimodal IRT process data
#'
#' Generates deterministic synthetic person-item observations for staged M0-M3
#' development. Gaze is a non-negative count process and pupil is a neutral
#' pupil-responsivity channel with explicit nuisance effects.
#'
#' @param n_person,n_item Positive integers.
#' @param seed Random seed.
#' @param latent_cor 4x4 correlation matrix for ability, speed, gaze process,
#'   and pupil responsivity.
#' @param rt_sd Residual log-RT SD.
#' @param gaze_size Negative-binomial size.
#' @param pupil_sd Residual pupil-channel SD.
#' @param pupil_luminance,gaze_x_effect,gaze_y_effect Nuisance coefficients.
#' @param missing_fraction Independent channel dropout fraction used for a
#'   baseline stress condition.
#' @return An `eye_multimodal_simulation`.
#' @export
simulate_multimodal_irt <- function(
    n_person = 120L,
    n_item = 20L,
    seed = 42L,
    latent_cor = diag(4L),
    rt_sd = 0.30,
    gaze_size = 8,
    pupil_sd = 0.20,
    pupil_luminance = -0.20,
    gaze_x_effect = 0.08,
    gaze_y_effect = -0.05,
    missing_fraction = 0
) {
    n_person <- as.integer(n_person); n_item <- as.integer(n_item)
    stopifnot(n_person > 1L, n_item > 1L, is.matrix(latent_cor),
              all(dim(latent_cor) == c(4L,4L)),
              isTRUE(all.equal(diag(latent_cor), rep(1,4), tolerance=1e-8)),
              missing_fraction >= 0, missing_fraction < 1)
    ev <- eigen(latent_cor, symmetric = TRUE, only.values = TRUE)$values
    if (min(ev) <= 0) stop("latent_cor must be positive definite.", call. = FALSE)

    .eye_local_seed(seed)

    L <- chol(latent_cor)
    latent <- matrix(rnorm(n_person * 4L), ncol=4L) %*% L
    colnames(latent) <- c("theta","speed","gaze_process","pupil_responsivity")

    item <- data.frame(
        item = sprintf("I%03d", seq_len(n_item)),
        difficulty = rnorm(n_item, 0, .8),
        time_intensity = rnorm(n_item, 2.7, .25),
        gaze_intensity = rnorm(n_item, 1.7, .30),
        pupil_intensity = rnorm(n_item, 0, .18),
        stringsAsFactors = FALSE
    )

    grid <- expand.grid(
        person_id = sprintf("P%03d", seq_len(n_person)),
        item = item$item,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )
    grid$trial <- ave(seq_len(nrow(grid)), grid$person_id, FUN=seq_along)
    p <- match(grid$person_id, sprintf("P%03d", seq_len(n_person)))
    i <- match(grid$item, item$item)

    eta_resp <- latent[p,"theta"] - item$difficulty[i]
    grid$response <- rbinom(nrow(grid), 1L, plogis(eta_resp))

    log_rt_mu <- item$time_intensity[i] - latent[p,"speed"]
    grid$rt <- exp(rnorm(nrow(grid), log_rt_mu, rt_sd))

    gaze_mu <- exp(item$gaze_intensity[i] + latent[p,"gaze_process"])
    grid$gaze_fixation_count <- rnbinom(nrow(grid), mu=gaze_mu, size=gaze_size)

    grid$luminance_z <- rnorm(nrow(grid))
    grid$gaze_x_z <- rnorm(nrow(grid))
    grid$gaze_y_z <- rnorm(nrow(grid))
    pupil_mu <- item$pupil_intensity[i] + latent[p,"pupil_responsivity"] +
        pupil_luminance * grid$luminance_z +
        gaze_x_effect * grid$gaze_x_z +
        gaze_y_effect * grid$gaze_y_z
    grid$pupil_response <- rnorm(nrow(grid), pupil_mu, pupil_sd)

    if (missing_fraction > 0) {
        for (nm in c("rt","gaze_fixation_count","pupil_response")) {
            miss <- runif(nrow(grid)) < missing_fraction
            grid[[nm]][miss] <- NA
        }
    }

    truth <- list(
        persons = data.frame(
            person_id=sprintf("P%03d", seq_len(n_person)),
            latent,
            stringsAsFactors=FALSE
        ),
        items = item,
        parameters = list(
            rt_sd=rt_sd, gaze_size=gaze_size, pupil_sd=pupil_sd,
            pupil_luminance=pupil_luminance,
            gaze_x_effect=gaze_x_effect, gaze_y_effect=gaze_y_effect,
            latent_cor=latent_cor
        ),
        seed = seed
    )

    measurement <- prepare_multimodal_irt_data(
        grid, person="person_id", item="item", trial="trial",
        response="response", rt="rt", gaze="gaze_fixation_count",
        pupil="pupil_response",
        quality=c("luminance_z","gaze_x_z","gaze_y_z"),
        provenance=list(simulator="simulate_multimodal_irt", seed=seed)
    )

    out <- list(data=grid, measurement=measurement, truth=truth)
    class(out) <- c("eye_multimodal_simulation","list")
    out
}

#' @export
print.eye_multimodal_simulation <- function(x, ...) {
    cat("<eye_multimodal_simulation>\n")
    cat(" observations :", nrow(x$data), "\n")
    cat(" persons      :", nrow(x$truth$persons), "\n")
    cat(" items        :", nrow(x$truth$items), "\n")
    cat(" seed         :", x$truth$seed, "\n")
    invisible(x)
}
