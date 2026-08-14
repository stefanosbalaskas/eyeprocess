.multimodal_require_ggplot2 <- function() {
    if (!requireNamespace("ggplot2", quietly=TRUE))
        stop("Package 'ggplot2' is required for multimodal plots.", call.=FALSE)
}

#' @export
plot.eye_multimodal_measurement <- function(
    x,
    type = c("availability","missingness","quality"),
    ...
) {
    .multimodal_require_ggplot2()
    type <- match.arg(type)
    a <- audit_multimodal_measurement(x)
    d <- a$channel_table
    if (!nrow(d)) stop("No channels to plot.", call.=FALSE)

    if (type=="availability") {
        d$value <- d$observed
        p <- ggplot2::ggplot(d, ggplot2::aes(x=d[["channel"]], y=d[["value"]])) +
            ggplot2::geom_col() +
            ggplot2::labs(x=NULL, y="Observed rows", title="Multimodal channel availability") +
            ggplot2::theme_minimal()
    } else if (type=="missingness") {
        d$value <- d$missing_fraction
        p <- ggplot2::ggplot(d, ggplot2::aes(x=d[["channel"]], y=d[["value"]])) +
            ggplot2::geom_col() +
            ggplot2::labs(x=NULL, y="Missing fraction", title="Multimodal channel missingness") +
            ggplot2::theme_minimal()
    } else {
        q <- x$quality
        if (!length(q)) stop("No quality fields registered.", call.=FALSE)
        vals <- vapply(q, function(nm) mean(!is.na(x$data[[nm]])), numeric(1))
        qd <- data.frame(field=q, observed_fraction=vals)
        p <- ggplot2::ggplot(qd, ggplot2::aes(x=qd[["field"]], y=qd[["observed_fraction"]])) +
            ggplot2::geom_col() +
            ggplot2::labs(x=NULL, y="Observed fraction", title="Registered quality fields") +
            ggplot2::theme_minimal()
    }
    p
}

#' @export
plot.eye_multimodal_simulation <- function(
    x,
    type = c("latent_correlation","item_profile","channel_distributions"),
    ...
) {
    .multimodal_require_ggplot2()
    type <- match.arg(type)
    if (type=="latent_correlation") {
        z <- x$truth$persons
        vars <- c("theta","speed","gaze_process","pupil_responsivity")
        C <- stats::cor(z[vars])
        dd <- as.data.frame(as.table(C), stringsAsFactors=FALSE)
        names(dd) <- c("dimension_1","dimension_2","correlation")
        ggplot2::ggplot(dd, ggplot2::aes(x=dd[["dimension_1"]], y=dd[["dimension_2"]], fill=dd[["correlation"]])) +
            ggplot2::geom_tile() +
            ggplot2::geom_text(ggplot2::aes(label=sprintf("%.2f", dd[["correlation"]])), size=3) +
            ggplot2::labs(x=NULL,y=NULL,title="Simulated latent correlation") +
            ggplot2::theme_minimal()
    } else if (type=="item_profile") {
        d <- x$truth$items
        long <- rbind(
            data.frame(item=d$item, parameter="difficulty", value=d$difficulty),
            data.frame(item=d$item, parameter="time intensity", value=d$time_intensity),
            data.frame(item=d$item, parameter="gaze intensity", value=d$gaze_intensity),
            data.frame(item=d$item, parameter="pupil intensity", value=d$pupil_intensity)
        )
        ggplot2::ggplot(long, ggplot2::aes(x=long[["item"]], y=long[["value"]], group=long[["parameter"]])) +
            ggplot2::geom_line() + ggplot2::facet_wrap(~parameter, scales="free_y") +
            ggplot2::labs(x="Item",y=NULL,title="Simulated item multimodal profile") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, vjust=.5))
    } else {
        d <- x$data
        long <- rbind(
            data.frame(channel="log RT", value=log(d$rt)),
            data.frame(channel="log(1+gaze count)", value=log1p(d$gaze_fixation_count)),
            data.frame(channel="pupil response", value=d$pupil_response)
        )
        ggplot2::ggplot(long, ggplot2::aes(x=long[["value"]])) +
            ggplot2::geom_histogram(bins=30) +
            ggplot2::facet_wrap(~channel, scales="free") +
            ggplot2::labs(x=NULL,y="Count",title="Simulated process-channel distributions") +
            ggplot2::theme_minimal()
    }
}

#' @export
plot.eye_process_information <- function(
    x,
    type = c("gain","relative_variance"),
    ...
) {
    .multimodal_require_ggplot2()
    type <- match.arg(type)
    y <- if (type=="gain") x$value else x$relative_variance_reduction
    d <- data.frame(target=x$target, value=y)
    ggplot2::ggplot(d, ggplot2::aes(x=d[["target"]], y=d[["value"]])) +
        ggplot2::geom_col() +
        ggplot2::labs(
            x=NULL,
            y=if (type=="gain") unique(x$metric) else "Relative posterior variance reduction",
            title="Incremental process information"
        ) +
        ggplot2::theme_minimal()
}

#' @export
plot.eye_multimodal_validation <- function(
    x,
    type = c("checks"),
    ...
) {
    .multimodal_require_ggplot2()
    type <- match.arg(type)
    d <- x$checks
    d$value <- as.integer(d$pass)
    ggplot2::ggplot(d, ggplot2::aes(x=d[["check"]], y=d[["value"]])) +
        ggplot2::geom_col() +
        ggplot2::scale_y_continuous(breaks=c(0,1), labels=c("fail","pass")) +
        ggplot2::labs(x=NULL,y=NULL,title="Multimodal validation checks") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1))
}
