mi_trial_data <- function(n_person = 20L, n_item = 6L) {
  set.seed(20260807)
  data <- expand.grid(person_id = paste0("P", seq_len(n_person)), item_id = paste0("I", seq_len(n_item)), KEEP.OUT.ATTRS = FALSE)
  data$group <- rep(c("A", "B"), length.out = nrow(data))
  data$device <- rep(c("reference", "candidate"), each = nrow(data) / 2, length.out = nrow(data))
  data$session <- rep(1:2, length.out = nrow(data))
  data$time <- ave(seq_len(nrow(data)), data$person_id, FUN = seq_along)
  ability <- rep(stats::rnorm(n_person), each = n_item)
  difficulty <- rep(seq(-1, 1, length.out = n_item), times = n_person)
  data$response <- stats::rbinom(nrow(data), 1, stats::plogis(ability - difficulty + 0.15 * (data$group == "B")))
  data$dwell_ms <- stats::rlnorm(nrow(data), log(800 + 150 * difficulty^2 + 100 * (1 - data$response)), 0.15)
  data$pupil <- 0.1 + 0.04 * difficulty + stats::rnorm(nrow(data), 0, 0.02)
  data$entropy <- pmin(1, pmax(0, 0.45 + 0.1 * difficulty + stats::rnorm(nrow(data), 0, 0.05)))
  data$observed <- stats::rbinom(nrow(data), 1, stats::plogis(2 - 0.4 * difficulty - 0.2 * data$pupil))
  data
}

mi_gaze_data <- function(n = 120L) {
  set.seed(20260807)
  data.frame(
    sample_id = seq_len(n),
    time = seq(0, 12, length.out = n),
    x = pmin(1, pmax(0, cumsum(stats::rnorm(n, 0, 0.03)) + 0.5)),
    y = pmin(1, pmax(0, cumsum(stats::rnorm(n, 0, 0.03)) + 0.5)),
    duration = stats::runif(n, 8, 20),
    pupil = 3 + 0.2 * sin(seq(0, 4 * pi, length.out = n)) + stats::rnorm(n, 0, 0.05)
  )
}

mi_aois <- function() data.frame(
  aoi_id = c("left", "right", "bottom"),
  xmin = c(0.05, 0.55, 0.25), xmax = c(0.45, 0.95, 0.75),
  ymin = c(0.05, 0.05, 0.60), ymax = c(0.45, 0.45, 0.95)
)

expect_plot_silent <- function(code) {
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({ grDevices::dev.off(); unlink(file) }, add = TRUE)
  expect_silent(force(code))
}
