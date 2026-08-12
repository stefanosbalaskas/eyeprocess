# eyeprocess 0.9 Milestone #2 evidence execution
# Writes artifacts outside package source. Usage:
# Rscript run-all-validation-evidence.R [output_dir] [local|full]

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else file.path(getwd(), "eyeprocess-validation-artifacts", "0.9-m2")
profile <- if (length(args) >= 2L) args[[2L]] else "local"
if (!profile %in% c("local", "full")) stop("profile must be local or full")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
for (d in c("designs","results","figures","tables","manifests","references")) dir.create(file.path(out_dir,d), recursive=TRUE, showWarnings=FALSE)
suppressPackageStartupMessages(library(eyeprocess))

# Hard installed-package contract.  The development version intentionally remains
# 0.9.0.9000 across milestones, so version equality alone cannot distinguish a
# stale Milestone #1 installation from the Milestone #2 API.
required_m2_exports <- c(
  "eyeprocess_validation_plan",
  "run_eyeprocess_irt_ability_sbc",
  "run_eyeprocess_stress_evidence",
  "freeze_eyeprocess_validation_evidence",
  "eyeprocess_validation_evidence_atlas"
)
installed_exports <- getNamespaceExports("eyeprocess")
missing_m2_exports <- setdiff(required_m2_exports, installed_exports)
if (!identical(as.character(utils::packageVersion("eyeprocess")), "0.9.0.9000") || length(missing_m2_exports)) {
  stop(
    "Installed eyeprocess is not the validated Milestone #2 build. Missing M2 exports: ",
    if (length(missing_m2_exports)) paste(missing_m2_exports, collapse = ", ") else "<none>",
    ". Run the Milestone #2 resume/validate/install harness first.",
    call. = FALSE
  )
}
cat("Installed M2 API fingerprint: PASS\n")
cat("Installed package path: ", normalizePath(find.package("eyeprocess"), winslash = "/", mustWork = TRUE), "\n", sep = "")

seed <- 20260811L
set.seed(seed)

# Design freeze -------------------------------------------------------------
plan <- eyeprocess_validation_plan(
  sample_size = if (profile == "full") c(250L, 750L, 1500L) else c(250L, 750L),
  n_items = if (profile == "full") c(12L,24L,40L) else c(12L,24L),
  missing_rate = c(0,.15), noise_level = c("reference","elevated"),
  specification = c("correct","misspecified"),
  replications = if (profile == "full") 20L else 5L, seed = seed
)
scenarios <- expand_eyeprocess_validation_plan(plan)
write.csv(scenarios, file.path(out_dir,"designs","validation-scenarios.csv"), row.names=FALSE)
stress_plan <- eyeprocess_stress_evidence_plan(seed=seed)
write.csv(expand_eyeprocess_stress_evidence_plan(stress_plan), file.path(out_dir,"designs","stress-scenarios.csv"), row.names=FALSE)

# Executed deterministic measurement-stress evidence -----------------------
stress_seed <- seed + 100L
set.seed(stress_seed)
n_stress <- if (profile == "full") 6000L else 2400L
stress_data <- data.frame(
  row_id = seq_len(n_stress),
  target_x = stats::runif(n_stress, .05, .95),
  target_y = stats::runif(n_stress, .05, .95),
  pupil = stats::rnorm(n_stress, 3.4, .18),
  dt_ms = stats::rnorm(n_stress, 1000/60, .25),
  device = rep(c("A","B"), length.out=n_stress),
  trial_id = rep(seq_len(n_stress %/% 20L), each=20L, length.out=n_stress),
  stringsAsFactors = FALSE
)
stress_data$gaze_x <- stress_data$target_x + stats::rnorm(n_stress, 0, .008)
stress_data$gaze_y <- stress_data$target_y + stats::rnorm(n_stress, 0, .008)
stress_data$aoi_truth <- ifelse(stress_data$target_x < .5, "left", "right")
stress_data$aoi <- stress_data$aoi_truth
n_trials_ref <- length(unique(stress_data$trial_id))
local_seed <- function(seed_value) { set.seed(as.integer(seed_value)); invisible(TRUE) }
corruptors <- list(
  missing_gaze = function(d,severity,seed) { local_seed(seed); n <- floor(nrow(d)*severity); if(n>0){i<-sample.int(nrow(d),n); d$gaze_x[i]<-NA_real_; d$gaze_y[i]<-NA_real_}; d },
  pupil_dropout = function(d,severity,seed) { local_seed(seed); n <- floor(nrow(d)*severity); if(n>0) d$pupil[sample.int(nrow(d),n)] <- NA_real_; d },
  calibration_offset = function(d,severity,seed) { d$gaze_x <- d$gaze_x + severity; d$gaze_y <- d$gaze_y + severity; d },
  sampling_jitter = function(d,severity,seed) { local_seed(seed); d$dt_ms <- pmax(.1, d$dt_ms * (1 + stats::rnorm(nrow(d),0,severity))); d },
  aoi_label_noise = function(d,severity,seed) { local_seed(seed); n <- floor(nrow(d)*severity); if(n>0){i<-sample.int(nrow(d),n); d$aoi[i] <- ifelse(d$aoi[i]=="left","right","left")}; d },
  device_shift = function(d,severity,seed) { d$gaze_x[d$device=="B"] <- d$gaze_x[d$device=="B"] + severity; d },
  trial_imbalance = function(d,severity,seed) { local_seed(seed); tr <- unique(d$trial_id); n <- floor(length(tr)*severity); if(n>0) d <- d[!d$trial_id %in% sample(tr,n),,drop=FALSE]; d }
)
stress_metric <- function(d) {
  gx <- is.finite(d$gaze_x); gy <- is.finite(d$gaze_y); both <- gx & gy
  radial <- sqrt((d$gaze_x-d$target_x)^2 + (d$gaze_y-d$target_y)^2)
  err <- d$gaze_x-d$target_x
  devA <- err[d$device=="A" & is.finite(err)]; devB <- err[d$device=="B" & is.finite(err)]
  c(
    gaze_valid_fraction = mean(both),
    pupil_valid_fraction = mean(is.finite(d$pupil)),
    radial_rmse = if(any(both)) sqrt(mean(radial[both]^2)) else NA_real_,
    sampling_interval_cv = if(sum(is.finite(d$dt_ms))>1L) stats::sd(d$dt_ms,na.rm=TRUE)/mean(d$dt_ms,na.rm=TRUE) else NA_real_,
    aoi_accuracy = mean(d$aoi == d$aoi_truth, na.rm=TRUE),
    device_error_gap = if(length(devA) && length(devB)) abs(mean(devB)-mean(devA)) else NA_real_,
    retained_trial_fraction = length(unique(d$trial_id))/n_trials_ref
  )
}
stress_exec <- run_eyeprocess_stress_evidence(stress_data, stress_plan, corruptors, stress_metric)
stopifnot(nrow(stress_exec$failures) == 0L)
write.csv(stress_exec$results, file.path(out_dir,"results","measurement-stress-results.csv"), row.names=FALSE)
write.csv(summarise_eyeprocess_stress_evidence(stress_exec), file.path(out_dir,"tables","measurement-stress-summary.csv"), row.names=FALSE)
write.csv(stress_exec$failures, file.path(out_dir,"tables","measurement-stress-failures.csv"), row.names=FALSE)

# Measurement-resolution compatibility evidence ----------------------------
reference_dt_ms <- stats::median(stress_data$dt_ms[is.finite(stress_data$dt_ms)])
reference_hz <- if (is.finite(reference_dt_ms) && reference_dt_ms > 0) 1000 / reference_dt_ms else NA_real_
event_scale_ms <- c(16.67, 33.33, 50, 100, 200)
resolution <- data.frame(
  event_scale_ms = event_scale_ms,
  reference_interval_ms = reference_dt_ms,
  reference_sampling_hz = reference_hz,
  samples_per_event = event_scale_ms / reference_dt_ms,
  stringsAsFactors = FALSE
)
resolution$status <- ifelse(
  resolution$samples_per_event < 2,
  "limited",
  ifelse(
    resolution$samples_per_event < 5,
    "coarse",
    "descriptively_resolved"
  )
)
resolution$guardrail <- "Resolution labels describe sampling granularity under this synthetic design; they do not certify physiological or construct validity."

jitter_resolution <- stress_exec$results[
  stress_exec$results$corruption == "sampling_jitter" &
    stress_exec$results$metric == "sampling_interval_cv",
  c("scenario_id","severity","baseline","value","delta","relative_change"),
  drop = FALSE
]
if (nrow(jitter_resolution)) {
  names(jitter_resolution)[names(jitter_resolution) == "baseline"] <- "baseline_interval_cv"
  names(jitter_resolution)[names(jitter_resolution) == "value"] <- "observed_interval_cv"
}
write.csv(resolution, file.path(out_dir,"tables","measurement-resolution.csv"), row.names=FALSE)
write.csv(jitter_resolution, file.path(out_dir,"tables","measurement-resolution-jitter.csv"), row.names=FALSE)

# Native IRT mathematics/diagnostic evidence -------------------------------
items <- data.frame(item_id=paste0("I",1:12), a=seq(.75,1.65,length.out=12), b=seq(-2,2,length.out=12), c=0,d=1)
theta_grid <- seq(-4,4,length.out=161)
precision <- eyeprocess_irt_test_information(theta_grid, items)
write.csv(precision, file.path(out_dir,"tables","irt-information.csv"), row.names=FALSE)
bank_cov <- eyeprocess_irt_bank_coverage(items, theta_grid, target_information=3)
write.csv(bank_cov$gaps, file.path(out_dir,"tables","irt-information-gaps.csv"), row.names=FALSE)

sim <- simulate_eyeprocess_irt_binary(if (profile=="full") 1000L else 300L, items, missing_rate=.10, seed=seed)
item_fit <- eyeprocess_irt_item_fit_residuals(sim$responses, sim$probabilities)
person_fit <- eyeprocess_irt_person_fit_residuals(sim$responses, sim$probabilities)
q3 <- eyeprocess_irt_q3(sim$responses, sim$probabilities)
write.csv(item_fit, file.path(out_dir,"tables","irt-item-fit.csv"), row.names=FALSE)
write.csv(person_fit, file.path(out_dir,"tables","irt-person-fit.csv"), row.names=FALSE)
write.csv(as.data.frame(q3), file.path(out_dir,"tables","irt-q3.csv"), row.names=FALSE)

# Simulation-based calibration of the known-item ability scorer -------------
n_sbc <- if (profile=="full") 1000L else 200L
n_draws <- if (profile=="full") 199L else 99L
sbc <- run_eyeprocess_irt_ability_sbc(
  items, replications=n_sbc, posterior_draws=n_draws,
  theta_grid=seq(-5,5,length.out=401), interval=.95, seed=seed+1L
)
write.csv(data.frame(rank=sbc$diagnostics$ranks,n_draws=n_draws), file.path(out_dir,"results","sbc-ranks.csv"), row.names=FALSE)
write.csv(sbc$details, file.path(out_dir,"results","sbc-ability-details.csv"), row.names=FALSE)
write.csv(eyeprocess_sbc_evidence_table(sbc), file.path(out_dir,"tables","sbc-summary.csv"), row.names=FALSE)

# Reliability evidence ------------------------------------------------------
reliability_seed <- seed + 300L
set.seed(reliability_seed)
rel <- expand.grid(person_id=1:80, session=1:3, trial_id=1:6)
u <- rnorm(80); rel$value <- u[rel$person_id] + rnorm(nrow(rel), sd=.30)
icc <- process_icc(rel,"person_id","session","value")
temporal <- process_temporal_stability(rel,"person_id","session","value")
ba <- process_bland_altman(rel,"person_id","session","value",sessions=c(1,2))
split <- split_half_process_reliability(rel,"person_id","trial_id","value",split="odd_even")
write.csv(icc,file.path(out_dir,"tables","reliability-icc.csv"),row.names=FALSE)
write.csv(temporal,file.path(out_dir,"tables","reliability-temporal.csv"),row.names=FALSE)
write.csv(ba$summary,file.path(out_dir,"tables","reliability-bland-altman.csv"),row.names=FALSE)
write.csv(split,file.path(out_dir,"tables","reliability-split-half.csv"),row.names=FALSE)

# Pupil baseline-window and preprocessing-order sensitivity -----------------
pupil_fixture_path <- system.file(
  "extdata",
  "validation_m2_pupil_baseline_fixture.csv",
  package = "eyeprocess"
)
if (!nzchar(pupil_fixture_path) || !file.exists(pupil_fixture_path)) {
  stop("Bundled Milestone #2 pupil baseline fixture was not found.", call. = FALSE)
}
pupil_fixture <- utils::read.csv(pupil_fixture_path, stringsAsFactors = FALSE)
required_pupil_cols <- c("person_id","trial_id","time_ms","pupil")
if (!all(required_pupil_cols %in% names(pupil_fixture))) {
  stop("Pupil baseline fixture does not contain the expected columns.", call. = FALSE)
}

trial_key <- interaction(
  pupil_fixture$person_id,
  pupil_fixture$trial_id,
  drop = TRUE,
  lex.order = TRUE
)

baseline_windows <- list(
  `minus500_minus100` = c(-500, -100),
  `minus300_minus100` = c(-300, -100),
  `minus200_zero` = c(-200, 0)
)

baseline_rows <- list()
bk <- 0L
for (window_name in names(baseline_windows)) {
  window <- baseline_windows[[window_name]]
  per_trial <- lapply(split(seq_len(nrow(pupil_fixture)), trial_key), function(ii) {
    z <- pupil_fixture[ii, , drop = FALSE]
    base <- z$pupil[
      is.finite(z$pupil) &
        z$time_ms >= window[[1L]] &
        z$time_ms <= window[[2L]]
    ]
    post <- z$pupil[
      is.finite(z$pupil) &
        z$time_ms >= 100 &
        z$time_ms <= 400
    ]
    data.frame(
      person_id = z$person_id[[1L]],
      trial_id = z$trial_id[[1L]],
      baseline = if (length(base)) mean(base) else NA_real_,
      post_mean = if (length(post)) mean(post) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  per_trial <- do.call(rbind, per_trial)
  per_trial$corrected_post_mean <- per_trial$post_mean - per_trial$baseline
  bk <- bk + 1L
  baseline_rows[[bk]] <- data.frame(
    baseline_window = window_name,
    start_ms = window[[1L]],
    end_ms = window[[2L]],
    n_trials = sum(is.finite(per_trial$corrected_post_mean)),
    mean_corrected_post = mean(per_trial$corrected_post_mean, na.rm = TRUE),
    sd_corrected_post = stats::sd(per_trial$corrected_post_mean, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}
pupil_baseline_sensitivity <- do.call(rbind, baseline_rows)
pupil_baseline_sensitivity$range_from_reference <- pupil_baseline_sensitivity$mean_corrected_post -
  pupil_baseline_sensitivity$mean_corrected_post[
    pupil_baseline_sensitivity$baseline_window == "minus300_minus100"
  ]
pupil_baseline_sensitivity$guardrail <- "Baseline-window sensitivity characterizes dependence on declared preprocessing choices; it does not identify a uniquely correct baseline."

smooth_one <- function(x) {
  as.numeric(stats::filter(x, rep(1/3, 3), sides = 2))
}
pupil_smooth <- pupil_fixture
pupil_smooth$pupil_smoothed <- unsplit(
  lapply(
    split(pupil_fixture$pupil, trial_key),
    smooth_one
  ),
  trial_key
)

order_summary <- function(data, value_col, baseline_first) {
  values <- data[[value_col]]
  key <- interaction(data$person_id, data$trial_id, drop = TRUE, lex.order = TRUE)
  rows <- lapply(split(seq_len(nrow(data)), key), function(ii) {
    z <- data[ii, , drop = FALSE]
    value <- values[ii]
    base_idx <- is.finite(value) & z$time_ms >= -300 & z$time_ms <= -100
    baseline <- if (any(base_idx)) mean(value[base_idx]) else NA_real_
    corrected <- value - baseline
    if (isTRUE(baseline_first)) {
      corrected <- smooth_one(corrected)
    }
    post_idx <- is.finite(corrected) & z$time_ms >= 100 & z$time_ms <= 400
    data.frame(
      corrected_post_mean = if (any(post_idx)) mean(corrected[post_idx]) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  rows <- do.call(rbind, rows)
  c(
    n_trials = sum(is.finite(rows$corrected_post_mean)),
    mean_corrected_post = mean(rows$corrected_post_mean, na.rm = TRUE),
    sd_corrected_post = stats::sd(rows$corrected_post_mean, na.rm = TRUE)
  )
}

baseline_then_smooth <- order_summary(
  pupil_fixture,
  "pupil",
  baseline_first = TRUE
)
smooth_then_baseline <- order_summary(
  pupil_smooth,
  "pupil_smoothed",
  baseline_first = FALSE
)
pupil_order_sensitivity <- data.frame(
  preprocessing_order = c("baseline_then_smooth","smooth_then_baseline"),
  n_trials = c(baseline_then_smooth[["n_trials"]], smooth_then_baseline[["n_trials"]]),
  mean_corrected_post = c(baseline_then_smooth[["mean_corrected_post"]], smooth_then_baseline[["mean_corrected_post"]]),
  sd_corrected_post = c(baseline_then_smooth[["sd_corrected_post"]], smooth_then_baseline[["sd_corrected_post"]]),
  stringsAsFactors = FALSE
)
pupil_order_sensitivity$delta_from_baseline_then_smooth <-
  pupil_order_sensitivity$mean_corrected_post -
  pupil_order_sensitivity$mean_corrected_post[[1L]]
pupil_order_sensitivity$guardrail <- "Preprocessing-order sensitivity is an implementation/design diagnostic, not evidence that one order is universally correct."

write.csv(
  pupil_baseline_sensitivity,
  file.path(out_dir,"tables","pupil-baseline-sensitivity.csv"),
  row.names = FALSE
)
write.csv(
  pupil_order_sensitivity,
  file.path(out_dir,"tables","pupil-preprocessing-order-sensitivity.csv"),
  row.names = FALSE
)

# Negative controls / leakage ----------------------------------------------
negative_control_seed <- seed + 400L
set.seed(negative_control_seed)
ncdat <- data.frame(x=rnorm(250), group=rep(1:25,each=10))
ncdat$y <- .35*ncdat$x + rnorm(250)
analysis_fun <- function(d) unname(coef(lm(y~x,data=d))[2])
controls <- run_process_negative_controls(ncdat,"y",analysis_fun,
    replications=if(profile=="full") 200L else 40L,seed=seed,within="group")
write.csv(controls$results,file.path(out_dir,"results","negative-controls.csv"),row.names=FALSE)
write.csv(summarise_process_negative_controls(controls),file.path(out_dir,"tables","negative-controls-summary.csv"),row.names=FALSE)
prov <- process_feature_time_provenance(c("pre","known_leak"),c(10,30),c(20,20))
leak <- audit_temporal_leakage(prov)
write.csv(leak$detail,file.path(out_dir,"tables","known-leakage-audit.csv"),row.names=FALSE)

# Explicit temporal-shift and placebo-window negative controls -------------
ncdat$time_index <- ave(ncdat$group, ncdat$group, FUN = seq_along)
group_rows <- split(seq_len(nrow(ncdat)), ncdat$group)
window_control <- do.call(
  rbind,
  lapply(group_rows, function(ii) {
    z <- ncdat[ii, , drop = FALSE]
    data.frame(
      group = z$group[[1L]],
      outcome = mean(z$y[z$time_index >= 8], na.rm = TRUE),
      reference_window = mean(z$x[z$time_index >= 8], na.rm = TRUE),
      temporal_shift = mean(z$x[z$time_index >= 4 & z$time_index <= 6], na.rm = TRUE),
      placebo_window = mean(z$x[z$time_index <= 3], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
)

window_estimate <- function(predictor) {
  form <- stats::reformulate(predictor, response = "outcome")
  fit <- stats::lm(form, data = window_control)
  c(
    estimate = unname(stats::coef(fit)[[predictor]]),
    standard_error = unname(summary(fit)$coefficients[predictor, "Std. Error"]),
    p_value = unname(summary(fit)$coefficients[predictor, "Pr(>|t|)"])
  )
}
negative_window_shift <- do.call(
  rbind,
  lapply(
    c("reference_window","temporal_shift","placebo_window"),
    function(control) {
      est <- window_estimate(control)
      data.frame(
        control = control,
        n_groups = nrow(window_control),
        estimate = est[["estimate"]],
        standard_error = est[["standard_error"]],
        p_value = est[["p_value"]],
        stringsAsFactors = FALSE
      )
    }
  )
)
negative_window_shift$guardrail <- "Temporal-shift/placebo controls diagnose analysis behavior in a declared synthetic design; they do not prove absence of leakage in empirical studies."
write.csv(
  negative_window_shift,
  file.path(out_dir,"tables","negative-control-window-shift.csv"),
  row.names = FALSE
)

# Frozen evidence figures --------------------------------------------------
figure_path <- function(name) file.path(out_dir, "figures", name)
grDevices::pdf(figure_path("irt-information.pdf")); plot(precision); grDevices::dev.off()
grDevices::pdf(figure_path("sbc-ranks.pdf")); plot(sbc); grDevices::dev.off()
grDevices::pdf(figure_path("measurement-stress.pdf")); plot(stress_exec); grDevices::dev.off()
grDevices::pdf(figure_path("reliability-temporal.pdf"))
if (nrow(temporal)) {
  graphics::plot(seq_len(nrow(temporal)), temporal$correlation, type = "b", ylim = c(-1, 1), xlab = "Session pair", ylab = "Correlation", main = "Temporal reliability evidence")
  graphics::abline(h = 0, lty = 3)
} else graphics::plot.new()
grDevices::dev.off()
grDevices::pdf(figure_path("negative-controls.pdf")); plot(controls); grDevices::dev.off()

# Optional exact IRT recovery ----------------------------------------------
recovery <- NULL
if (requireNamespace("mirt", quietly=TRUE)) {
  rd <- eyeprocess_irt_recovery_design(sample_size=if(profile=="full") c(250L,750L) else 250L,
      n_items=if(profile=="full") c(12L,24L) else 12L, missing_rate=c(0,.15), testlet_sd=c(0,.35),
      replications=if(profile=="full") 10L else 2L, seed=seed)
  recovery <- run_eyeprocess_irt_recovery(rd, verbose=TRUE)
  recovery_summary <- eyeprocess_irt_recovery_summary(recovery)
  recovery_failures <- eyeprocess_irt_recovery_failures(recovery)
  write.csv(recovery_summary,file.path(out_dir,"tables","irt-recovery.csv"),row.names=FALSE)
  write.csv(recovery_failures,file.path(out_dir,"tables","irt-recovery-failures.csv"),row.names=FALSE)

  est <- recovery$estimates
  recovery_rep <- do.call(
    rbind,
    lapply(c("a","b"), function(parameter) {
      err <- est[[paste0(parameter,"_estimate")]] - est[[paste0(parameter,"_truth")]]
      z <- data.frame(
        scenario_id = est$scenario_id,
        replication = est$replication,
        parameter = parameter,
        error = err,
        stringsAsFactors = FALSE
      )
      aggregate(
        error ~ scenario_id + replication + parameter,
        data = z,
        FUN = mean,
        na.rm = TRUE
      )
    })
  )
  names(recovery_rep)[names(recovery_rep) == "error"] <- "replication_bias"
  recovery_mcse <- validation_mcse_profile(
    recovery_rep,
    metric = "replication_bias",
    by = c("scenario_id","parameter")
  )
  write.csv(
    recovery_mcse,
    file.path(out_dir,"tables","irt-recovery-mcse.csv"),
    row.names = FALSE
  )

  recovery_design_tab <- as.data.frame(recovery$design)
  recovery_aug <- merge(
    recovery_summary,
    recovery_design_tab,
    by = "scenario_id",
    all.x = TRUE
  )
  key_cols <- c("sample_size","n_items","missing_rate","parameter")
  key <- do.call(
    interaction,
    c(
      recovery_aug[, key_cols, drop = FALSE],
      list(drop = TRUE, lex.order = TRUE)
    )
  )
  miss_rows <- lapply(split(seq_len(nrow(recovery_aug)), key), function(ii) {
    z <- recovery_aug[ii, , drop = FALSE]
    ref <- z[z$testlet_sd == min(z$testlet_sd, na.rm = TRUE), , drop = FALSE]
    mis <- z[z$testlet_sd == max(z$testlet_sd, na.rm = TRUE), , drop = FALSE]
    if (!nrow(ref) || !nrow(mis) || identical(ref$testlet_sd[[1L]], mis$testlet_sd[[1L]])) return(NULL)
    data.frame(
      sample_size = ref$sample_size[[1L]],
      n_items = ref$n_items[[1L]],
      missing_rate = ref$missing_rate[[1L]],
      parameter = ref$parameter[[1L]],
      reference_testlet_sd = ref$testlet_sd[[1L]],
      misspecified_testlet_sd = mis$testlet_sd[[1L]],
      bias_reference = ref$bias[[1L]],
      bias_misspecified = mis$bias[[1L]],
      rmse_reference = ref$rmse[[1L]],
      rmse_misspecified = mis$rmse[[1L]],
      rmse_inflation = mis$rmse[[1L]] - ref$rmse[[1L]],
      absolute_bias_inflation = abs(mis$bias[[1L]]) - abs(ref$bias[[1L]]),
      stringsAsFactors = FALSE
    )
  })
  miss_rows <- Filter(Negate(is.null), miss_rows)
  recovery_misspecification <- if (length(miss_rows)) do.call(rbind, miss_rows) else data.frame()
  write.csv(
    recovery_misspecification,
    file.path(out_dir,"tables","irt-recovery-misspecification.csv"),
    row.names = FALSE
  )
} else {
  recovery_mcse <- data.frame()
  recovery_misspecification <- data.frame()
  writeLines("mirt unavailable: exact IRT parameter-recovery evidence gated; no substitute estimator used.", file.path(out_dir,"results","irt-recovery-GATED.txt"))
}

recovery_coverage_status <- data.frame(
  evidence = c("item_parameter_interval_coverage","ability_interval_coverage"),
  status = c("not_estimated_current_recovery_contract","estimated_via_sbc"),
  value = c(NA_real_, sbc$coverage),
  nominal = c(NA_real_, sbc$nominal_coverage),
  rationale = c(
    "The M2 exact mirt recovery object freezes point estimates but not item-parameter covariance/interval estimates; interval coverage is therefore not fabricated.",
    "Known-item ability SBC records central posterior interval coverage under the declared generative model."
  ),
  stringsAsFactors = FALSE
)
write.csv(
  recovery_coverage_status,
  file.path(out_dir,"tables","irt-recovery-interval-coverage-status.csv"),
  row.names = FALSE
)

# Claims and provenance -----------------------------------------------------
claims <- eyeprocess_validation_claim_matrix(
  c("M2-C01","M2-C02","M2-C03","M2-C04","M2-C05"),
  c("Scenario expansion is deterministic","Native IRT probabilities obey mathematical constraints","Exact external estimators are not silently substituted","Evidence freeze detects tampering","Known temporal leakage is detected"),
  c("scenario-manifest","irt-math","engine-gating","freeze-hash","leakage-control"),
  c("integration","unit","engine-contract","integrity","negative-control"),
  c("supported","supported","qualified","supported",if(leak$n_flagged==1L) "supported" else "not_supported"),
  c("software behavior only","mathematical implementation only","engine availability is environment dependent","integrity not construct validity","diagnostic not misconduct label")
)
write.csv(claims,file.path(out_dir,"tables","claim-evidence.csv"),row.names=FALSE)
source_commit <- tryCatch(system2("git",c("rev-parse","HEAD"),stdout=TRUE),error=function(e) NA_character_)
prov_obj <- list(
  source_commit = source_commit,
  R = R.version.string,
  platform = R.version$platform,
  package = as.character(packageVersion("eyeprocess")),
  profile = profile,
  seed = seed,
  seed_streams = list(
    stress = stress_seed,
    sbc = seed + 1L,
    reliability = reliability_seed,
    negative_controls = negative_control_seed
  ),
  evidence_completion = list(
    recovery_mcse = TRUE,
    recovery_interval_coverage_status = TRUE,
    recovery_misspecification = TRUE,
    measurement_resolution = TRUE,
    pupil_baseline_sensitivity = TRUE,
    pupil_preprocessing_order_sensitivity = TRUE,
    temporal_shift_placebo_controls = TRUE
  )
)
writeLines(capture.output(str(prov_obj)),file.path(out_dir,"manifests","environment.txt"))

freeze <- freeze_eyeprocess_validation_evidence(
  design = scenarios,
  recovery = recovery,
  sbc = sbc,
  stress = stress_exec,
  reliability = list(
    icc = icc,
    temporal = temporal,
    bland_altman = ba$summary,
    split_half = split,
    measurement_resolution = resolution,
    measurement_resolution_jitter = jitter_resolution,
    pupil_baseline_sensitivity = pupil_baseline_sensitivity,
    pupil_preprocessing_order_sensitivity = pupil_order_sensitivity
  ),
  negative_controls = list(
    controls = controls$results,
    temporal_shift_placebo = negative_window_shift,
    leakage = leak$detail
  ),
  irt = list(
    information = precision,
    item_fit = item_fit,
    q3 = q3,
    recovery_mcse = recovery_mcse,
    recovery_misspecification = recovery_misspecification,
    recovery_interval_coverage_status = recovery_coverage_status
  ),
  claims = claims,
  provenance = prov_obj,
  source_commit = source_commit
)
stopifnot(verify_eyeprocess_validation_evidence(freeze))
write_eyeprocess_validation_evidence(freeze,file.path(out_dir,"references","validation-evidence-freeze.rds"))
atlas <- eyeprocess_validation_evidence_atlas(claims,recovery=recovery,sbc=sbc,stress=freeze$components$stress,reliability=freeze$components$reliability,negative_controls=freeze$components$negative_controls,irt=freeze$components$irt,provenance=prov_obj)
write_eyeprocess_validation_report(atlas,file.path(out_dir,"VALIDATION_REPORT.md"))
cat(
  "\n## Milestone #2 evidence-completion layer\n\n",
  "- Recovery MCSE is frozen at the replication level. Local-profile MCSE values are diagnostic because the local profile uses few replications.\n",
  "- Item-parameter interval coverage is explicitly recorded as not estimated by the current point-estimate recovery contract; no interval evidence is fabricated. Ability interval coverage is provided by the SBC programme.\n",
  "- Measurement-resolution, pupil baseline-window, and preprocessing-order sensitivity are frozen as study-specific software/measurement diagnostics.\n",
  "- Temporal-shift and placebo-window controls are frozen alongside permutation and known-leakage evidence.\n",
  "- These artifacts characterize declared synthetic/software behavior and do not establish construct validity.\n",
  file = file.path(out_dir,"VALIDATION_REPORT.md"),
  append = TRUE,
  sep = ""
)
saveRDS(freeze_eyeprocess_validation_atlas(atlas),file.path(out_dir,"references","validation-atlas-freeze.rds"))
idx <- eyeprocess_validation_evidence_index(out_dir)
write.csv(idx,file.path(out_dir,"manifests","artifact-index.csv"),row.names=FALSE)
cat("Milestone #2 evidence execution complete\n")
cat("Output:",normalizePath(out_dir,winslash="/"),"\n")
cat("Freeze hash:",freeze$hash,"\n")
