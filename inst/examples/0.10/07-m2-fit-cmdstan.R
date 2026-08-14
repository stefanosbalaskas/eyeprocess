library(eyeprocess)

sim <- simulate_multimodal_m2(
  n_person = 100,
  n_item = 10,
  seed = 20260817
)

# Manual estimator example. The M2 reference backend is gated and
# requires cmdstanr + a configured CmdStan toolchain.
if (
  requireNamespace("cmdstanr", quietly = TRUE) &&
  nzchar(tryCatch(cmdstanr::cmdstan_path(), error = function(e) ""))
) {
  message(
    "CmdStan is available. Run fit_multimodal_m2(sim) when a full ",
    "Bayesian fit is intended; this example does not start sampling automatically."
  )
} else {
  message(
    "CmdStan is unavailable. No fallback estimator is substituted."
  )
}
