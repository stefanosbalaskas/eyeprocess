library(eyeprocess)
sim <- simulate_multimodal_irt(n_person=60, n_item=10, seed=11)

# Existing eyeprocess channel constructors should be used in the real workflow.
# This example is intentionally gated because the exact constructor arguments
# depend on the installed eyeprocess channel API.
multimodal_backend_status()

if (requireNamespace("cmdstanr", quietly=TRUE)) {
  message("CmdStan development engine is available. Fit only after the local ",
          "0.10 channel-specification audit has established the exact existing ",
          "irt_*_channel() constructor calls.")
}
