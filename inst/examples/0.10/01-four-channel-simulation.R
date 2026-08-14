library(eyeprocess)
sim <- simulate_multimodal_irt(n_person=80, n_item=12, seed=42)
sim
audit_multimodal_measurement(sim$measurement)
validate_multimodal_irt(sim)
plot(sim, type="latent_correlation")
plot(sim$measurement, type="missingness")
