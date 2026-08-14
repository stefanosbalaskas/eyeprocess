library(eyeprocess)
sim <- simulate_multimodal_irt(n_person=60, n_item=10, seed=7)
abl <- ablate_multimodal_channels(sim$measurement)
abl
names(abl$scenarios)
