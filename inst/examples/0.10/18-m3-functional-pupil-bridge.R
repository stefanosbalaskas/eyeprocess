sim <- simulate_multimodal_m3(n_person = 40, n_item = 8, seed = 20260815)
d <- sim$data
# In a real analysis this score should come from the existing functional pupil/deconvolution workflow.
d$functional_score <- as.numeric(scale(d$pupil_baseline - mean(d$pupil_baseline, na.rm = TRUE)))
bridge <- multimodal_m3_functional_bridge(d, "functional_score", provenance = "demonstration score only")
print(bridge)
spec <- multimodal_m3_spec(pupil_representation = "functional_score")
print(spec)
