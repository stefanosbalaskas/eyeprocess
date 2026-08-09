# Process-decision proxies and research-frontier gates

## Pre-action and gaze-evidence representations

``` r

pre <- preaction_process_features(samples)
proxy <- addm_glam_proxy_features(
  samples,
  target_aoi = "target",
  distractor_aoi = "distractor",
  action_aoi = "button"
)
plot(pre)
plot(proxy)
```

These are aDDM/GLAM-inspired feature summaries. They are not fitted
drift rate, gaze-discount, or decision-threshold parameters.

``` r

process_feature_family_registry()
assign_process_feature_family(c("pupil_peak", "aoi_entropy", "valid_gaze_prop"))
process_feature_stability(repeated_importance_table)
```

## Frontier estimators remain gated

``` r

fit_kde_latent_distribution_irt(response_matrix)
fit_persistence_gaze_diffusion_irt(process_data)
fit_nonignorable_missing_irt(missingness_data)
fit_crossclassified_process_irt_mhrm(crossclassified_data)
```

Without an explicit validated external engine these functions return a
gated model contract rather than silently substituting a simpler model.

``` r

repr <- prepare_structured_unstructured_process_features(
  structured_features,
  unstructured = sequence_data,
  fold = "fold_id"
)
```

The representation contract states that learned scaling, vocabulary,
embedding, feature selection, and similar operations must be fitted
inside training folds only.
