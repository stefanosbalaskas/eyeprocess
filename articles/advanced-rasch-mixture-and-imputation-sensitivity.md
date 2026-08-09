# Advanced Rasch, mixture-IRT, and imputation sensitivity

This article collects optional diagnostic/sensitivity adapters that
complement, rather than replace, the stable process-IRT core.

## True response-mixture IRT

``` r

mix <- fit_mixture_irt_process_classes(
  binary_response_matrix,
  n_classes = 2,
  itemtype = "2PL"
)
plot(mix)
```

The mixture components are latent response-distribution classes. They
must not be named as cognitive strategies without external
response-process evidence.

If defensible class assignments have been extracted, compare them with
independent process summaries:

``` r

alignment <- map_latent_classes_to_process_profiles(
  class_membership,
  person_process_data,
  person = "person_id",
  class_col = "class",
  process_features = c("dwell_ms", "pupil_peak", "aoi_entropy", "revisits")
)
plot(alignment)
```

## Nonparametric Rasch diagnostics

``` r

np <- audit_nonparametric_rasch(
  binary_response_matrix,
  methods = c("T1", "T10"),
  n = 100
)
np$status
plot(np, method = "T1")
```

## Stepwise item-reduction sensitivity

``` r

red <- audit_item_reduction_sensitivity(
  erm_rasch,
  criterion = list("itemfit"),
  alpha = 0.05,
  maxstep = 5
)
red$eliminated_items
plot(red)
```

Automated elimination is never sufficient evidence for deleting an item;
content validity, theoretical coverage, DIF, local dependence, and
process evidence remain required.

## Biometric-feature imputation sensitivity

``` r

imp <- biometric_imputation_sensitivity(
  trial_process_data,
  variables = c("rt_ms", "dwell_ms", "pupil_peak", "pupil_auc", "valid_gaze_prop"),
  methods = c("mice", "missForest")
)
imp$missingness
imp$status
plot(imp)
```

Completed/imputed data are sensitivity datasets by default and do not
silently replace the primary missingness strategy.

## Process-informed Rasch trees

``` r

tree <- fit_process_rasch_tree(
  binary_response_matrix,
  covariates = person_process_covariates
)
plot(tree)
```

Tree splits diagnose conditional item-parameter heterogeneity. They are
not automatically psychological strategy classes.
