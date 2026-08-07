# Process Reliability and Device Transportability

A process feature should not be treated as an individual-difference
measure until its dependability across items, sessions, and devices is
quantified.

``` r

gstudy <- fit_process_gstudy(
  process_long,
  metric = "pupil_auc",
  facets = c("person", "item", "session", "device")
)
process_variance_components(gstudy)
plot_variance_components(gstudy)
dstudy <- design_process_dstudy(
  gstudy,
  items = seq(5, 40, 5),
  sessions = 1:4,
  devices = 1:2
)
plot_dependability_surface(dstudy)
reliability <- audit_process_reliability(
  process_long,
  metrics = c("dwell_ms", "pupil_auc", "aoi_entropy"),
  method = "icc"
)
plot_reliability_by_metric(reliability)
```

Vendor-neutral import does not imply metric equivalence. Paired
cross-device data can be linked and audited against a declared
equivalence margin.

``` r

link <- fit_device_linking(
  paired_device_data,
  metric = "pupil_auc",
  reference_device = "laboratory_reference",
  id_cols = c("person_id", "trial_id")
)
plot_device_agreement(link)
plot_device_bias_by_magnitude(link)
plot_device_transfer_curve(link)
equivalence <- audit_device_equivalence(link, equivalence_margin = 0.05)
plot_device_equivalence_intervals(equivalence)
```
