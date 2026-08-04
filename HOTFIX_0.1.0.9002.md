# eyeprocess 0.1.0.9002 validation-runtime hotfix

This release corrects the remaining empirical-validation test findings reported
on Windows after 0.1.0.9001.

## Corrections

- Removed unsupported `qmethod` forwarding through `utils::write.csv()`, which
  generated one warning per canonical table on Windows.
- Assigned explicit canonical units to imported biometric channels. Known
  channels use canonical units and unrecognized channels use `vendor_units`,
  preserving uncertainty without leaving the unit field empty.
- Made `init_validation_corpus()` safe to run repeatedly. Existing manifests,
  README files, and export cases are preserved unless `overwrite = TRUE`.
- Added regression tests for warning-free round trips, populated biometric
  units, and non-destructive repeated corpus initialization.

No psychometric, plotting, coordinate, AOI, or vendor parsing algorithm was
otherwise changed.
