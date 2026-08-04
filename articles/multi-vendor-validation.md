# Multi-vendor empirical validation

`eyeprocess` separates adapter availability, fixture testing, and
empirical compatibility. A production claim requires independent real
exports for each software version and device family, together with an
explicit licence and redistribution review.

``` r

corpus <- validate_eye_corpus("C:/private/eyeprocess-validation-corpus")
audit <- audit_vendor_validation(
  corpus,
  vendor_validation_spec(
    required_vendors = c("gazepoint", "tobii", "pupillabs", "eyelink", "smi"),
    min_cases_per_vendor = 2,
    min_pass_rate = 0.95,
    require_licence_reviewed = TRUE
  )
)
print(audit)
plot(audit)
write_vendor_validation_report(audit, "validation/vendor-validation.md")
```

Raw proprietary exports should remain outside the public repository.
Share only manually reviewed, de-identified validation bundles and
aggregate evidence.
