# Specify multi-vendor empirical validation requirements

Specify multi-vendor empirical validation requirements

## Usage

``` r
vendor_validation_spec(
  required_vendors = c("gazepoint", "tobii", "pupillabs", "eyelink", "smi"),
  min_cases_per_vendor = 2L,
  min_pass_rate = 0.95,
  require_versions = TRUE,
  require_devices = TRUE,
  require_independent_sources = TRUE,
  require_licence_reviewed = TRUE
)
```

## Arguments

- required_vendors:

  Vendors that must be represented.

- min_cases_per_vendor:

  Minimum independent cases per vendor.

- min_pass_rate:

  Minimum acceptable pass rate per vendor.

- require_versions:

  Require non-missing software versions.

- require_devices:

  Require non-missing device models.

- require_independent_sources:

  Require each case to be marked as an independently obtained source
  rather than a duplicated fixture.

- require_licence_reviewed:

  Require each corpus case to have a completed data/code licence and
  redistribution review.

## Value

An \`eye_vendor_validation_spec\`.
