# Validate vendor-specific timestamp semantics

The audit records expected semantics rather than silently coercing
clocks. Tobii data can carry device/system timing; Pupil Labs Neon
timestamps are high-resolution UTC nanoseconds in native recordings;
Gazepoint may expose native monotonic and media-relative clocks
depending on export type.

## Usage

``` r
validate_vendor_timestamp_semantics(
  data,
  vendor,
  device_time = NULL,
  system_time = NULL,
  media_time = NULL
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- vendor:

  Vendor identifier.

- device_time:

  Device timestamp column.

- system_time:

  System timestamp column.

- media_time:

  Media/stimulus timestamp column.
