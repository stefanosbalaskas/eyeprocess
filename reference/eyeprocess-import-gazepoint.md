# Import Gazepoint and Gazepoint Biometrics exports

Detect, profile, import, pair, validate, and reconstruct Gazepoint
Analysis and Biometrics exports, including Gazepoint Analysis 7.x
`*_all_gaze.csv`, `*_fixations.csv`, and multi-section
`Data_Summary_export_*.csv` files.

## Details

These functions operate on the canonical relational representation used
by eyeprocess. For Gazepoint Analysis 7.x exports, `TIMETICK(f=...)` is
retained as the native clock and normalized to seconds from recording
start, while media-relative `TIME(...)` values are retained as source
fields. Fixation identifiers that restart for each media item are
namespaced by stimulus. Multi-section Data Summary reports are parsed
into AOI definitions, user-AOI features, raw report tables, and vendor
metadata. Native fields and transformations remain represented in
provenance and vendor-specific metadata.

## Value

The returned value depends on the function. Import and transformation
functions generally return an `eye_dataset`; audit and feature functions
return data frames or enriched datasets; plotting functions return their
plotted data invisibly; modelling functions return engine-specific or
`eyeprocess_model` objects.

## See also

[`new_eye_dataset()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-class.md),
[`read_eye_export()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-adapters.md),
[`validate_eye_dataset()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-class.md),
[`provenance_manifest()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-class.md)
