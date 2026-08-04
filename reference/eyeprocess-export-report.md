# Export, reporting, and package bridges

Persist canonical datasets, produce provenance-aware reports, and bridge
gp3tools or gpbiometrics objects.

## Details

These functions operate on the canonical relational representation used
by eyeprocess. Native fields and timestamps are retained whenever
possible, and transformations should be recorded in the provenance
table. Optional modelling engines are used only when their packages are
installed.

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
