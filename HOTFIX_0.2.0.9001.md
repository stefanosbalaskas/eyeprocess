# eyeprocess 0.2.0.9001 Gazepoint compatibility hotfix

The genuine Gazepoint Analysis 7.2.0 validation corpus passed in
0.2.0.9000, but the full package test gate exposed two
legacy-compatibility defects.

## Corrections

1.  The bundled legacy fixture contains `GSR`, which is a raw vendor
    signal. The test now requires `gsr_raw` rather than incorrectly
    requiring processed `eda`. `GSR_US` and explicit `EDA` fields
    continue to map to `eda` in microsiemens.
2.  [`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-gazepoint.md)
    now declares `recording_id` explicitly. This prevents the same
    argument from being passed both explicitly and through `...`.
3.  [`read_gazepoint()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-gazepoint.md)
    forwards a directory-level `recording_id` override.
4.  A single recording-ID override is accepted only for folders with one
    recording group; multi-recording folders retain filename-derived
    identifiers.

No change was made to the already successful six-recording real-export
parser, clock normalization, fixation handling, AOI Summary parsing,
biometrics import, corpus validation, or round-trip evidence.
