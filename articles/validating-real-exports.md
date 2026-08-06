# Validating Real Eye-Tracking Exports

## Why empirical validation is separate from declared support

An adapter can be structurally implemented and exercised with synthetic
fixtures without yet being validated against the diversity of real
exports created by different device models, software versions, export
selections, and laboratory conventions. `eyeprocess` therefore
distinguishes:

1.  **declared support**, based on the implemented parser and documented
    format;
2.  **synthetic-fixture validation**, based on reproducible package
    fixtures;
3.  **real-export validation**, based on de-identified empirical files.

``` r

head(eye_format_profiles())
#>              format_id               vendor   adapter
#> 1   gazepoint_analysis            Gazepoint gazepoint
#> 2  gazepoint_fixations            Gazepoint gazepoint
#> 3        gazepoint_aoi            Gazepoint gazepoint
#> 4 gazepoint_biometrics Gazepoint Biometrics gazepoint
#> 5        tobii_pro_lab                Tobii     tobii
#> 6       pupillabs_neon           Pupil Labs pupillabs
#>                   export_family          input_kind dedicated_adapter  gaze
#> 1        Analysis sample export      file_or_folder              TRUE  TRUE
#> 2      Analysis fixation export                file              TRUE FALSE
#> 3                AOI statistics                file              TRUE FALSE
#> 4 Combined or paired biometrics      file_or_folder              TRUE  TRUE
#> 5        Pro Lab tabular export                file              TRUE  TRUE
#> 6            Neon folder export folder_or_gaze_file              TRUE  TRUE
#>   pupil episodes events   aoi biometrics calibration default_time_unit
#> 1  TRUE     TRUE   TRUE  TRUE       TRUE       FALSE           seconds
#> 2 FALSE     TRUE  FALSE FALSE      FALSE       FALSE           seconds
#> 3 FALSE    FALSE  FALSE  TRUE      FALSE       FALSE           seconds
#> 4  TRUE    FALSE   TRUE FALSE       TRUE       FALSE           seconds
#> 5  TRUE     TRUE   TRUE  TRUE      FALSE       FALSE      microseconds
#> 6  TRUE     TRUE   TRUE FALSE      FALSE       FALSE       nanoseconds
#>      default_coordinate_space  validation_level
#> 1 display_normalized_top_left synthetic_fixture
#> 2 display_normalized_top_left synthetic_fixture
#> 3 display_normalized_top_left          declared
#> 4 display_normalized_top_left synthetic_fixture
#> 5     display_pixels_top_left synthetic_fixture
#> 6         world_camera_pixels synthetic_fixture
#>                                                                                        notes
#> 1  First-class adapter; native columns and out-of-range normalized coordinates are retained.
#> 2                                Vendor-derived fixations remain labelled as vendor-derived.
#> 3                    AOI summaries are retained separately from sample-level AOI assignment.
#> 4 Supports pupil, EDA/GSR, heart-rate, IBI, and engagement-style channels when identifiable.
#> 5                           Handles heterogeneous gaze/event rows and separate eye validity.
#> 6                  Uses gaze.csv plus optional fixation, event, and 3D eye-state companions.
```

## Validate one source

The following example uses the bundled Gazepoint fixture. For real work,
replace `path` with a source file or export folder.

``` r

path <- system.file("extdata", "gazepoint", "demo-user.csv", package = "eyeprocess")

result <- validate_eye_source(
  path,
  vendor = "gazepoint",
  spec = format_validation_spec(run_roundtrip = TRUE),
  import_args = list(recording_id = "R001", quiet = TRUE),
  retain_dataset = TRUE,
  case_id = "gazepoint-demo"
)

summary(result)
#>          case_id
#> 1 gazepoint-demo
#>                                                                                                                     path
#> 1 C:/Users/Stefanos-PC/AppData/Local/Temp/RtmpIV6txd/temp_libpath2ba445525e77/eyeprocess/extdata/gazepoint/demo-user.csv
#>      vendor status files detection_confidence imported validation_errors
#> 1 gazepoint   pass     1                    1     TRUE                 0
#>   validation_warnings roundtrip
#> 1                   0      pass
result$checks
#>                       check status      value
#> 1          format_detection   pass  1.0000000
#> 2                    import   pass  1.0000000
#> 3        dataset_validation   pass  0.0000000
#> 4  critical_schema_coverage   pass  0.7321429
#> 5              gaze_samples   pass 12.0000000
#> 6  native_time_preservation   pass  1.0000000
#> 7          coordinate_space   pass  1.0000000
#> 8                provenance   pass  2.0000000
#> 9             raw_retention   pass  2.0000000
#> 10      canonical_roundtrip   pass  1.0000000
#>                                                                message
#> 1                                         Selected adapter: gazepoint.
#> 2                                   Source imported to an eye_dataset.
#> 3                                               0 validation issue(s).
#> 4                             Critical canonical fields were assessed.
#> 5                                      Gaze observations are required.
#> 6            Native and normalized time should remain distinguishable.
#> 7                             Coordinate semantics should be explicit.
#> 8           Transformations and source references should be auditable.
#> 9                                    Raw source retention is optional.
#> 10 Canonical folder export and re-import were compared table by table.
```

The result retains separate evidence for format detection,
adapter-specific findings, canonical validation, schema coverage, source
preservation, quality audits, and canonical round-trip comparison.

## Build a validation corpus

Create a private corpus skeleton with:

``` r

init_validation_corpus("C:/private/eyeprocess-validation-corpus")
```

The initializer is safe to run repeatedly: existing manifest and case
files are preserved unless `overwrite = TRUE` is supplied.

A corpus should normally contain one directory per export case. Record
the vendor, device model, software version, export family, and any known
options in a manifest.

``` r

manifest <- validation_manifest(
  paths = c(
    system.file("extdata", "gazepoint", "demo-user.csv", package = "eyeprocess"),
    system.file("extdata", "tobii-demo.tsv", package = "eyeprocess")
  ),
  vendor = c("gazepoint", "tobii"),
  format_family = c("gazepoint_analysis", "tobii_pro_lab"),
  software_version = c("fixture", "fixture"),
  case_id = c("gp-fixture", "tobii-fixture")
)

corpus <- validate_eye_corpus(
  manifest,
  spec = format_validation_spec(run_roundtrip = FALSE),
  import_args = list(
    `gp-fixture` = list(recording_id = "R001", quiet = TRUE),
    `tobii-fixture` = list(recording_id = "R002", quiet = TRUE)
  )
)

corpus$summary
#>                     case_id
#> gp-fixture       gp-fixture
#> tobii-fixture tobii-fixture
#>                                                                                                                                 path
#> gp-fixture    C:/Users/Stefanos-PC/AppData/Local/Temp/RtmpIV6txd/temp_libpath2ba445525e77/eyeprocess/extdata/gazepoint/demo-user.csv
#> tobii-fixture          C:/Users/Stefanos-PC/AppData/Local/Temp/RtmpIV6txd/temp_libpath2ba445525e77/eyeprocess/extdata/tobii-demo.tsv
#>                  vendor status files detection_confidence imported
#> gp-fixture    gazepoint   pass     1                 1.00     TRUE
#> tobii-fixture     tobii   pass     1                 0.85     TRUE
#>               validation_errors validation_warnings roundtrip
#> gp-fixture                    0                   0      pass
#> tobii-fixture                 0                   0      pass
#>                    format_family software_version device_model expected_import
#> gp-fixture    gazepoint_analysis          fixture         <NA>            TRUE
#> tobii-fixture      tobii_pro_lab          fixture         <NA>            TRUE
#>               require_gaze require_native_time require_coordinate_space
#> gp-fixture            TRUE                TRUE                     TRUE
#> tobii-fixture         TRUE                TRUE                     TRUE
#>               require_provenance require_raw_retention run_roundtrip notes
#> gp-fixture                  TRUE                 FALSE          TRUE  <NA>
#> tobii-fixture               TRUE                 FALSE          TRUE  <NA>
#>               validation_status expectation_met
#> gp-fixture                 pass            TRUE
#> tobii-fixture              pass            TRUE
```

The compatibility matrix can then combine declared capabilities with
observed case outcomes.

``` r

format_compatibility_matrix(corpus)[, c(
  "format_id", "adapter", "validation_level",
  "empirical_cases", "empirical_passes", "empirical_failures"
)]
#>               format_id   adapter  validation_level empirical_cases
#> 1    gazepoint_analysis gazepoint synthetic_fixture               1
#> 2   gazepoint_fixations gazepoint synthetic_fixture               0
#> 3         gazepoint_aoi gazepoint          declared               0
#> 4  gazepoint_biometrics gazepoint synthetic_fixture               0
#> 5         tobii_pro_lab     tobii synthetic_fixture               1
#> 6        pupillabs_neon pupillabs synthetic_fixture               0
#> 7        pupillabs_core pupillabs synthetic_fixture               0
#> 8           eyelink_asc   eyelink synthetic_fixture               0
#> 9   eyelink_data_viewer   eyelink          declared               0
#> 10          eyelink_edf   eyelink          declared               0
#> 11      smi_begaze_text       smi synthetic_fixture               0
#> 12    generic_delimited   generic   generic_mapping               0
#>    empirical_passes empirical_failures
#> 1                 1                  0
#> 2                 0                  0
#> 3                 0                  0
#> 4                 0                  0
#> 5                 1                  0
#> 6                 0                  0
#> 7                 0                  0
#> 8                 0                  0
#> 9                 0                  0
#> 10                0                  0
#> 11                0                  0
#> 12                0                  0
```

## Produce a safe validation bundle

Validation bundles contain the report, checks, manifests, coverage
tables, and optionally an anonymized canonical dataset. They do not
include raw vendor exports.

``` r

create_validation_bundle(
  result,
  path = "gazepoint-validation-bundle.zip",
  include_dataset = TRUE,
  anonymize = TRUE,
  overwrite = TRUE
)
```

Automated anonymization replaces core identifiers, removes raw data and
source paths, and can redact free-text values. It cannot identify every
possible study-specific disclosure. Review every bundle before sharing
it.

## Recommended real-export acceptance process

For each vendor and software version:

1.  collect multiple de-identified exports with different export
    selections;
2.  record device, firmware, software version, sampling rate, and
    coordinate configuration;
3.  validate each case using a manifest;
4.  inspect every warning and failed canonical field;
5.  verify trial markers, pupil units, fixation provenance, and AOI
    semantics;
6.  retain compatibility evidence with the package release;
7.  promote support from fixture-tested to empirically validated only
    after all required cases pass.
