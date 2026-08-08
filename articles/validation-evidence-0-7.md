# Independent Vendor Validation and Semantic Fidelity

## Why import success is not validation

`eyeprocess` treats vendor compatibility as an evidence claim. A file
that can be read without error has established only parser reachability.
It has not shown that timestamps, coordinate systems, eye identity,
pupil units, event meanings, or missingness semantics survived
harmonisation.

Version 0.7 therefore adds a detailed evidence ladder:

``` r

eyeprocess::validation_evidence_levels()
#>   rank                         level
#> 1    1                      declared
#> 2    2             synthetic-fixture
#> 3    3                vendor-example
#> 4    4       independent-public-real
#> 5    5 multisession-multidevice-real
#> 6    6  semantic-roundtrip-validated
#>                                                                                          requirement
#> 1                                                             Adapter or schema support is declared.
#> 2                           Deterministic synthetic or package fixture passes the declared contract.
#> 3                                A vendor-provided example export passes import and semantic checks.
#> 4                        An independently produced public real recording passes the declared checks.
#> 5                        Evidence spans repeated sessions and/or more than one device/model context.
#> 6 Native-to-canonical-to-interchange-to-canonical round trip has field-level semantic-loss evidence.
```

The intended progression is:

1.  `declared`
2.  `synthetic-fixture`
3.  `vendor-example`
4.  `independent-public-real`
5.  `multisession-multidevice-real`
6.  `semantic-roundtrip-validated`

These detailed tiers complement, rather than replace, the package’s
existing support-status mechanism.

## Public validation corpus

The package does **not** auto-download public human-participant
datasets. Use the manifest to review licences/terms and select cases
deliberately.

``` r

corpus <- eyeprocess::public_validation_corpus()
corpus[, c("ecosystem", "device", "corpus", "evidence_goal", "access")]
#>    ecosystem                                              device
#> 1  Gazepoint                                       GP3 HD 150 Hz
#> 2  Gazepoint                                       GP3 HD 150 Hz
#> 3    EyeLink EyeLink (raw EDF; device metadata verify in corpus)
#> 4    EyeLink                                EyeLink 1000 1000 Hz
#> 5    EyeLink                        EyeLink Portable Duo 1000 Hz
#> 6      Tobii                             Tobii Pro Fusion 120 Hz
#> 7      Tobii                          Tobii Pro Glasses 3 ~50 Hz
#> 8 Pupil Labs                                                Neon
#>                                                                 corpus
#> 1                            Pavia free observation of moving elements
#> 2                               Pavia symmetric dynamic stimuli (2026)
#> 3               Raw eye-tracking data (EyeLink EDF; 10 subjects, 2026)
#> 4                                                             GazeBase
#> 5 Eye movement benchmark data for smooth-pursuit classification (2026)
#> 6                                                         MCFW-Gaze v3
#> 7                                                     GroupAffect-4 v3
#> 8                            Pupil Labs official Neon sample recording
#>                                                   evidence_goal
#> 1 independent-public-real; multisession-real; raw Gazepoint CSV
#> 2 independent-public-real; multisession-real; raw Gazepoint CSV
#> 3                      raw-native-format; EDF parser validation
#> 4             large multisession longitudinal EyeLink benchmark
#> 5           raw EDF plus ASC conversion; event-parser benchmark
#> 6            remote-screen Tobii; binocular continuous raw gaze
#> 7               wearable Tobii; synchronized multimodal streams
#> 8                 vendor-example; native/CSV semantic roundtrip
#>                                            access
#> 1                                          public
#> 2 public; research/education/non-commercial terms
#> 3                                          public
#> 4                                public CC BY 4.0
#> 5                                      public OSF
#> 6                                   public Zenodo
#> 7            public Zenodo; some audio restricted
#> 8                                  vendor example
```

The initial corpus targets two independent Gazepoint GP3 HD
repeated-session sets, raw EyeLink EDF data, GazeBase, a 2026 EyeLink
smooth-pursuit benchmark, Tobii Pro Fusion, Tobii Pro Glasses 3, and the
official Pupil Labs Neon example.

## Semantic round-trip contract

A strong test is not

    native -> import succeeds

but

    native vendor
       -> eyeprocess canonical
       -> BIDS eye tracking
       -> eyeprocess canonical
       -> field-by-field semantic comparison

Every compared field should be classified explicitly, for example as
`LOSSLESS`, `UNIT_TRANSFORMED`, `COORDINATE_TRANSFORMED`,
`SEMANTICALLY_EQUIVALENT`, `DERIVED`, `UNSUPPORTED`,
`INTENTIONALLY_DROPPED`, or `AMBIGUOUS`.

``` r

spec <- semantic_fidelity_spec(
  timestamp_tolerance = 1e-6,
  coordinate_tolerance = 1e-6,
  pupil_tolerance = 1e-6
)

audit <- semantic_roundtrip_audit(
  original = native_canonical,
  roundtrip = bids_reimported,
  key = c("recording_id", "sample_index"),
  fields = c("timestamp", "gaze_x", "gaze_y", "pupil", "eye", "event")
)

semantic_loss_map(audit)
plot(audit)
```

## Timestamp semantics

Clock meaning is part of the schema. Device timestamps and system
timestamps are not interchangeable merely because both are numeric.

``` r

timestamp_fidelity_audit(
  source = imported_native,
  roundtrip = imported_bids,
  source_time = "device_time",
  roundtrip_time = "device_time",
  tolerance = 1e-6
)

validate_vendor_timestamp_semantics(imported_native)
```

## Coordinate and pupil fidelity

Coordinate transformations are acceptable when they are explicit and
invertible. Silent transformations are evidence failures.

``` r

coordinate_fidelity_audit(
  source = original,
  roundtrip = transformed_back,
  source_x = "gaze_x",
  source_y = "gaze_y",
  roundtrip_x = "gaze_x",
  roundtrip_y = "gaze_y"
)

pupil_unit_fidelity_audit(
  source = original,
  roundtrip = transformed_back,
  source_pupil = "pupil_left",
  roundtrip_pupil = "pupil_left"
)
```

## BIDS eye-tracking semantics

BIDS 1.11.1 now specifies eye tracking under physiological recordings.
Among the important semantics are `PhysioType = "eyetrack"`,
`RecordedEye`, and `SampleCoordinateSystem`; gaze-on-screen recordings
also require screen presentation metadata.
[`validate_bids_eye_semantics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/validate_bids_eye_semantics.md)
is a lightweight structural audit for these requirements. It is
intentionally not presented as a replacement for the official BIDS
validator.

``` r

validate_bids_eye_semantics(
  data = bids_table,
  metadata = bids_json
)
```

## HED event semantics

Event survival is not enough. An event that becomes `event_17` has
preserved an identifier but may have lost experimental meaning. HED
provides a controlled, machine-actionable event vocabulary.

``` r

event_semantics_audit(original_events, roundtrip_events,
                      key = "event_id", label = "trial_type", time = "timestamp")
validate_hed_event_semantics(events)
```

[`validate_hed_event_semantics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/validate_hed_event_semantics.md)
performs only package-level structural checks. For formal HED-schema
validation, use the official HED tooling.

## Evidence matrix

``` r

base <- build_compatibility_matrix()
case_evidence <- data.frame(
  ecosystem = "Gazepoint",
  device = "GP3 HD",
  evidence_level = "independent-public-real",
  semantic_roundtrip_pass = FALSE
)

mat <- compatibility_evidence_matrix(base, case_evidence)
plot(mat)
```

A vendor should be promoted only from retained evidence, not from
undocumented manual impressions.
