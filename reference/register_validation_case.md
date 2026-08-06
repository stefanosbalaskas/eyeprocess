# Register an independent validation case

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
register_validation_case(corpus_path, source_path, vendor, device_model,
  software_name, software_version, hardware_version = NA_character_,
  export_profile = NA_character_, sampling_rate_hz = NA_real_,
  coordinate_system = NA_character_, timebase = NA_character_,
  event_semantics = NA_character_, ocular_structure = NA_character_,
  missingness_convention = NA_character_, vendor_fixations = NA_character_,
  package_transformations = NA_character_, unsupported_fields = NA_character_,
  independent_source = TRUE, licence_reviewed = FALSE, redistribution_allowed = FALSE,
  support_level = c("declared", "fixture-tested", "empirically-validated"),
  mode = c("reference", "copy"), case_id = NULL, notes = NA_character_)
```

## Arguments

- corpus_path:

  Corpus directory.

- source_path:

  Real export file or directory.

- vendor:

  Vendor name.

- device_model:

  Hardware model.

- software_name:

  Export software and version.

- software_version:

  Export software and version.

- hardware_version:

  Optional hardware/firmware version.

- export_profile:

  Export options/profile.

- sampling_rate_hz:

  Nominal or observed rate.

- coordinate_system:

  Semantics metadata.

- timebase:

  Semantics metadata.

- event_semantics:

  Semantics metadata.

- ocular_structure:

  Monocular/binocular structure.

- missingness_convention:

  Vendor missing-value convention.

- vendor_fixations:

  Description of vendor-derived fixation fields.

- package_transformations:

  Declared package transformations.

- unsupported_fields:

  Known unsupported fields.

- independent_source:

  Whether independently obtained.

- licence_reviewed:

  Whether licensing review is complete.

- redistribution_allowed:

  Whether redacted material may be redistributed.

- support_level:

  Declared support level.

- mode:

  Reference source in place or copy it into the private corpus.

- case_id:

  Optional explicit identifier.

- notes:

  Notes.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
