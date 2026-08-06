# Redact a validation case without inventing replacement data

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
redact_validation_case(source_path, output_path, id_columns = c("participant_id",
  "subject", "participant", "recording_id", "session_id"), remove_columns = c("name",
  "email", "address", "birthdate", "date_of_birth"), text_redactor = NULL, salt,
  copy_non_tabular = FALSE, overwrite = FALSE)
```

## Arguments

- source_path:

  Source file/directory.

- output_path:

  Redacted output directory.

- id_columns:

  Identifier columns to pseudonymize.

- remove_columns:

  Columns to remove.

- text_redactor:

  Optional function applied to character columns.

- salt:

  Required project-specific salt.

- copy_non_tabular:

  Copy unsupported files unchanged.

- overwrite:

  Replace output.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
