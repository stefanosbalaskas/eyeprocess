# Create a scenario manifest for frozen validation work

Create a scenario manifest for frozen validation work

## Usage

``` r
validation_scenario_manifest(
  plan,
  source_commit = NA_character_,
  generated_at = Sys.time()
)
```

## Arguments

- plan:

  Validation or stress-evidence plan object.

- source_commit:

  Source-control commit associated with the evidence.

- generated_at:

  Generation timestamp stored in the manifest.
