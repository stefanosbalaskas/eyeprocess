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

## Value

An object of class "eye_validation_scenario_manifest", stored as a named
list, with components "label", "plan_hash", "scenarios",
"source_commit", "generated_at", "scientific_scope". It contains a
scenario manifest for frozen validation work and associated metadata or
diagnostics needed to interpret the result.
