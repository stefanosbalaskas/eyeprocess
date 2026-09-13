# Lock a decision manifest by content hash

Lock a decision manifest by content hash

## Usage

``` r
lock_decision_manifest(x, label = "analysis_decisions")
```

## Arguments

- x:

  Manifest.

- label:

  Optional lock label.

## Value

An object of class "eye_decision_manifest_lock", stored as a named list,
with components "manifest", "manifest_hash", "label", "locked_at",
"status". It contains lock a decision manifest by content hash and
associated metadata or diagnostics needed to interpret the result.
