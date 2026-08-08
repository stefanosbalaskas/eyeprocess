# Execute and audit an Eye-Tracking-BIDS round trip

This is a callback harness so it remains stable even if the package's
BIDS writer/reader signatures evolve. \`exporter\` receives the source
object plus \`export_args\`; \`importer\` receives the exporter result
plus \`import_args\`.

## Usage

``` r
roundtrip_eye_bids(
  source,
  exporter,
  importer,
  export_args = list(),
  import_args = list(),
  extract_samples = function(x) {
     if (is.data.frame(x))
         x
     else if
(is.list(x) && !is.null(x$samples))
x$samples
     else
    stop("Define `extract_samples` for this object class.", call. = FALSE)
 },
  audit_args = list()
)
```

## Arguments

- source:

  Source eyeprocess object/table.

- exporter:

  Function that writes/exports BIDS and returns a locator or object
  consumable by \`importer\`.

- importer:

  Function that reconstructs an eyeprocess object/table.

- export_args, import_args:

  Named argument lists.

- extract_samples:

  Function extracting the canonical sample table from source and
  reconstructed objects.

- audit_args:

  Arguments forwarded to \`semantic_roundtrip_audit()\`.
