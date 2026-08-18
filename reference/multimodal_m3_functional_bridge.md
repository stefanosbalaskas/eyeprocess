# Bridge existing functional pupil outputs into the scalar M3 reference layer

This bridge deliberately requires an analyst-supplied, already-derived
trial-level functional score. It does not silently reduce a raw time
series. Existing \`functional_pupil_irt_spec()\`, deconvolution, and
confound tools remain the authoritative trajectory-level machinery.

## Usage

``` r
multimodal_m3_functional_bridge(
  data,
  score,
  pupil = "pupil",
  provenance = NULL
)
```

## Arguments

- data:

  Trial-level M3 data.

- score:

  Trial-level functional/trajectory score or its column name.

- pupil:

  Name of the output pupil column.

- provenance:

  Free-text derivation/provenance note.

## Value

An \`eye_multimodal_m3_functional_bridge\` data object.
