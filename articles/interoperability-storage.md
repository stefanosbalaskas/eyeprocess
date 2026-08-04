# Interoperability, Eye-Tracking-BIDS, and storage

## Eye-Tracking-BIDS

``` r

export_eye_bids(
  dataset,
  "bids-eye-study",
  task = "reasoning",
  screen_distance_m = 0.60,
  screen_size_m = c(0.53, 0.30),
  overwrite = TRUE
)
roundtrip <- import_eye_bids("bids-eye-study")
validate_eye_dataset(roundtrip)
```

The exporter writes one physiological recording per eye, with
`timestamp`, `x_coordinate`, and `y_coordinate` first, optional
`pupil_size`, no TSV header, and required column metadata in the JSON
sidecar.

## Parquet and Arrow

``` r

handle <- write_eye_storage(dataset, "storage", format = "parquet", overwrite = TRUE)
handle
restored <- collect_eye_storage(handle)
```

## Package ecosystems

``` r

x <- as_eyeprocess_eyetools(external_object, mapping = declared_mapping)
proc <- as_procdata_sequence(x)
seqs <- as_traminer_sequence(x, create_object = TRUE)
hmm_data <- as_seqhmm_data(x)
```
