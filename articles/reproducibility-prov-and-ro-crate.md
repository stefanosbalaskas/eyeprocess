# Reproducibility fingerprints, provenance, and RO-Crate

eyeprocess 0.9 consolidates hashes of data, decisions, model
specifications, results, package versions, files, and the R environment
into a reproducibility fingerprint.

``` r

fp <- eye_reproducibility_fingerprint(data=data, analysis_spec=spec, decisions=manifest, result=fit)
write_reproducibility_fingerprint(fp, "fingerprint.rds")
verify_reproducibility_fingerprint(fp)
```

Lineage nodes and edges can be represented using PROV-like relation
labels, exported as compact JSON, or written as Graphviz DOT.
[`export_ro_crate_metadata()`](https://stefanosbalaskas.github.io/eyeprocess/reference/export_ro_crate_metadata.md)
writes minimal RO-Crate 1.3 metadata. The package intentionally
describes this as interoperability scaffolding rather than claiming full
external conformance without independent validation.
