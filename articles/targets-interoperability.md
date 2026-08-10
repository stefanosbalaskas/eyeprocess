# Interoperability with targets-style workflows

eyeprocess pipelines and targets solve different but complementary
problems. The eyeprocess object records scientific decisions and process
lineage; a targets workflow can orchestrate file/function dependencies
and incremental execution.

``` r

man <- eye_targets_manifest(pipeline)
write_eye_targets_template(pipeline, "_targets.R")
```

The generated file is intentionally a template with explicit
placeholders. eyeprocess does not silently rewrite arbitrary step
closures into executable targets expressions because such translation
can change semantics or hide undeclared dependencies.
