# eyeprocess 0.1.0.9003 regression-test alignment hotfix

This metadata-and-test-only patch aligns the validation-corpus regression test
with the idempotent, non-destructive behavior introduced in version 0.1.0.9002.

## Change

The obsolete expectation that a second call to `init_validation_corpus()` must
error on a non-empty directory has been replaced by assertions that repeated
initialization:

- returns the same corpus path;
- emits the documented already-initialized message;
- preserves the existing manifest;
- preserves the README; and
- preserves existing case files.

No importer, schema, preprocessing, plotting, modelling, or validation logic was
changed.
