# eyeprocess 0.3.0.9000: Integrated Gazepoint Downstream Workflow

This milestone converts the validated Gazepoint Analysis 7.2.0 adapter into a
complete research workflow.

## Workflow

```text
real Gazepoint folder
→ canonical eye_dataset
→ QC and recording diagnostics
→ media/trial reconstruction
→ fixation and AOI summaries
→ pupil and biometric processing
→ gaze/pupil/biometric plots
→ person × item × trial process tables
→ IRT-ready response/process structures
→ reproducible report and rerun assets
```

## Main API

- `gazepoint_workflow_spec()`
- `run_gazepoint_workflow()`
- `build_gazepoint_media_trials()`
- `derive_gazepoint_workflow_features()`
- `gazepoint_analysis_tables()`
- `gazepoint_irt_tables()`
- `plot_gazepoint_workflow()`
- `write_gazepoint_workflow_report()`
- `validate_gazepoint_workflow()`

## Scientific safeguards

- Media presentations become explicit trials; item labels may be mapped without
  altering native stimulus identifiers.
- Raw Gazepoint values are retained; only validity-flagged biometric values enter
  analysis features.
- Pupil baseline correction is disabled by default because media-onset data are
  not automatically a pre-stimulus baseline.
- Response templates are produced when responses are unavailable. No scores or
  IRT estimates are fabricated.
- The six-user corpus is a software-validation corpus, not a psychometric sample.
- The report repeats the package's interpretation boundaries for gaze, pupil,
  EDA, heart rate, latent process factors, and strategy labels.

## Validation status

The 0.2.0.9002 baseline passed the full Windows package gate and real Gazepoint
corpus. The 0.3.0.9000 workflow candidate has passed the available static source
audit and awaits the authoritative Windows runtime gate.
