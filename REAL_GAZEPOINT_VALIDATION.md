# Real Gazepoint Analysis 7.2.0 validation corpus

This private development corpus was constructed from the de-identified
`gp3_test_exports` files supplied for empirical adapter validation. It
must remain outside the public package repository.

## Corpus structure

- Six `User *_all_gaze.csv` sample exports.
- Six paired `User *_fixations.csv` vendor-fixation exports.
- Four multi-section `Data_Summary_export_*.csv` AOI reports.
- Two media items per user.
- Native `TIMETICK(f=10000000)` and media-relative `TIME(...)` clocks.
- Gaze, binocular pupil, blink, saccade, AOI, TTL, and video-frame
  fields.
- Valid GSR/EDA, heart-rate, IBI, and engagement-dial streams for Users
  3–5.

## Observed profile

| participant_label | sample_rows | fixation_rows | media_items | recording_elapsed_seconds | estimated_sampling_rate_hz | combined_gaze_valid_percent | left_pupil_valid_percent | right_pupil_valid_percent | biometrics_valid | aoi_labeled_samples |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|:---|---:|
| User 0 | 1223 | 54 | 2 | 20.0758 | 60.871 | 97.547 | 97.465 | 97.302 | False | 131 |
| User 1 | 1222 | 52 | 2 | 20.0589 | 60.831 | 98.527 | 98.527 | 98.527 | False | 167 |
| User 2 | 1220 | 57 | 2 | 20.0074 | 60.868 | 95.164 | 95.082 | 95.164 | False | 85 |
| User 3 | 1227 | 56 | 2 | 20.1418 | 60.856 | 97.229 | 97.066 | 97.229 | True | 197 |
| User 4 | 1222 | 56 | 2 | 20.0597 | 60.844 | 94.19 | 94.108 | 74.877 | True | 172 |
| User 5 | 1226 | 62 | 2 | 20.1251 | 60.825 | 97.961 | 97.961 | 92.251 | True | 246 |

## Adapter requirements revealed by the corpus

1.  `_all_gaze.csv` and `_fixations.csv` must be recognized before
    biometric-column heuristics are applied.
2.  The blank `USER` column requires participant identity to be inferred
    from filenames.
3.  `TIME(...)` resets for each media item, whereas
    `TIMETICK(f=10000000)` remains monotonic across the recording.
4.  `FPOGID` restarts for each media item, so fixation identifiers must
    be namespaced by stimulus.
5.  `Data_Summary_export_*.csv` is a multi-section report rather than a
    rectangular CSV.
6.  `GSR_US`, `GSR_US_TONIC`, and `GSR_US_PHASIC` are conductance
    channels distinct from raw `GSR`.
7.  Gazepoint `IBI` values in these exports are expressed in seconds.

## Evidence status

The dedicated six-recording Gazepoint Analysis 7.2.0 validation and the
complete private corpus workflow both passed on Windows under version
0.2.0.9000. The subsequent full package gate exposed two legacy
synthetic-test compatibility defects, corrected in 0.2.0.9001. A fresh
full package gate and real-corpus rerun remain required for the hotfix
candidate.

## 0.2.0.9002 package-check status

The dedicated six-recording real-export workflow and complete private
corpus remain PASS. Version 0.2.0.9001 also passed all unit tests and
substantive package checks; its only `R CMD check` finding was a
portable-filename WARNING for two bundled fixture paths containing
spaces. Version 0.2.0.9002 renamed those packaged fixtures without
changing the empirical adapter behavior and subsequently passed the
complete Windows runtime gate with 0 errors, 0 warnings, and 0 notes.
Version 0.3.0.9000 builds the integrated downstream workflow on this
validated baseline.
