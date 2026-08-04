# eyeprocess 0.2.0.9000 real Gazepoint empirical-validation milestone

This development milestone extends the validated 0.1.0.9003 baseline
with an adapter path derived from genuine Gazepoint Analysis 7.2.0
export structures.

## Empirical source structure

The private corpus contains:

- six `User *_all_gaze.csv` files;
- six paired `User *_fixations.csv` files;
- four `Data_Summary_export_*.csv` reports;
- two media items per recording;
- binocular pupil data for all six recordings;
- valid GSR/EDA, heart-rate, IBI, and engagement-dial streams for Users
  3–5.

## Implemented compatibility changes

- Current Gazepoint filename recognition and pair grouping.
- Participant/recording identity inference when `USER` is blank.
- Native `TIMETICK(f=10000000)` preservation and zero-based
  normalization.
- Separate retention of media-relative `TIME(...)` values.
- Media-scoped fixation identifiers because `FPOGID` restarts per media.
- Explicit raw GSR, microsiemens, tonic, phasic, HR, IBI, and dial
  channels.
- Validity propagation from `GSRV`, `HRV`, and `DIALV`.
- Multi-section Data Summary parsing.
- AOI definitions and participant-AOI feature extraction.
- Real-structure regression fixtures and tests.
- A private, ready-to-run validation corpus and Windows preparation
  script.

## Evidence boundary

The files were inspected and the adapter logic was built directly
against their structures. The package version remains a validation
candidate until it passes on Windows:

1.  the complete unit-test suite;
2.  `R CMD check` with 0 errors, 0 warnings, and 0 notes;
3.  [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html);
4.  the private real-corpus validation manifest.

No broader Gazepoint-version compatibility claim should be made until
additional independent exports have been tested.
