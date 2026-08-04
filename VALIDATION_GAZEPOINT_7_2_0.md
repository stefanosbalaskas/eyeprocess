# Real Gazepoint Analysis 7.2.0 Validation

**Validation date:** 4 August 2026

**Validated package version:** `eyeprocess 0.3.0.9000`

## Corpus

A private, de-identified Gazepoint Analysis 7.2.0 validation corpus was used.
The raw exports and generated participant-level outputs are not included in
the public repository.

## Empirical workflow results

- Workflow status: PASS
- Recording groups: 6
- Participants: 6
- Reconstructed trials: 12
- Analysis-ready process rows: 12
- Gaze samples: 7,340
- Eye samples: 14,680
- Fixations: 337
- Biometric observations: 58,720
- AOI definitions: 3
- Workflow plots: 74
- Generated output artifacts: 131
- IRT status: process_ready_response_pending

## Workflow integrity checks

All seven checks passed:

1. Canonical dataset validation
2. Unique trial keys
3. One process row per trial
4. Native timestamp preservation
5. Coordinate-space registration
6. Provenance availability
7. Required output generation

## Package validation

- testthat: 212 passed, 0 failed, 0 warnings, 0 skipped
- R CMD check: 0 errors, 0 warnings, 0 notes

This validation establishes successful software execution and data-structure
handling for the supplied demonstration corpus. It does not establish
psychometric parameter recovery or substantive scientific validity.
