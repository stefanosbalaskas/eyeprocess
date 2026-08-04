# eyeprocess architecture

## Layers

1. **Adapters** detect and parse vendor exports.
2. **Canonical relational data** preserve recordings, streams, samples, eyes,
   episodes, events, intervals, responses, coordinate spaces, AOIs, biometrics,
   calibrations, features, quality, and provenance.
3. **Transformations** operate only through declared coordinate, clock,
   preprocessing, trial, and AOI specifications.
4. **Analysis** derives gaze, pupil, response-time, biometric, scanpath, and
   transition features.
5. **Psychometrics** prepares linked person-item-trial inputs and delegates to
   mature optional engines or explicitly marked experimental models.

## Dependency direction

```text
gp3tools ---------\
                   > eyeprocess core -> optional model engines
gpbiometrics -----/
```

`gp3tools` and `gpbiometrics` are optional bridges. They do not import
`eyeprocess`, preventing circular package dependencies.

## Invariants

- Native timestamps and transformed seconds coexist.
- Coordinate spaces and units are always explicit.
- Raw, vendor-derived, and package-derived records remain distinguishable.
- Different streams are never silently forced onto one sampling grid.
- Unsupported fields remain available in raw/vendor metadata.
- Transformations and exclusions are recorded in provenance.
