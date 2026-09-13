# Bland-Altman repeatability summary for two sessions

Bland-Altman repeatability summary for two sessions

## Usage

``` r
process_bland_altman(data, person, session, measure, sessions = NULL)
```

## Arguments

- data:

  Long data.

- person:

  Participant column.

- session:

  Session column containing exactly two selected sessions.

- measure:

  Measure column.

- sessions:

  Optional two session labels.

## Value

An object of class "eye_process_bland_altman", stored as a named list,
with components "pairs", "summary", "sessions". It contains bland-Altman
repeatability summary for two sessions and associated metadata or
diagnostics needed to interpret the result.
