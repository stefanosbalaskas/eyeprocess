# Test-retest process reliability profile

Test-retest process reliability profile

## Usage

``` r
process_reliability_profile(data, person, session, measure)
```

## Arguments

- data:

  Long data.

- person, session, measure:

  Column names.

## Value

An object of class "eye_process_reliability_profile", stored as a named
list, with components "measure", "icc", "bland_altman", "caveat". It
contains test-retest process reliability profile and associated metadata
or diagnostics needed to interpret the result.
