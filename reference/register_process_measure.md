# Add a process measure to a registry without global mutation

Add a process measure to a registry without global mutation

## Usage

``` r
register_process_measure(
  registry = process_measure_registry(),
  name,
  channel,
  unit,
  level,
  interpretation,
  guardrail,
  status = "user_defined"
)
```

## Arguments

- registry:

  Registry.

- name, channel, unit, level, interpretation, guardrail, status:

  Measure metadata.
