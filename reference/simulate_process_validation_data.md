# Simulate a generic multimodal validation dataset with known truth

This simulator is a neutral software-validation fixture, not a
substantive psychological data-generating model. The generated process
channels should not be interpreted as mental-state measurements.

## Usage

``` r
simulate_process_validation_data(
  condition,
  replication = 1L,
  seed = NULL,
  beta = 0.35
)
```

## Arguments

- condition:

  One-row validation condition.

- replication:

  Replication index.

- seed:

  Optional seed override.

- beta:

  Known effect of \`x\` on the generic process outcome.

## Value

List with \`data\` and \`truth\`.
