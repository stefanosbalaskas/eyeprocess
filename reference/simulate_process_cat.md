# Simulate a simple process-aware CAT policy

This is a design simulator, not a production testing engine.

## Usage

``` r
simulate_process_cat(
  item_bank,
  true_theta = 0,
  n_items = 10L,
  weights = c(response = 1, rt = 0, process = 0),
  burden_weight = 0,
  seed = 1
)
```

## Arguments

- item_bank:

  Value supplied to \`item_bank\`; see Details for its model-specific
  role.

- true_theta:

  Simulated true latent-trait value or values.

- n_items:

  Number of items.

- weights:

  Weights used to combine information components.

- burden_weight:

  Penalty applied to expected burden.

- seed:

  Random-number seed.
