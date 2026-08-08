# Select the next item using response/process utility

Select the next item using response/process utility

## Usage

``` r
select_next_item_process(
  theta,
  item_bank,
  used = character(),
  weights = c(response = 1, rt = 0, process = 0),
  burden_weight = 0
)
```

## Arguments

- theta:

  Latent-trait values.

- item_bank:

  Value supplied to \`item_bank\`; see Details for its model-specific
  role.

- used:

  Items already used or unavailable for selection.

- weights:

  Weights used to combine information components.

- burden_weight:

  Penalty applied to expected burden.
