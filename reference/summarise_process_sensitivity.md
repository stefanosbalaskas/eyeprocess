# Summarise process sensitivity results

Summarise process sensitivity results

## Usage

``` r
summarise_process_sensitivity(
  x,
  effect = "effect",
  p_value = NULL,
  threshold = 0,
  alpha = 0.05
)
```

## Arguments

- x:

  Sensitivity result.

- effect:

  Effect column.

- p_value:

  Optional p-value column.

- threshold:

  Optional substantive effect threshold.

- alpha:

  Significance threshold used only when \`p_value\` is supplied.
