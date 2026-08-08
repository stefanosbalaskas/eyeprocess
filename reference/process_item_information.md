# 2PL response item information

2PL response item information

## Usage

``` r
process_item_information(
  theta,
  a,
  b,
  process_information = 0,
  rt_information = 0,
  weights = c(response = 1, rt = 0, process = 0),
  expected_time = 0,
  burden_weight = 0
)
```

## Arguments

- theta:

  Latent-trait values.

- a:

  Item discrimination parameter or parameters.

- b:

  Item difficulty/location parameter or parameters.

- process_information:

  Information supplied by the process channel.

- rt_information:

  Information supplied by response time.

- weights:

  Weights used to combine information components.

- expected_time:

  Expected response time or burden.

- burden_weight:

  Penalty applied to expected burden.
