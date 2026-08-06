# Define structural-zero and allowed transition masks

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
structural_transition_mask(states, forbidden = NULL, allowed = NULL,
  allow_self = TRUE, structural_zeros = NULL, allowed_transitions = NULL)
```

## Arguments

- states:

  State labels.

- forbidden:

  Two-column data frame/matrix of forbidden from-to pairs.

- allowed:

  Two-column data frame/matrix of explicitly allowed pairs.

- allow_self:

  Whether self transitions are allowed by default.

- structural_zeros:

  Alias for \`forbidden\`.

- allowed_transitions:

  Alias for \`allowed\`.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
