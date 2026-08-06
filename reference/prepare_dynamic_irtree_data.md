# Prepare ordered transition data for dynamic IRTree models

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
prepare_dynamic_irtree_data(x, spec = dynamic_irtree_spec(),
  person = "participant_id", item = "item_id", trial = "trial_id", state = "state",
  time = NULL, from = "from_state", to = "to_state", states = NULL)
```

## Arguments

- x:

  An \`eye_dataset\` or data frame.

- spec:

  Dynamic IRTree specification.

- person:

  Column names for long state data.

- item:

  Column names for long state data.

- trial:

  Column names for long state data.

- state:

  Column names for long state data.

- time:

  Column names for long state data.

- from:

  Existing transition columns for transition-format data.

- to:

  Existing transition columns for transition-format data.

- states:

  Optional complete state vocabulary.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
