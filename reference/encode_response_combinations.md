# Encode multiple-response item response combinations

Encode multiple-response item response combinations

## Usage

``` r
encode_response_combinations(
  data,
  person = "participant_id",
  item = "item_id",
  option = "option_id",
  selected = "selected",
  sort_options = TRUE,
  empty_code = "<none>"
)
```

## Arguments

- data:

  Long person-item-option table.

- person, item, option, selected:

  Column names.

- sort_options:

  Sort selected option labels before combining.

- empty_code:

  Code for no selected options.
