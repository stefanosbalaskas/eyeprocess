# Build an item-to-visual-context registry

Build an item-to-visual-context registry

## Usage

``` r
visual_context_registry(
  item_metadata,
  item = "item_id",
  context = NULL,
  context_candidates = c("visual_anchor_id", "stimulus_id", "stimulus_page", "page_id",
    "layout_id", "screen_id", "diagram_id"),
  min_items_per_context = 3L
)
```

## Arguments

- item_metadata:

  Item-level metadata.

- item:

  Item identifier column.

- context:

  Optional explicit context column.

- context_candidates:

  Candidate metadata columns searched in order.

- min_items_per_context:

  Minimum items required for a shared context.
