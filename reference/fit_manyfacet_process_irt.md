# Many-facet process IRT reference model

Fits crossed random effects for available person, item, device, session,
site, algorithm, and AOI-definition facets. For a binary response this
is a generalized many-facet reference model; it is not marketed as a
FACETS software replica.

## Usage

``` r
fit_manyfacet_process_irt(
  data,
  response = "response",
  process = NULL,
  person = "participant_id",
  item = "item_id",
  device = NULL,
  session = NULL,
  site = NULL,
  algorithm = NULL,
  aoi_definition = NULL,
  process_family = c("gaussian", "poisson", "negative_binomial")
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- process:

  Process variable or column name.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- device:

  Device identifier or device facet.

- session:

  Session identifier or session facet.

- site:

  Site identifier or site facet.

- algorithm:

  Algorithm identifier or algorithm facet.

- aoi_definition:

  Value supplied to \`aoi_definition\`; see Details for its
  model-specific role.

- process_family:

  Distributional family for the process channel.
