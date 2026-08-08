# Fit an event-time IRT reference workflow

The built-in Cox reference conditions on an already supplied theta and
is therefore an event-time \*measurement diagnostic\*, not a full
continuous-time latent-trait estimator. A validated exact implementation
can be supplied via \`external_engine\`.

## Usage

``` r
fit_event_time_irt(
  data,
  event_time = "event_time",
  event = "event",
  theta = "theta",
  person = "participant_id",
  item = "item_id",
  engine = c("cox_reference", "external"),
  external_engine = NULL,
  ...
)
```

## Arguments

- data:

  Long event/item data.

- event_time:

  Time-to-event column.

- event:

  Event indicator (1 event, 0 censored).

- theta:

  Supplied latent-trait column for the reference engine.

- person, item:

  Person and item identifiers.

- engine:

  \`cox_reference\` or \`external\`.

- external_engine:

  Function implementing a study-specific event-time IRT.

- ...:

  Additional arguments passed to the external engine.
