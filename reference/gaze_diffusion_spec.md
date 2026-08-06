# Define a gaze-informed diffusion model

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
gaze_diffusion_spec(response = "score", response_time = "response_time",
  participant = "participant_id", item = "item_id", drift_features = character(),
  boundary_features = character(), nondecision_features = character(),
  starting_features = character(), censor_column = NULL, contaminant = TRUE,
  engine = c("baseline", "stan", "ez_regression", "diffIRT", "brms"),
  gaze_features = NULL, chains = 4L, parallel_chains = min(4L, chains),
  iter_warmup = 1000L, iter_sampling = 1000L, adapt_delta = 0.97, max_treedepth = 13L)
```

## Arguments

- response:

  Binary response column.

- response_time:

  Response-time column in seconds.

- participant:

  Participant identifier.

- item:

  Item identifier.

- drift_features:

  Features assigned a priori to drift rate.

- boundary_features:

  Features assigned a priori to boundary separation.

- nondecision_features:

  Features assigned a priori to non-decision time.

- starting_features:

  Features assigned a priori to starting-point bias.

- censor_column:

  Optional censoring column with \`observed\`, \`left\`, or \`right\`.

- contaminant:

  Whether to estimate a uniform contaminant mixture.

- engine:

  Baseline approximation or Stan Wiener model.

- gaze_features:

  Value for \`gaze_features\`. See the function description and relevant
  article for constraints.

- chains:

  Stan controls.

- parallel_chains:

  Stan controls.

- iter_warmup:

  Stan controls.

- iter_sampling:

  Stan controls.

- adapt_delta:

  Stan controls.

- max_treedepth:

  Stan controls.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
