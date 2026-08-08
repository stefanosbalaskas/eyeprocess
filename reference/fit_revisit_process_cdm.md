# Fit a revisiting-aware cognitive-diagnosis process workflow

Convenience adapter motivated by current work combining response time
and item revisiting with cognitive diagnosis. The response layer is
delegated to \`fit_cognitive_diagnosis_process()\`; revisiting/RT/gaze
remain process evidence and do not redefine attributes or the Q-matrix.

## Usage

``` r
fit_revisit_process_cdm(
  response_matrix,
  q_matrix,
  process_data,
  person_id = "participant_id",
  revisited = "revisited",
  rt = "response_time",
  gaze = NULL,
  ...
)
```

## Arguments

- response_matrix:

  Person-by-item responses.

- q_matrix:

  Item-by-attribute Q-matrix.

- process_data:

  Long process data.

- person_id:

  Person identifier in \`process_data\`.

- revisited:

  Revisit indicator/count column.

- rt:

  Response-time column.

- gaze:

  Optional gaze process columns.

- ...:

  Passed to \`fit_cognitive_diagnosis_process()\`.
