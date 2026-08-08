# Fit one model-validation replicate

Fit one model-validation replicate

## Usage

``` r
fit_validation_replicate(
  replicate,
  generator,
  fitter,
  extractor,
  scenario = "baseline",
  engine = "unspecified"
)
```

## Arguments

- replicate:

  Replicate id.

- generator:

  Function \`(replicate, scenario)\` returning simulated data; the
  simulation should expose truth via \`extract_parameter_truth()\`.

- fitter:

  Function accepting the simulated data (or its \`\$data\` member).

- extractor:

  Function \`(fit, simulation)\` returning estimates with at least
  \`parameter\` and \`estimate\`; optional \`lower\`/\`upper\` are
  retained.

- scenario:

  Scenario label/object passed to the generator.

- engine:

  Engine label.
