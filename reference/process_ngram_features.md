# N-gram features from process sequences

N-gram features from process sequences

## Usage

``` r
process_ngram_features(sequence, n = c(1L, 2L, 3L), separator = ">")
```

## Arguments

- sequence:

  Sequence input.

- n:

  Requested count or n-gram order, depending on context.

- separator:

  Sequence-token separator.

## Value

An object of class "matrix", stored as an R object, containing n-gram
features from process sequences and associated metadata needed to
interpret the result.
