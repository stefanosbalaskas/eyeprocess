# Low-dimensional embedding of response-process sequences

Uses TF-IDF-weighted n-gram features and truncated SVD. This is a
transparent classical embedding; sequence autoencoders can be supplied
later through an external engine without changing the downstream
contract.

## Usage

``` r
process_sequence_embedding(sequence, n = c(1L, 2L, 3L), dimensions = 5L)
```

## Arguments

- sequence:

  Sequence input.

- n:

  Requested count or n-gram order, depending on context.

- dimensions:

  Number of embedding dimensions.

## Value

An R object containing low-dimensional embedding of response-process
sequences. The concrete class and structure follow the selected method,
engine, or input object and are preserved as documented by that
workflow.
