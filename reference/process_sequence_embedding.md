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
