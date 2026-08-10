# Run a computational scaling benchmark

Run a computational scaling benchmark

## Usage

``` r
run_eye_benchmark(
  design = eye_benchmark_design(),
  generator = .ep09_default_benchmark_generator,
  operation = .ep09_default_benchmark_operation,
  gc_before = TRUE,
  progress = interactive()
)
```

## Arguments

- design:

  Benchmark design.

- generator:

  Function \`(n, row)\` returning benchmark input.

- operation:

  Function \`(data, row)\` representing the operation under test.

- gc_before:

  Run garbage collection before timing.

- progress:

  Print progress.
