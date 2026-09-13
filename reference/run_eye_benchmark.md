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

## Value

An object of class "eye_benchmark_result", stored as a named list, with
components "design", "results", "created_at", "status", "caveat". It
contains a computational scaling benchmark and associated metadata or
diagnostics needed to interpret the result.
