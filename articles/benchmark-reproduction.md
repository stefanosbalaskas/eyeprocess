# Public benchmark and software-paper reproduction

## Bundled multimodal study

The package includes a compact, fully synthetic and openly
redistributable benchmark containing participant and item metadata,
binary responses, response times, recordings, gaze samples, events,
AOIs, pupil trajectories, quality indicators, and provenance.

``` r

library(eyeprocess)
#> eyeprocess 0.8.0.9000: vendor-neutral eye/process data harmonization with first-class Gazepoint support.
study <- eyeprocess_benchmark_study()
study
#> eyeprocess public benchmark study
#> Path:   /home/runner/work/_temp/Library/eyeprocess/extdata/benchmark-study
#> Files:  10
#> Status: synthetic, openly redistributable benchmark - not empirical vendor validation
```

The benchmark tests contracts and reproducibility. It is not real
participant data, vendor compatibility evidence, or scientific
validation of advanced models.

## Integrity and expected outputs

``` r

validation <- validate_benchmark_study(study)
validation$valid
#> [1] TRUE

reproduction <- run_benchmark_reproduction(study)
reproduction$comparison
#>                 metric     observed     expected tolerance absolute_error
#> 1             accuracy    0.5520833    0.5520833     1e-10   0.000000e+00
#> 2            aoi_count    3.0000000    3.0000000     0e+00   0.000000e+00
#> 3         gaze_samples 4800.0000000 4800.0000000     0e+00   0.000000e+00
#> 4                items    8.0000000    8.0000000     0e+00   0.000000e+00
#> 5           mean_pupil    3.2497872    3.2497872     1e-10   4.440892e-16
#> 6   mean_response_time    1.3480609    1.3480609     1e-10   0.000000e+00
#> 7         participants   12.0000000   12.0000000     0e+00   0.000000e+00
#> 8        pupil_samples 3840.0000000 3840.0000000     0e+00   0.000000e+00
#> 9         quality_rows   96.0000000   96.0000000     0e+00   0.000000e+00
#> 10              trials   96.0000000   96.0000000     0e+00   0.000000e+00
#> 11 valid_gaze_fraction    0.9637500    0.9637500     1e-10   0.000000e+00
#>    passed
#> 1    TRUE
#> 2    TRUE
#> 3    TRUE
#> 4    TRUE
#> 5    TRUE
#> 6    TRUE
#> 7    TRUE
#> 8    TRUE
#> 9    TRUE
#> 10   TRUE
#> 11   TRUE
```

All source files are fingerprinted. Relational checks ensure that
participants, items, trials, events, gaze, and pupil records agree.
Expected scalar outputs use explicit numerical tolerances.

## Data dictionary and reproduction directory

``` r

write_benchmark_data_dictionary(study, "benchmark-data-dictionary.md")
write_software_paper_reproduction("software-paper-reproduction", study)
```

The reproduction scaffold contains copied benchmark data, an executable
R script, expected outputs, a README, and a file/session manifest.

## Release audit

``` r

audit_benchmark_release(study)$ready
#> [1] TRUE
```

A public empirical benchmark can later be added under an independently
reviewed licence. It must remain distinct from this synthetic test
asset.
