# AOI Robustness Reporting Bundle

## Purpose

A sensitivity result should be reviewable as an evidence package, not
only as a narrative sentence. Keep the prespecified plan, geometry
audit, assignment summaries, model outputs, failures, provenance, report
text, and diagnostic figures together.

## Recommended bundle

- `analysis-plan.yml` — declared perturbation/model/failure plan;
- `perturbation-audit.csv` — branch completion and geometry failures;
- `assignment-stability.csv` — measurement-level robustness;
- `assignment-table.csv` — observation-level branch assignments;
- `model-results.csv` — branch coefficients, intervals, convergence, and
  N;
- `inference-stability.csv` — term-level robustness summary;
- `failures.csv` — retained non-evaluable branches;
- `provenance.txt` or JSON — source/AOI hashes and analysis provenance;
- `report.md` — manuscript-oriented narrative;
- diagnostic figures and a file manifest.

## Export from a result

``` r

out_dir <- "workflow-output/aoi-reporting-bundle"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(result$grid_result$audit,
                 file.path(out_dir, "perturbation-audit.csv"), row.names = FALSE)
utils::write.csv(result$stability$overall,
                 file.path(out_dir, "assignment-stability.csv"), row.names = FALSE)
utils::write.csv(result$assignment_table,
                 file.path(out_dir, "assignment-table.csv"), row.names = FALSE)
utils::write.csv(result$models,
                 file.path(out_dir, "model-results.csv"), row.names = FALSE)
utils::write.csv(
  assess_aoi_inference_stability(result, term = "condition"),
  file.path(out_dir, "inference-stability.csv"), row.names = FALSE
)
utils::write.csv(result$failures,
                 file.path(out_dir, "failures.csv"), row.names = FALSE)
writeLines(report_aoi_sensitivity(result), file.path(out_dir, "report.md"))
```

If a cryptographic manifest is required, create it with the repository
or archival tooling used by the project and record the hashing
algorithm. Do not add a package dependency solely to hash this example.

## Reviewer-facing reading order

1.  analysis plan;
2.  perturbation/failure audits;
3.  assignment stability and figures;
4.  model results, convergence, intervals, and N;
5.  provenance;
6.  narrative report.

## Interpretation

The bundle demonstrates what was run and what remained stable within the
declared AOI alternatives. It does not prove that the nominal AOIs are
scientifically correct or that the perturbation set captures every
source of measurement uncertainty.

## Privacy and limitations

Observation-level assignment tables can contain participant/trial
identifiers. Review them before external release. Geometry sensitivity
also does not replace calibration-quality, event-detector, missingness,
or estimator-specific diagnostics.

## API map

Use
[`run_aoi_sensitivity_analysis()`](https://stefanosbalaskas.github.io/eyeprocess/reference/aoi_perturbation_uncertainty.md)
for the core result,
[`assess_aoi_inference_stability()`](https://stefanosbalaskas.github.io/eyeprocess/reference/aoi_perturbation_uncertainty.md)
for model-level robustness,
[`report_aoi_sensitivity()`](https://stefanosbalaskas.github.io/eyeprocess/reference/aoi_perturbation_uncertainty.md)
for the narrative draft, and the four AOI plotting functions for visual
evidence.
