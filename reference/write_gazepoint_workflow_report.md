# Write a reproducible Gazepoint workflow report

Write a reproducible Gazepoint workflow report

## Usage

``` r
write_gazepoint_workflow_report(
  workflow,
  path = file.path(workflow$output_dir, "gazepoint-workflow-report.md"),
  render_html = workflow$spec$create_html_report
)
```

## Arguments

- workflow:

  An \`eye_gazepoint_workflow\` result.

- path:

  Markdown report destination.

- render_html:

  Render an HTML copy when possible.

## Value

The normalized report path.
