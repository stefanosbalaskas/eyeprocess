# CRAN remediation report

Generated automatically from the remediation branch.

- `addm_glam_proxy_features`: An object of class "eye_decision_process_proxy", stored as a named list, with components "features", "by", "status", "caveat". It contains aDDM/GLAM-inspired gaze-evidence proxy features and associated metadata or diagnostics needed to interpret the result.
- `adjust_pupil_confounds`: An R object containing confound-adjusted pupil values. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
- `algorithm_facet_effects`: An object of class "eye_process_facet_effects", stored as a named list, with components "facet", "column", "channel", "random_effects", "variance_component". It contains algorithm facet effects and associated metadata or diagnostics needed to interpret the result.
- `analysis_environment_snapshot`: A named list with components "r_version", "platform", "os", "locale", "timezone", "packages", containing snapshot an eyeprocess analysis environment and associated metadata or diagnostics.
- `analysis_resolution_guard`: A named list with components "expected_samples", "temporal_ok", "spatial_error_fraction", "spatial_ok", "min_samples", "max_error_fraction", "overall", "caveat", containing compatibility between measurement resolution and an analysis target and associated metadata or diagnostics.
- `aoi_membership_probability`: A tabular R object containing aOI membership probabilities from uncertainty draws; rows represent analysis units and columns contain the returned quantities.
- `validation_condition_ranking`: A tabular R object containing rank validation conditions by a transparent robustness score; rows represent analysis units and columns contain the returned quantities.
- `validation_replication_budget`: A numeric value or vector containing a replication budget from a target MCSE.
- `validation_robustness_score`: A single numeric robustness score: the mean finite condition-level robustness score, or `NA_real_` when no finite score is available.

## Inventory

- Generated `.Rd` files with `\usage`: **954**
- Usage-bearing generated `.Rd` files missing `\value`: **0**
- `RETURN_V2_INVENTORY=630`
- `RETURN_V2_POLYMORPHIC=158`
- `RETURN_V2_COUNTS=character/terminal-call:10,classed/terminal-dynamic-class:6,data.frame/terminal:78,data.frame/terminal-class:29,data.frame/terminal-subset:4,data.frame/wrapper->terminal:4,data.frame/wrapper->terminal-class:5,factor/terminal:1,list/terminal:26,list/terminal-class:166,list/wrapper->terminal-class:13,list/wrapper->terminal-dynamic-class:9,list/wrapper->wrapper->terminal-class:4,logical/terminal:10,logical/terminal-call:5,logical/terminal-expression:15,logical/wrapper->terminal-expression:1,matrix/terminal:3,numeric/terminal-call:16,numeric/terminal-expression:10,object/conditional:2,object/definition-missing:12,object/terminal-call-polymorphic:1,object/terminal-subset:4,object/terminal-symbol:38,object/unresolved:74,object/wrapper->assignment-cycle:3,object/wrapper->terminal-symbol:7,object/wrapper->unresolved:12,object/wrapper->wrapper->assignment-cycle:1,object/wrapper->wrapper->terminal-symbol:1,object/wrapper->wrapper->unresolved:3,tabular/terminal-call:23,tabular/terminal-subset:19,tabular/wrapper->terminal-call:10,tabular/wrapper->terminal-subset:1,vector-or-matrix/terminal-call:4`
