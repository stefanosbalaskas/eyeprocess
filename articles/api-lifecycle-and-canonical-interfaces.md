# API lifecycle and canonical interfaces

A large scientific package needs a navigable public surface. The 0.9
lifecycle layer inventories exports, maps them to conceptual families,
and permits explicit statuses such as core, workflow, advanced,
experimental, gated, compatibility, and deprecated. Unreviewed functions
remain `unreviewed`; eyeprocess does not infer maturity from a name
alone.

``` r

inv <- eye_api_inventory()
reg <- eye_api_lifecycle()
reg <- register_eye_api_status(reg, "run_eye_pipeline", "workflow", canonical="run_eye_pipeline")
audit <- audit_eye_api(inv, reg)
api_surface_summary(audit$table)
eye_api_recommendation(audit)
```

The purpose is staged consolidation. Version 0.9 does not aggressively
remove established interfaces.
