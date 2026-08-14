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

## Packaged lifecycle closure

The 0.9 development surface is accompanied by a packaged lifecycle
registry covering every currently exported API symbol. Classification is
deliberately conservative: foundational front-door interfaces are
`core`; orchestration and governance interfaces are `workflow`;
specialist scientific interfaces are `advanced`; research-facing
interfaces with explicit provisional contracts are `experimental`; and
frontier or optional-engine interfaces may be `gated`.

Lifecycle status describes software-interface maturity and governance.
It does not establish construct validity, empirical adequacy, device
equivalence, or the scientific interpretation of gaze, pupil,
response-time, sequence, or psychometric measures.

``` r

lifecycle <- eyeprocess::eye_api_lifecycle()
inventory <- eyeprocess::eye_api_inventory()
audit <- eyeprocess::audit_eye_api(inventory, lifecycle)

eyeprocess::api_surface_summary(inventory)
#>        family       status Freq
#> 1          io     advanced    0
#> 2       model     advanced   46
#> 3        plot     advanced   91
#> 4  simulation     advanced   27
#> 5     summary     advanced   34
#> 6     utility     advanced  303
#> 7  validation     advanced   50
#> 8    workflow     advanced    7
#> 9          io         core   26
#> 10      model         core    0
#> 11       plot         core   19
#> 12 simulation         core    0
#> 13    summary         core    4
#> 14    utility         core  114
#> 15 validation         core   22
#> 16   workflow         core    0
#> 17         io experimental    0
#> 18      model experimental   32
#> 19       plot experimental    0
#> 20 simulation experimental    2
#> 21    summary experimental    2
#> 22    utility experimental   45
#> 23 validation experimental    7
#> 24   workflow experimental    1
#> 25         io        gated    0
#> 26      model        gated   11
#> 27       plot        gated    0
#> 28 simulation        gated    1
#> 29    summary        gated    0
#> 30    utility        gated    5
#> 31 validation        gated    2
#> 32   workflow        gated    0
#> 33         io     workflow   45
#> 34      model     workflow   15
#> 35       plot     workflow   10
#> 36 simulation     workflow    5
#> 37    summary     workflow   10
#> 38    utility     workflow  151
#> 39 validation     workflow   49
#> 40   workflow     workflow   13
audit
#> eyeprocess API lifecycle audit
#>   APIs             : 1149 
#>   reviewed fraction: 100.0% 
#>   invalid mappings : 0
```

The current registry is intentionally frozen to the current exported
surface. A future exported symbol that is absent from the registry
continues to resolve as `unreviewed`; this makes lifecycle review an
explicit release obligation rather than an inference from function
naming.
