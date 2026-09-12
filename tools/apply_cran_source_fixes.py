#!/usr/bin/env python3
"""Apply deterministic source fixes for the CRAN 0.11.1 manual review.

This script is intentionally narrow and fail-closed: every replacement asserts
its expected occurrence count so repository drift cannot silently produce a
partial remediation.
"""

from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def replace_exact(path: str, old: str, new: str, expected: int) -> None:
    p = ROOT / path
    text = p.read_text(encoding="utf-8")
    count = text.count(old)
    if count != expected:
        raise SystemExit(f"{path}: expected {expected} occurrences, found {count}")
    p.write_text(text.replace(old, new), encoding="utf-8")
    print(f"PATCHED|{path}|{count}")


def remove_exact(path: str) -> None:
    p = ROOT / path
    if not p.exists():
        raise SystemExit(f"{path}: expected file is missing")
    p.unlink()
    print(f"REMOVED|{path}")


def main() -> None:
    common_rng = '''  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({ if (is.null(old)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old, envir = .GlobalEnv) }, add = TRUE)
  set.seed(seed)'''
    replace_exact(
        "R/085-irt-scoring-adaptive-0-9.R",
        common_rng,
        "  .eye_local_seed(seed)",
        1,
    )
    replace_exact(
        "R/089-irt-validation-evidence-0-9.R",
        common_rng,
        "  .eye_local_seed(seed)",
        4,
    )

    multimodal_rng = '''    old <- .Random.seed_exists <- exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
    if (.Random.seed_exists) old_seed <- get(".Random.seed", envir=.GlobalEnv)
    on.exit({
        if (.Random.seed_exists) assign(".Random.seed", old_seed, envir=.GlobalEnv)
        else if (exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))
            rm(".Random.seed", envir=.GlobalEnv)
    }, add=TRUE)
    set.seed(seed)'''
    replace_exact(
        "R/096-multimodal-simulation-0-10.R",
        multimodal_rng,
        "    .eye_local_seed(seed)",
        1,
    )

    replace_exact(
        "R/019-gazepoint-downstream-workflow.R",
        "envir = new.env(parent = globalenv())",
        "envir = new.env(parent = baseenv())",
        1,
    )
    replace_exact(
        "inst/scripts/run-validation-chunk.R",
        "workflow <- new.env(parent = globalenv())",
        'workflow <- new.env(parent = asNamespace("eyeprocess"))',
        1,
    )
    replace_exact(
        "inst/scripts/run-complete-validation-program.R",
        "environment <- new.env(parent = globalenv())",
        'environment <- new.env(parent = asNamespace("eyeprocess"))',
        1,
    )

    launchers = (
        "inst/validation/0.9-m2/scripts/run-recovery.R",
        "inst/validation/0.9-m2/scripts/run-negative-controls.R",
        "inst/validation/0.9-m2/scripts/run-sbc.R",
        "inst/validation/0.9-m2/scripts/run-stress.R",
        "inst/validation/0.9-m2/scripts/freeze-evidence.R",
    )
    for path in launchers:
        replace_exact(
            path,
            "sys.source(main, envir = globalenv())",
            'validation_env <- new.env(parent = asNamespace("eyeprocess"))\nsys.source(main, envir = validation_env)',
            1,
        )

    remove_exact("inst/scripts/install-eyeprocess.R")


if __name__ == "__main__":
    main()
