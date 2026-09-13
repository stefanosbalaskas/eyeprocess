#!/usr/bin/env python3
"""Repository-wide audit for CRAN manual-review findings.

This intentionally performs static checks only. It prints stable, grep-friendly
records that can be inspected in GitHub Actions logs before any remediation is
applied.
"""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

TEXT_SUFFIXES = {".R", ".r", ".Rmd", ".rmd", ".qmd", ".Rd", ".md"}
SCAN_DIRS = ("R", "man", "vignettes", "inst", "demo", "examples")


def iter_text_files():
    for dirname in SCAN_DIRS:
        base = ROOT / dirname
        if not base.exists():
            continue
        for path in base.rglob("*"):
            if path.is_file() and path.suffix in TEXT_SUFFIXES:
                yield path


def rel(path: Path) -> str:
    return path.relative_to(ROOT).as_posix()


def scan_pattern(name: str, pattern: str):
    rx = re.compile(pattern)
    hits = []
    for path in iter_text_files():
        try:
            lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
        except OSError:
            continue
        for lineno, line in enumerate(lines, 1):
            if rx.search(line):
                hits.append((rel(path), lineno, line.strip()))
    print(f"{name}_COUNT={len(hits)}")
    for path, lineno, line in hits:
        print(f"{name}|{path}:{lineno}|{line}")
    return hits


def audit_rd_values():
    man = ROOT / "man"
    rd_files = sorted(man.glob("*.Rd")) if man.exists() else []
    missing = []
    with_usage = 0
    for path in rd_files:
        text = path.read_text(encoding="utf-8", errors="replace")
        if "\\usage{" not in text:
            continue
        with_usage += 1
        if "\\value{" not in text:
            missing.append(path)
    print(f"RD_FILES_TOTAL={len(rd_files)}")
    print(f"RD_FILES_WITH_USAGE={with_usage}")
    print(f"RD_MISSING_VALUE_COUNT={len(missing)}")
    for path in missing:
        print(f"RD_MISSING_VALUE|{rel(path)}")
    return missing


def audit_description_acronyms():
    path = ROOT / "DESCRIPTION"
    if not path.exists():
        print("DESCRIPTION_MISSING=1")
        return
    text = path.read_text(encoding="utf-8", errors="replace")
    desc_lines = []
    in_description = False
    for line in text.splitlines():
        if line.startswith("Description:"):
            in_description = True
            desc_lines.append(line.split(":", 1)[1].strip())
        elif in_description and (line.startswith(" ") or line.startswith("\t")):
            desc_lines.append(line.strip())
        elif in_description:
            break
    desc = " ".join(desc_lines)
    tokens = sorted(set(re.findall(r"\b[A-Z][A-Z0-9-]{1,}\b", desc)))
    print("DESCRIPTION_ACRONYM_CANDIDATES=" + ",".join(tokens))


def main():
    print("CRAN_POLICY_AUDIT_BEGIN")
    audit_description_acronyms()
    audit_rd_values()
    scan_pattern("INSTALL_PACKAGES", r"\binstall\.packages\s*\(")
    scan_pattern("GLOBALENV_LITERAL", r"\.GlobalEnv\b")
    scan_pattern("GLOBALENV_CALL", r"\bglobalenv\s*\(")
    scan_pattern("SUPERASSIGN", r"<<-")
    print("CRAN_POLICY_AUDIT_END")


if __name__ == "__main__":
    main()
