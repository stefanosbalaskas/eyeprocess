#!/usr/bin/env python3
"""Generate durable roxygen @return documentation for CRAN remediation.

The script uses generated Rd files as the inventory, locates the corresponding
roxygen block in its declared R source file, infers the returned base/classed
structure from the function body, and can update both source and generated Rd.
It intentionally refuses low-confidence cases so they can be reviewed rather
than filled with meaningless boilerplate.
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


@dataclass
class Item:
    rd: Path
    source: Path | None
    name: str
    title: str
    return_text: str | None
    confidence: str


def rd_field(text: str, field: str) -> str:
    m = re.search(r"\\" + re.escape(field) + r"\{([^{}]*)\}", text, re.S)
    return re.sub(r"\s+", " ", m.group(1)).strip() if m else ""


def source_from_rd(text: str) -> Path | None:
    m = re.search(r"^% Please edit documentation in (R/[^\r\n]+)$", text, re.M)
    return ROOT / m.group(1).strip() if m else None


def find_definition(lines: list[str], name: str) -> int | None:
    candidates = [name]
    # Rd method names may occasionally be represented using \method syntax in
    # usage but the \name field normally matches the source symbol.
    for symbol in candidates:
        escaped = re.escape(symbol)
        rx = re.compile(r"^\s*(?:`" + escaped + r"`|" + escaped + r")\s*(?:<-|=)\s*function\s*\(")
        for i, line in enumerate(lines):
            if rx.search(line):
                return i
    return None


def function_slice(lines: list[str], start: int) -> str:
    # Roxygen blocks delimit nearly every top-level public function in this
    # package. Keep the complete body until the next roxygen block.
    end = len(lines)
    for i in range(start + 1, len(lines)):
        if lines[i].startswith("#'"):
            end = i
            break
    return "\n".join(lines[start:end])


def roxygen_block(lines: list[str], def_i: int) -> tuple[int, int] | None:
    i = def_i - 1
    while i >= 0 and not lines[i].startswith("#'"):
        if lines[i].strip():
            return None
        i -= 1
    if i < 0:
        return None
    end = i + 1
    while i >= 0 and lines[i].startswith("#'"):
        i -= 1
    return i + 1, end


def quoted_classes(body: str) -> list[str]:
    classes: list[str] = []
    for m in re.finditer(r"\bclass\s*=\s*(c\s*\((.*?)\)|(['\"])(.*?)\3)", body, re.S):
        raw = m.group(2) if m.group(2) is not None else m.group(4)
        found = re.findall(r"['\"]([^'\"]+)['\"]", raw) if m.group(2) is not None else [raw]
        for value in found:
            if value and value not in classes:
                classes.append(value)
    return classes


def matching_paren(text: str, open_i: int) -> int | None:
    depth = 0
    quote = None
    escape = False
    for i in range(open_i, len(text)):
        ch = text[i]
        if quote:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
        elif ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return i
    return None


def named_components(body: str) -> list[str]:
    # Prefer the last structure(list(...)) or bare list(...) associated with a
    # returned object and extract top-level named components.
    starts = [m.start() for m in re.finditer(r"\blist\s*\(", body)]
    if not starts:
        return []
    start = starts[-1]
    open_i = body.find("(", start)
    close_i = matching_paren(body, open_i)
    if close_i is None:
        return []
    inner = body[open_i + 1 : close_i]
    parts = []
    current = []
    depth = 0
    quote = None
    escape = False
    for ch in inner:
        if quote:
            current.append(ch)
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            continue
        if ch in "'\"":
            quote = ch
            current.append(ch)
        elif ch in "([{":
            depth += 1
            current.append(ch)
        elif ch in ")]}":
            depth = max(0, depth - 1)
            current.append(ch)
        elif ch == "," and depth == 0:
            parts.append("".join(current).strip())
            current = []
        else:
            current.append(ch)
    if current:
        parts.append("".join(current).strip())
    names = []
    for part in parts:
        m = re.match(r"([A-Za-z.][A-Za-z0-9._]*)\s*=", part)
        if m and m.group(1) not in names:
            names.append(m.group(1))
    return names


def result_phrase(title: str, name: str) -> str:
    title = title.strip().rstrip(".")
    if not title:
        return name.replace("_", " ") + " result"
    for verb in (
        "Compute ", "Calculate ", "Estimate ", "Derive ", "Build ", "Create ",
        "Construct ", "Generate ", "Fit ", "Run ", "Perform ", "Apply ",
        "Assess ", "Audit ", "Evaluate ", "Validate ", "Check ", "Inspect ",
        "Summarize ", "Summarise ", "Compare ", "Convert ", "Transform ",
        "Import ", "Read ", "Export ", "Write ", "Prepare ", "Make ",
    ):
        if title.startswith(verb):
            rest = title[len(verb):]
            return rest[:1].lower() + rest[1:]
    return title[:1].lower() + title[1:]


def infer_return(name: str, title: str, body: str) -> tuple[str | None, str]:
    phrase = result_phrase(title, name)
    classes = quoted_classes(body)
    components = named_components(body)

    if name.startswith("print."):
        return "Invisibly returns the input object after printing its user-facing summary; the object's class and contents are unchanged.", "method"

    if name.startswith("plot."):
        if "ggplot" in body or "ggplot2::" in body:
            return "A ggplot object representing " + phrase + ". Printing the result draws the plot.", "method"
        if "invisible(" in body:
            return "Invisibly returns the data or object used to construct " + phrase + "; the primary side effect is drawing on the active graphics device.", "method"
        return "No meaningful return value; the function is called for the side effect of drawing " + phrase + " on the active graphics device.", "method"

    if classes:
        class_text = ", ".join('"' + x + '"' for x in classes[:4])
        if components:
            shown = components[:10]
            comp_text = ", ".join('"' + x + '"' for x in shown)
            if len(components) > len(shown):
                comp_text += ", and additional documented components"
            return (
                "An object of class " + class_text + ", stored as a named list with components " +
                comp_text + ". It contains " + phrase + " together with the associated metadata, diagnostics, or provenance needed to interpret the result."
            ), "classed-list"
        return (
            "An object of class " + class_text + " containing " + phrase +
            " and the associated metadata needed to interpret the result."
        ), "classed"

    # Side-effect-only functions are recognizable by a terminal invisible(NULL)
    # or explicit NULL without a richer returned structure.
    if re.search(r"invisible\s*\(\s*NULL\s*\)\s*\}?\s*$", body, re.S):
        return "No return value; called for side effects while producing " + phrase + ".", "side-effect"

    # Base structures. These are deliberately phrased conservatively: the
    # function title provides meaning, while the base type provides structure.
    if re.search(r"\bdata\.frame\s*\(", body) or re.search(r"\bas\.data\.frame\s*\(", body):
        return "A data frame containing " + phrase + "; columns contain the computed quantities and identifiers described in the function documentation.", "data.frame"
    if re.search(r"\bmatrix\s*\(", body) or re.search(r"\bas\.matrix\s*\(", body):
        return "A matrix containing " + phrase + ", with rows and columns corresponding to the analysis units described in the function documentation.", "matrix"
    if re.search(r"\blist\s*\(", body):
        if components:
            comp_text = ", ".join('"' + x + '"' for x in components[:10])
            return "A named list with components " + comp_text + ", containing " + phrase + " and supporting metadata or diagnostics.", "list"
        return "A list containing " + phrase + " and supporting metadata or diagnostics.", "list"

    # Strong scalar/vector clues.
    if re.search(r"\b(?:isTRUE|is\.na|is\.finite|grepl|identical|all|any)\s*\(", body) and "return(" not in body:
        return "A logical value indicating the result of " + phrase + ".", "logical"
    if re.search(r"\b(?:as\.character|paste0?|sprintf)\s*\(", body) and not re.search(r"\b(?:data\.frame|list|matrix)\s*\(", body):
        return "A character vector containing " + phrase + ".", "character"
    if re.search(r"\b(?:as\.numeric|numeric|rowMeans|colMeans)\s*\(", body) and not re.search(r"\b(?:data\.frame|list|matrix)\s*\(", body):
        return "A numeric value or vector containing " + phrase + ".", "numeric"

    return None, "unresolved"


def rd_escape(text: str) -> str:
    return text.replace("\\", "\\\\").replace("%", "\\%").replace("{", "\\{").replace("}", "\\}")


def inventory() -> list[Item]:
    items: list[Item] = []
    for rd in sorted((ROOT / "man").glob("*.Rd")):
        text = rd.read_text(encoding="utf-8", errors="replace")
        if "\\docType{data}" in text or "\\usage{" not in text or "\\value{" in text:
            continue
        name = rd_field(text, "name") or rd.stem
        title = rd_field(text, "title") or name.replace("_", " ")
        source = source_from_rd(text)
        ret = None
        conf = "unresolved"
        if source and source.exists():
            lines = source.read_text(encoding="utf-8", errors="replace").splitlines()
            def_i = find_definition(lines, name)
            if def_i is not None:
                ret, conf = infer_return(name, title, function_slice(lines, def_i))
        items.append(Item(rd, source, name, title, ret, conf))
    return items


def apply(items: list[Item]) -> None:
    source_insertions: dict[Path, list[tuple[int, str]]] = {}
    unresolved = [x for x in items if not x.return_text]
    if unresolved:
        raise SystemExit(
            "Refusing to apply: %d low-confidence return descriptions remain. Run without --apply and review UNRESOLVED records." % len(unresolved)
        )

    seen_blocks: set[tuple[Path, str]] = set()
    for item in items:
        assert item.return_text is not None
        if not item.source or not item.source.exists():
            raise SystemExit("Missing source for " + item.rd.as_posix())
        lines = item.source.read_text(encoding="utf-8", errors="replace").splitlines()
        def_i = find_definition(lines, item.name)
        if def_i is None:
            raise SystemExit("Definition not found for " + item.name)
        block = roxygen_block(lines, def_i)
        if block is None:
            raise SystemExit("Roxygen block not found for " + item.name)
        start, end = block
        if any("@return" in line for line in lines[start:end]):
            continue
        key = (item.source, item.name)
        if key in seen_blocks:
            continue
        seen_blocks.add(key)
        export_i = next((i for i in range(start, end) if "@export" in lines[i]), end)
        insertion = "#' @return " + item.return_text
        source_insertions.setdefault(item.source, []).append((export_i, insertion))

    for source, insertions in source_insertions.items():
        lines = source.read_text(encoding="utf-8", errors="replace").splitlines()
        for index, text in sorted(insertions, reverse=True):
            lines.insert(index, text)
        source.write_text("\n".join(lines) + "\n", encoding="utf-8")

    for item in items:
        assert item.return_text is not None
        text = item.rd.read_text(encoding="utf-8", errors="replace")
        if "\\value{" in text:
            continue
        value = "\\value{\n" + rd_escape(item.return_text) + "\n}\n"
        anchor = "\\description{"
        pos = text.find(anchor)
        if pos < 0:
            for candidate in ("\\details{", "\\examples{"):
                pos = text.find(candidate)
                if pos >= 0:
                    break
        if pos < 0:
            text = text.rstrip() + "\n" + value
        else:
            text = text[:pos] + value + text[pos:]
        item.rd.write_text(text, encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--apply", action="store_true")
    args = parser.parse_args()
    items = inventory()
    print("RETURN_DOC_INVENTORY=%d" % len(items))
    counts: dict[str, int] = {}
    for item in items:
        counts[item.confidence] = counts.get(item.confidence, 0) + 1
        prefix = "RETURN_DOC" if item.return_text else "UNRESOLVED"
        print(f"{prefix}|{item.rd.relative_to(ROOT).as_posix()}|{item.name}|{item.confidence}|{item.return_text or ''}")
    print("RETURN_DOC_COUNTS=" + ",".join(f"{k}:{counts[k]}" for k in sorted(counts)))
    if args.apply:
        apply(items)
        print("RETURN_DOC_APPLIED=%d" % len(items))


if __name__ == "__main__":
    main()
