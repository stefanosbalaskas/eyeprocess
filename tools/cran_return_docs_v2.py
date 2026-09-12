#!/usr/bin/env python3
"""Infer and apply CRAN-quality return documentation from R terminal expressions.

Unlike a simple body-wide keyword scan, this analyzer follows the function's
terminal top-level expression, traces terminal symbols to their last top-level
assignment, and follows wrappers to other package functions. It is conservative
for genuinely polymorphic returns and records the inference reason for review.
"""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


@dataclass
class FunctionInfo:
    name: str
    source: Path
    def_i: int
    body: str
    statements: list[str]


@dataclass
class ReturnInfo:
    kind: str
    classes: list[str]
    components: list[str]
    expr: str
    confidence: str


@dataclass
class Item:
    rd: Path
    source: Path | None
    name: str
    title: str
    return_text: str
    info: ReturnInfo


def rd_field(text: str, field: str) -> str:
    m = re.search(r"\\" + re.escape(field) + r"\{([^{}]*)\}", text, re.S)
    return re.sub(r"\s+", " ", m.group(1)).strip() if m else ""


def source_from_rd(text: str) -> Path | None:
    m = re.search(r"^% Please edit documentation in (R/[^\r\n]+)$", text, re.M)
    return ROOT / m.group(1).strip() if m else None


def find_definition(lines: list[str], name: str) -> int | None:
    escaped = re.escape(name)
    rx = re.compile(r"^\s*(?:`" + escaped + r"`|" + escaped + r")\s*(?:<-|=)\s*function\s*\(")
    for i, line in enumerate(lines):
        if rx.search(line):
            return i
    return None


def function_slice(lines: list[str], start: int) -> str:
    end = len(lines)
    for i in range(start + 1, len(lines)):
        if lines[i].startswith("#'"):
            end = i
            break
    return "\n".join(lines[start:end]).rstrip()


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


def _first_body_brace(text: str) -> int | None:
    quote = None
    escape = False
    comment = False
    paren = 0
    for i, ch in enumerate(text):
        if comment:
            if ch == "\n":
                comment = False
            continue
        if quote:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            continue
        if ch == "#":
            comment = True
        elif ch in "'\"`":
            quote = ch
        elif ch == "(":
            paren += 1
        elif ch == ")":
            paren = max(paren - 1, 0)
        elif ch == "{" and paren == 0:
            return i
    return None


def top_level_statements(function_text: str) -> list[str]:
    start = _first_body_brace(function_text)
    if start is None:
        return []
    out: list[str] = []
    buf: list[str] = []
    brace = 1
    paren = 0
    bracket = 0
    quote = None
    escape = False
    comment = False
    i = start + 1
    while i < len(function_text):
        ch = function_text[i]
        if comment:
            if ch == "\n":
                comment = False
                if brace == 1 and paren == 0 and bracket == 0 and "".join(buf).strip():
                    out.append("".join(buf).strip())
                    buf = []
                else:
                    buf.append(ch)
            i += 1
            continue
        if quote:
            buf.append(ch)
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            i += 1
            continue
        if ch == "#":
            comment = True
            i += 1
            continue
        if ch in "'\"`":
            quote = ch
            buf.append(ch)
        elif ch == "{":
            brace += 1
            buf.append(ch)
        elif ch == "}":
            brace -= 1
            if brace == 0:
                if "".join(buf).strip():
                    out.append("".join(buf).strip())
                break
            buf.append(ch)
        elif ch == "(":
            paren += 1
            buf.append(ch)
        elif ch == ")":
            paren = max(paren - 1, 0)
            buf.append(ch)
        elif ch == "[":
            bracket += 1
            buf.append(ch)
        elif ch == "]":
            bracket = max(bracket - 1, 0)
            buf.append(ch)
        elif ch == ";" and brace == 1 and paren == 0 and bracket == 0:
            if "".join(buf).strip():
                out.append("".join(buf).strip())
            buf = []
        elif ch == "\n" and brace == 1 and paren == 0 and bracket == 0:
            if "".join(buf).strip():
                out.append("".join(buf).strip())
            buf = []
        else:
            buf.append(ch)
        i += 1
    return [re.sub(r"\s+", " ", x).strip() for x in out if x.strip()]


def split_call_args(expr: str) -> list[str]:
    open_i = expr.find("(")
    if open_i < 0:
        return []
    args: list[str] = []
    buf: list[str] = []
    paren = bracket = brace = 0
    quote = None
    escape = False
    for ch in expr[open_i + 1 :]:
        if quote:
            buf.append(ch)
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            continue
        if ch in "'\"`":
            quote = ch
            buf.append(ch)
        elif ch == "(":
            paren += 1; buf.append(ch)
        elif ch == ")":
            if paren == 0 and bracket == 0 and brace == 0:
                if "".join(buf).strip(): args.append("".join(buf).strip())
                break
            paren = max(paren - 1, 0); buf.append(ch)
        elif ch == "[": bracket += 1; buf.append(ch)
        elif ch == "]": bracket = max(bracket - 1, 0); buf.append(ch)
        elif ch == "{": brace += 1; buf.append(ch)
        elif ch == "}": brace = max(brace - 1, 0); buf.append(ch)
        elif ch == "," and paren == 0 and bracket == 0 and brace == 0:
            args.append("".join(buf).strip()); buf = []
        else:
            buf.append(ch)
    return args


def classes_in(expr: str) -> list[str]:
    m = re.search(r"\bclass\s*=\s*(c\s*\((.*?)\)|(['\"])(.*?)\3)", expr, re.S)
    if not m:
        return []
    if m.group(2) is not None:
        return re.findall(r"['\"]([^'\"]+)['\"]", m.group(2))
    return [m.group(4)] if m.group(4) else []


def list_components(expr: str) -> list[str]:
    m = re.search(r"\blist\s*\(", expr)
    if not m:
        return []
    sub = expr[m.start():]
    names: list[str] = []
    for arg in split_call_args(sub):
        mm = re.match(r"([A-Za-z.][A-Za-z0-9._]*)\s*=", arg)
        if mm and mm.group(1) not in names:
            names.append(mm.group(1))
    return names


def assignments(statements: list[str]) -> dict[str, str]:
    out: dict[str, str] = {}
    for stmt in statements:
        m = re.match(r"^([A-Za-z.][A-Za-z0-9._]*)\s*<-\s*(.+)$", stmt, re.S)
        if m:
            out[m.group(1)] = m.group(2).strip()
    return out


def call_name(expr: str) -> str | None:
    m = re.match(r"^(?:[A-Za-z.][A-Za-z0-9._]*:::{0,1})?([A-Za-z.][A-Za-z0-9._]*)\s*\(", expr)
    return m.group(1) if m else None


def infer_expr(expr: str, assigns: dict[str, str], funcs: dict[str, FunctionInfo], seen: set[str]) -> ReturnInfo:
    expr = expr.strip().rstrip(";")
    compact = re.sub(r"\s+", " ", expr)
    if re.fullmatch(r"[A-Za-z.][A-Za-z0-9._]*", expr):
        if expr in assigns:
            return infer_expr(assigns[expr], assigns, funcs, seen)
        if expr in {"TRUE", "FALSE"}:
            return ReturnInfo("logical", [], [], compact, "terminal")
        if expr == "NULL":
            return ReturnInfo("null", [], [], compact, "terminal")
        return ReturnInfo("object", [], [], compact, "terminal-symbol")
    if re.fullmatch(r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?L?", expr):
        return ReturnInfo("numeric", [], [], compact, "terminal")
    if (len(expr) >= 2 and expr[0] == expr[-1] and expr[0] in "'\""):
        return ReturnInfo("character", [], [], compact, "terminal")

    name = call_name(expr)
    if name in {"invisible", "return"}:
        args = split_call_args(expr)
        if not args or args[0].strip() == "NULL":
            return ReturnInfo("null", [], [], compact, "terminal")
        return infer_expr(args[0], assigns, funcs, seen)

    cls = classes_in(expr)
    if name == "structure" and cls:
        comps = list_components(expr)
        kind = "data.frame" if "data.frame" in cls else ("list" if "list(" in expr else "classed")
        return ReturnInfo(kind, cls, comps, compact, "terminal-class")

    if name in {"data.frame", "as.data.frame"}:
        return ReturnInfo("data.frame", ["data.frame"], [], compact, "terminal")
    if name in {"list", "as.list"}:
        return ReturnInfo("list", ["list"], list_components(expr), compact, "terminal")
    if name in {"matrix", "as.matrix"}:
        return ReturnInfo("matrix", ["matrix"], [], compact, "terminal")
    if name in {"array", "as.array"}:
        return ReturnInfo("array", ["array"], [], compact, "terminal")
    if name in {"factor", "as.factor"}:
        return ReturnInfo("factor", ["factor"], [], compact, "terminal")

    numeric_calls = {
        "as.numeric", "numeric", "double", "mean", "median", "sum", "prod", "min", "max", "var", "sd",
        "quantile", "cor", "cov", "plogis", "qlogis", "pnorm", "qnorm", "dnorm", "runif", "rnorm", "rbinom",
        "rnbinom", "sqrt", "exp", "log", "log1p", "expm1", "abs", "round", "floor", "ceiling", "signif",
        "seq", "seq_len", "seq_along", "sample", "sample.int", "rowMeans", "colMeans", "rowSums", "colSums",
        "length", "nrow", "ncol", "which", "match", "findInterval", "uniroot", "optimize",
    }
    logical_calls = {"isTRUE", "identical", "all", "any", "is.na", "is.null", "is.finite", "is.infinite", "grepl", "nzchar"}
    character_calls = {"as.character", "character", "paste", "paste0", "sprintf", "format", "formatC", "normalizePath", "file.path", "basename", "dirname"}
    list_calls = {"lapply", "split"}
    if name in numeric_calls:
        return ReturnInfo("numeric", [], [], compact, "terminal-call")
    if name in logical_calls:
        return ReturnInfo("logical", [], [], compact, "terminal-call")
    if name in character_calls:
        return ReturnInfo("character", [], [], compact, "terminal-call")
    if name in list_calls:
        return ReturnInfo("list", ["list"], [], compact, "terminal-call")
    if name in {"vapply", "sapply"}:
        return ReturnInfo("vector-or-matrix", [], [], compact, "terminal-call")
    if name == "do.call" and re.search(r"\b(?:rbind|cbind)\b", expr):
        return ReturnInfo("tabular", [], [], compact, "terminal-call")
    if name in {"rbind", "cbind"}:
        return ReturnInfo("tabular", [], [], compact, "terminal-call")
    if name in {"ggplot", "qplot"} or "ggplot2::ggplot" in expr:
        return ReturnInfo("ggplot", ["ggplot"], [], compact, "terminal-call")
    if name in {"writeLines", "write.table", "write.csv", "saveRDS", "save", "cat"}:
        return ReturnInfo("null", [], [], compact, "terminal-side-effect")

    # Arithmetic/comparison expressions are common terminal expressions.
    if re.search(r"(?:==|!=|<=|>=|<|>|%in%|\&\&|\|\|)", expr):
        return ReturnInfo("logical", [], [], compact, "terminal-expression")
    if re.search(r"(?:\+|-|\*|/|\^|%%|%/%)", expr) and not re.search(r"['\"]", expr):
        return ReturnInfo("numeric", [], [], compact, "terminal-expression")

    # Follow wrappers to another package function when its terminal expression is known.
    if name and name in funcs and name not in seen:
        target = infer_function(name, funcs, seen | {name})
        return ReturnInfo(target.kind, target.classes, target.components, compact, "wrapper->" + target.confidence)

    # A terminal if-expression may have different branches; infer both when easy.
    if expr.startswith("if ") or expr.startswith("if("):
        returns = re.findall(r"return\s*\(([^()]*(?:\([^()]*\)[^()]*)*)\)", expr)
        infos = [infer_expr(x, assigns, funcs, seen) for x in returns]
        kinds = {x.kind for x in infos if x.kind != "null"}
        if len(kinds) == 1:
            x = next(x for x in infos if x.kind != "null")
            return ReturnInfo(x.kind, x.classes, x.components, compact, "conditional")

    return ReturnInfo("object", [], [], compact, "unresolved")


def infer_function(name: str, funcs: dict[str, FunctionInfo], seen: set[str] | None = None) -> ReturnInfo:
    if seen is None:
        seen = {name}
    fn = funcs.get(name)
    if not fn or not fn.statements:
        return ReturnInfo("object", [], [], "", "definition-missing")
    stmts = fn.statements
    assigns = assignments(stmts)
    terminal = stmts[-1]
    # A terminal assignment returns its RHS invisibly.
    m = re.match(r"^[A-Za-z.][A-Za-z0-9._]*\s*<-\s*(.+)$", terminal, re.S)
    if m:
        terminal = m.group(1).strip()
    return infer_expr(terminal, assigns, funcs, seen)


def result_phrase(title: str, name: str) -> str:
    title = re.sub(r"\s+", " ", title.strip().rstrip("."))
    if not title:
        return name.replace("_", " ") + " result"
    verbs = (
        "Compute ", "Calculate ", "Estimate ", "Derive ", "Build ", "Create ", "Construct ", "Generate ",
        "Fit ", "Run ", "Perform ", "Apply ", "Assess ", "Audit ", "Evaluate ", "Validate ", "Check ",
        "Inspect ", "Summarize ", "Summarise ", "Compare ", "Convert ", "Transform ", "Import ", "Read ",
        "Export ", "Write ", "Prepare ", "Make ", "Score ", "Draw ", "Detect ", "Extract ", "Predict ",
        "Simulate ", "Reconstruct ", "Register ", "Map ", "Measure ", "Test ", "Select ", "Identify ",
    )
    for verb in verbs:
        if title.startswith(verb):
            rest = title[len(verb):]
            return rest[:1].lower() + rest[1:]
    return title[:1].lower() + title[1:]


def return_text(name: str, title: str, info: ReturnInfo) -> str:
    phrase = result_phrase(title, name)
    cls = info.classes
    comps = info.components
    if name.startswith("print."):
        return "Invisibly returns the input object after printing its summary; the object's class and contents are unchanged."
    if name.startswith("plot."):
        if info.kind == "ggplot":
            return "A ggplot object representing " + phrase + ". Printing the object draws the plot."
        return "Invisibly returns the plotting result when available; the primary effect is drawing " + phrase + "."
    if cls and cls != ["data.frame"] and cls != ["list"]:
        class_text = ", ".join('"' + x + '"' for x in cls)
        storage = "a data frame" if "data.frame" in cls else ("a named list" if info.kind == "list" or comps else "an R object")
        if comps:
            shown = ", ".join('"' + x + '"' for x in comps[:12])
            if len(comps) > 12:
                shown += ", and additional components"
            return f"An object of class {class_text}, stored as {storage}, with components {shown}. It contains {phrase} and associated metadata or diagnostics needed to interpret the result."
        return f"An object of class {class_text}, stored as {storage}, containing {phrase} and associated metadata needed to interpret the result."
    if info.kind == "data.frame":
        return "A data frame containing " + phrase + ". Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function."
    if info.kind == "list":
        if comps:
            return "A named list with components " + ", ".join('"' + x + '"' for x in comps[:12]) + ", containing " + phrase + " and associated metadata or diagnostics."
        return "A list containing " + phrase + " and associated metadata or diagnostics."
    if info.kind == "matrix":
        return "A matrix containing " + phrase + "; rows and columns correspond to the analysis dimensions described by the function arguments."
    if info.kind == "array":
        return "An array containing " + phrase + ", with dimensions corresponding to the analysis units described by the function arguments."
    if info.kind == "factor":
        return "A factor containing " + phrase + "."
    if info.kind == "numeric":
        return "A numeric value or vector containing " + phrase + "."
    if info.kind == "logical":
        return "A logical value or vector indicating " + phrase + "."
    if info.kind == "character":
        if any(word in title.lower() for word in ("path", "file", "export", "write", "save")):
            return "A character string or vector giving the path or identifier for " + phrase + "."
        return "A character value or vector containing " + phrase + "."
    if info.kind == "vector-or-matrix":
        return "A vector or matrix containing " + phrase + ", with shape determined by the supplied analysis units."
    if info.kind == "tabular":
        return "A tabular R object containing " + phrase + "; rows represent analysis units and columns contain the returned quantities."
    if info.kind == "ggplot":
        return "A ggplot object representing " + phrase + ". Printing the object draws the plot."
    if info.kind == "null":
        return "No meaningful return value; the function is called for its documented side effects while producing " + phrase + "."
    # Conservative, explicit polymorphic fallback. It still states structure and
    # meaning without inventing a class that static analysis cannot establish.
    return "An R object containing " + phrase + ". The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow."


def build_functions() -> dict[str, FunctionInfo]:
    funcs: dict[str, FunctionInfo] = {}
    for source in sorted((ROOT / "R").glob("*.R")):
        lines = source.read_text(encoding="utf-8", errors="replace").splitlines()
        for i, line in enumerate(lines):
            m = re.match(r"^\s*(?:`([^`]+)`|([A-Za-z.][A-Za-z0-9._]*))\s*(?:<-|=)\s*function\s*\(", line)
            if not m:
                continue
            name = m.group(1) or m.group(2)
            body = function_slice(lines, i)
            funcs[name] = FunctionInfo(name, source, i, body, top_level_statements(body))
    return funcs


def inventory(funcs: dict[str, FunctionInfo]) -> list[Item]:
    items: list[Item] = []
    for rd in sorted((ROOT / "man").glob("*.Rd")):
        text = rd.read_text(encoding="utf-8", errors="replace")
        if "\\docType{data}" in text or "\\usage{" not in text or "\\value{" in text:
            continue
        name = rd_field(text, "name") or rd.stem
        title = rd_field(text, "title") or name.replace("_", " ")
        source = source_from_rd(text)
        info = infer_function(name, funcs)
        items.append(Item(rd, source, name, title, return_text(name, title, info), info))
    return items


def rd_escape(text: str) -> str:
    return text.replace("\\", "\\\\").replace("%", "\\%").replace("{", "\\{").replace("}", "\\}")


def apply(items: list[Item], funcs: dict[str, FunctionInfo]) -> None:
    by_source: dict[Path, list[Item]] = {}
    for item in items:
        if not item.source or not item.source.exists():
            raise SystemExit("Missing roxygen source for " + item.rd.as_posix())
        if item.name not in funcs:
            raise SystemExit("Function definition not found for " + item.name)
        by_source.setdefault(item.source, []).append(item)

    for source, source_items in by_source.items():
        lines = source.read_text(encoding="utf-8", errors="replace").splitlines()
        insertions: list[tuple[int, str]] = []
        seen_ranges: set[tuple[int, int]] = set()
        for item in source_items:
            def_i = find_definition(lines, item.name)
            if def_i is None:
                raise SystemExit("Definition not found for " + item.name)
            block = roxygen_block(lines, def_i)
            if block is None:
                raise SystemExit("Roxygen block not found for " + item.name)
            start, end = block
            if (start, end) in seen_ranges:
                continue
            seen_ranges.add((start, end))
            if any("@return" in line for line in lines[start:end]):
                continue
            export_i = next((i for i in range(start, end) if "@export" in lines[i]), end)
            insertions.append((export_i, "#' @return " + item.return_text))
        for index, text in sorted(insertions, reverse=True):
            lines.insert(index, text)
        source.write_text("\n".join(lines) + "\n", encoding="utf-8")

    # Keep committed Rd files synchronized immediately. A subsequent
    # roxygen2::roxygenise() check verifies regeneration produces no drift.
    for item in items:
        text = item.rd.read_text(encoding="utf-8", errors="replace")
        if "\\value{" in text:
            continue
        value = "\\value{\n" + rd_escape(item.return_text) + "\n}\n"
        anchors = [text.find("\\description{"), text.find("\\details{"), text.find("\\examples{")]
        anchors = [x for x in anchors if x >= 0]
        if anchors:
            pos = min(anchors)
            text = text[:pos] + value + text[pos:]
        else:
            text = text.rstrip() + "\n" + value
        item.rd.write_text(text, encoding="utf-8")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--apply", action="store_true")
    parser.add_argument("--max-polymorphic", type=int, default=1000)
    args = parser.parse_args()
    funcs = build_functions()
    items = inventory(funcs)
    counts: dict[str, int] = {}
    for item in items:
        key = item.info.kind + "/" + item.info.confidence
        counts[key] = counts.get(key, 0) + 1
        print(f"RETURN_V2|{item.name}|{item.info.kind}|{item.info.confidence}|{item.info.expr[:240]}|{item.return_text}")
    polymorphic = sum(1 for x in items if x.info.kind == "object")
    print(f"RETURN_V2_INVENTORY={len(items)}")
    print(f"RETURN_V2_POLYMORPHIC={polymorphic}")
    print("RETURN_V2_COUNTS=" + ",".join(f"{k}:{counts[k]}" for k in sorted(counts)))
    if polymorphic > args.max_polymorphic:
        raise SystemExit(f"Polymorphic/unresolved returns {polymorphic} exceed threshold {args.max_polymorphic}")
    if args.apply:
        apply(items, funcs)
        print(f"RETURN_V2_APPLIED={len(items)}")


if __name__ == "__main__":
    main()
