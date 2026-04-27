#!/usr/bin/env python3
"""Generate SVG cheatsheets from macropad-cheatsheet-*.md.

Reads macropad-cheatsheet-{hypr,lcag,meh}.md from this directory and writes
matching .svg files in the style of keymap_diagram_l0_l1.svg.
"""
import re
import sys
from pathlib import Path

LAYER_INFO = {
    "hypr": {"name": "_HYPR", "color": "#8000ff", "prefix": "H", "active_col": 1},
    "lcag": {"name": "_LCAG", "color": "#0050ff", "prefix": "L", "active_col": 2},
    "meh":  {"name": "_MEH",  "color": "#ffa000", "prefix": "M", "active_col": 3},
}

TOP_LABELS = ["numpad", "codenav", "win nav", "win mutate"]
TOP_COLORS = ["#00b4b4", "#8000ff", "#0050ff", "#ffa000"]
BOTTOM_LABELS = ["Home", "PgUp", "PgDn", "End"]

LEFT_X = 24
COL_W = 130
COL_GAP = 6
ROW_H = 75
SMALL_H = 45
TOP_Y = 70
FUNC_YS = [133, 214, 295, 376]
BOTTOM_Y = 469
TABLE_TITLE_Y = 552
TABLE_FIRST_Y = 580
TABLE_ROW_H = 18
SVG_WIDTH = 606
BG = "#141414"
CELL_FILL = "#2d2d2d"
EMPTY_FILL = "#1c1c1c"
EMPTY_STROKE = "#3a3a3a"
NAV_STROKE = "#555"
TEXT_LIGHT = "#e0e0e0"
TEXT_DIM = "#aaa"


def col_x(i):
    return LEFT_X + i * (COL_W + COL_GAP)


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def parse_diagram(md):
    """Return rows from the box-drawn diagram. Each row is a list of cell strings
    (multi-line cells joined with \\n)."""
    in_block = False
    block = []
    for line in md.splitlines():
        if line.strip().startswith("```"):
            if in_block:
                break
            in_block = True
            continue
        if in_block:
            block.append(line)

    groups = []
    current = []
    for line in block:
        if "│" in line:
            cells = [p.strip() for p in line.split("│")[1:-1]]
            current.append(cells)
        elif current:
            groups.append(current)
            current = []
    if current:
        groups.append(current)

    rows = []
    for g in groups:
        if not g or not g[0]:
            continue
        n = len(g[0])
        rows.append([
            "\n".join(row[c] for row in g if c < len(row) and row[c])
            for c in range(n)
        ])
    return rows


def parse_table(md):
    """Return list of (key, binding, description) from the markdown table."""
    rows = []
    for line in md.splitlines():
        if not line.startswith("|"):
            continue
        parts = [p.strip() for p in line.strip().strip("|").split("|")]
        if len(parts) < 3:
            continue
        if all(re.match(r"^[-:]+$", p) for p in parts):
            continue
        if parts[0].lower() == "key":
            continue
        key, binding, desc = parts[0], parts[1].strip("`"), " | ".join(parts[2:])
        rows.append((key, binding, desc))
    return rows


def parse_title_intro(md):
    title = intro = ""
    for line in md.splitlines():
        if line.startswith("## ") and not title:
            title = line[3:].strip()
        elif title and not intro:
            s = line.strip()
            if s and not s.startswith(("#", "```")):
                intro = s
                break
    return title, intro


def indicator(x, y, w, h, fill, stroke, sw, label, label_fill="#0e0e0e"):
    cx = x + w / 2
    cy = y + h / 2
    return [
        f'  <rect x="{x}" y="{y}" width="{w}" height="{h}" rx="6" '
        f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"/>',
        f'  <text x="{cx}" y="{cy}" text-anchor="middle" dominant-baseline="central" '
        f'fill="{label_fill}" font-size="15px" font-family="monospace">{esc(label)}</text>',
    ]


def func_cell(x, y, color, prefix, default_idx, content):
    out = []
    cx = x + COL_W / 2
    lines = [l.strip() for l in content.split("\n") if l.strip()] if content else []

    binding_re = re.compile(rf"^{prefix}-([a-p])$")
    tag = None
    display = []
    for l in lines:
        if binding_re.match(l) and tag is None:
            tag = l
        else:
            display.append(l)

    if not display and tag is None:
        out.append(
            f'  <rect x="{x}" y="{y}" width="{COL_W}" height="{ROW_H}" rx="6" '
            f'fill="{EMPTY_FILL}" stroke="{EMPTY_STROKE}" stroke-width="1.5"/>'
        )
        return out

    out.append(
        f'  <rect x="{x}" y="{y}" width="{COL_W}" height="{ROW_H}" rx="6" '
        f'fill="{CELL_FILL}" stroke="{color}" stroke-width="1.5"/>'
    )
    if tag is None:
        tag = f"{prefix}-{chr(ord('a') + default_idx)}"
    out.append(
        f'  <text x="{x + 8}" y="{y + 15}" fill="{color}" '
        f'font-size="10px" font-family="monospace">{esc(tag)}</text>'
    )

    n = len(display)
    if n == 1:
        out.append(
            f'  <text x="{cx}" y="{y + ROW_H/2}" text-anchor="middle" '
            f'dominant-baseline="central" fill="{TEXT_LIGHT}" font-size="13px" '
            f'font-family="monospace">{esc(display[0])}</text>'
        )
    elif n == 2:
        for i, ln in enumerate(display):
            dy = y + ROW_H / 2 + (-9 if i == 0 else 9)
            out.append(
                f'  <text x="{cx}" y="{dy}" text-anchor="middle" '
                f'dominant-baseline="central" fill="{TEXT_LIGHT}" font-size="12px" '
                f'font-family="monospace">{esc(ln)}</text>'
            )
    elif n >= 3:
        for i, ln in enumerate(display[:3]):
            dy = y + 25 + i * 16
            out.append(
                f'  <text x="{cx}" y="{dy}" text-anchor="middle" '
                f'dominant-baseline="central" fill="{TEXT_LIGHT}" font-size="11px" '
                f'font-family="monospace">{esc(ln)}</text>'
            )
    return out


def make_svg(key, md):
    info = LAYER_INFO[key]
    color = info["color"]
    diagram = parse_diagram(md)
    table = parse_table(md)
    title, intro = parse_title_intro(md)

    if table:
        total_h = TABLE_FIRST_Y + len(table) * TABLE_ROW_H + 20
    else:
        total_h = BOTTOM_Y + SMALL_H + 30

    parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg" '
        f'width="{SVG_WIDTH}" height="{total_h}" '
        f'viewBox="0 0 {SVG_WIDTH} {total_h}" '
        f'font-family="system-ui, -apple-system, sans-serif">',
        f'  <rect width="{SVG_WIDTH}" height="{total_h}" fill="{BG}"/>',
        "",
    ]
    title_text = f'{info["name"]}     {title}' if title else info["name"]
    parts.append(
        f'  <text x="24" y="30" fill="{color}" font-size="15px" font-weight="bold" '
        f'font-family="monospace">{esc(title_text)}</text>'
    )
    if intro:
        parts.append(
            f'  <text x="24" y="52" fill="{TEXT_DIM}" font-size="11px" '
            f'font-family="monospace">{esc(intro)}</text>'
        )

    parts.append("")
    parts.append("  <!-- Top indicator row -->")
    for i, label in enumerate(TOP_LABELS):
        active = (i == info["active_col"])
        stroke = "#e0e0e0" if active else TOP_COLORS[i]
        sw = 2.5 if active else 1.5
        parts.extend(indicator(col_x(i), TOP_Y, COL_W, SMALL_H,
                               TOP_COLORS[i], stroke, sw, label))

    func_rows = diagram[1:5] if len(diagram) >= 6 else diagram[:4]
    for r, row in enumerate(func_rows):
        parts.append("")
        parts.append(f"  <!-- Row {r + 1} -->")
        for c, content in enumerate(row):
            parts.extend(func_cell(col_x(c), FUNC_YS[r], color,
                                   info["prefix"], r * 4 + c, content))

    parts.append("")
    parts.append("  <!-- Bottom indicator row -->")
    for i, label in enumerate(BOTTOM_LABELS):
        parts.extend(indicator(col_x(i), BOTTOM_Y, COL_W, SMALL_H,
                               CELL_FILL, NAV_STROKE, 1.5, label, TEXT_LIGHT))

    if table:
        parts.append("")
        parts.append(
            f'  <text x="24" y="{TABLE_TITLE_Y}" fill="{TEXT_DIM}" font-size="13px" '
            f'font-weight="bold" font-family="monospace">Quick Reference</text>'
        )
        parts.append(
            f'  <line x1="24" y1="{TABLE_TITLE_Y + 8}" x2="582" '
            f'y2="{TABLE_TITLE_Y + 8}" stroke="#3a3a3a" stroke-width="1"/>'
        )
        for i, (k, binding, desc) in enumerate(table):
            ty = TABLE_FIRST_Y + i * TABLE_ROW_H
            parts.append(
                f'  <text x="24"  y="{ty}" fill="{color}" font-size="12px" '
                f'font-family="monospace">{esc(k)}</text>'
            )
            parts.append(
                f'  <text x="70"  y="{ty}" fill="{TEXT_LIGHT}" font-size="12px" '
                f'font-family="monospace">{esc(binding)}</text>'
            )
            parts.append(
                f'  <text x="290" y="{ty}" fill="{TEXT_DIM}" font-size="12px" '
                f'font-family="monospace">{esc(desc)}</text>'
            )

    parts.append("</svg>")
    return "\n".join(parts) + "\n"


def main():
    base = Path(__file__).resolve().parent
    for key in LAYER_INFO:
        md_path = base / f"macropad-cheatsheet-{key}.md"
        if not md_path.exists():
            print(f"skip: {md_path}", file=sys.stderr)
            continue
        out = base / f"macropad-cheatsheet-{key}.svg"
        out.write_text(make_svg(key, md_path.read_text(encoding="utf-8")),
                       encoding="utf-8")
        print(f"wrote {out}")


if __name__ == "__main__":
    main()
