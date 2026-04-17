#!/usr/bin/env python3
"""Generate an SVG diagram from the Framework 16 RGB macropad's keymap.c.

The macropad is a 6x4 grid; rendering is straightforward. Only the bottom-left
key is physically wider (2.25u), and the top row is slightly shorter (0.75u).
Extra vertical spacing between rows 2/3 and 5/6 reflects the physical key gap.

Usage:
    python3 gen_macropad_svg.py [keymap.c [output.svg]]
"""

import re
import sys

# Geometry
UNIT = 60               # pixels per 1u
GAP = 6                 # gap between keys
TOP_H = 45              # top row is 0.75u in info.json
NORMAL_H = 75           # remaining rows are 1.25u
ROW_GAP_EXTRA = 12      # extra space between rows 2/3 and 5/6 (physical break)
LAYER_TITLE_H = 26

# Per-layer theme colors (must match the C keymap's CLR_* values)
LAYER_COLOR = {
    '_NUM_ON':  '#00b4b4',  # cyan
    '_NUM_OFF': '#00b400',  # green
    '_HYPR':    '#8000ff',  # purple
    '_LCAG':    '#0050ff',  # blue
    '_MEH':     '#ffa000',  # amber
}

LAYER_TITLE = {
    '_NUM_ON':  'L0  _NUM_ON   Numpad (OS Num Lock on)',
    '_NUM_OFF': 'L1  _NUM_OFF  Nav (OS Num Lock off, falls through to _NUM_ON)',
    '_HYPR':    'L2  _HYPR     Ctrl+Alt+Shift+Super + letter',
    '_LCAG':    'L3  _LCAG     Ctrl+Alt+Super + letter',
    '_MEH':     'L4  _MEH      Ctrl+Alt+Shift + letter',
}

# Top-row slot (col 0..3) -> which layer it switches to, for color coding.
# H1 is KC_NUM which toggles between _NUM_ON and _NUM_OFF; we use cyan as the
# canonical numpad color.
TOP_ROW_TARGET = {0: '_NUM_ON', 1: '_HYPR', 2: '_LCAG', 3: '_MEH'}

# Keycode -> display label
KC_LABELS = {
    'KC_NUM':  'Num',   'KC_PSLS': '/', 'KC_PAST': '*', 'KC_PMNS': '-',
    'KC_PPLS': '+',     'KC_PENT': 'Enter', 'KC_PDOT': '.', 'KC_EQL': '=',
    'KC_HOME': 'Home',  'KC_END':  'End', 'KC_PGUP': 'PgUp', 'KC_PGDN': 'PgDn',
    'KC_UP': '\u2191',  'KC_DOWN': '\u2193',
    'KC_LEFT': '\u2190','KC_RGHT': '\u2192',
    'KC_INS':  'Ins',   'KC_DEL': 'Del',
    'BL_STEP': 'BL',    'RGB_MOD': 'RGB+', 'RGB_RMOD': 'RGB-',
    'RGB_SPD': 'Spd-',  'RGB_HUD': 'Hue-', 'RGB_SAD': 'Sat-',
    '_______': '\u00b7',  # middle dot for transparent
    'KC_NO':   '',
}
for i in range(10):
    KC_LABELS[f'KC_P{i}'] = str(i)
for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
    KC_LABELS[f'KC_{c}'] = c

TO_FRIENDLY = {
    'NUM_ON':  'Num',
    'NUM_OFF': 'Nav',
    'HYPR':    'HYPR',
    'LCAG':    'LCAG',
    'MEH':     'MEH',
}


def label_for(kc):
    if kc in KC_LABELS:
        return KC_LABELS[kc]
    m = re.match(r'(HYPR|LCAG|MEH)\((KC_\w+)\)', kc)
    if m:
        return label_for(m.group(2))
    m = re.match(r'TO\(_(\w+)\)', kc)
    if m:
        return '\u2192' + TO_FRIENDLY.get(m.group(1), m.group(1))
    return kc


def parse_keymap(path):
    with open(path) as f:
        content = f.read()

    enum_match = re.search(r'enum\s+\w+\s*\{([^}]+)\}', content)
    if not enum_match:
        raise RuntimeError('Could not find a layer enum in ' + path)

    layer_names = []
    for line in enum_match.group(1).splitlines():
        line = line.strip()
        if not line or line.startswith('//'):
            continue
        m = re.match(r'(\w+)', line)
        if m:
            layer_names.append(m.group(1))

    layers = {}
    for name in layer_names:
        pat = rf'\[{re.escape(name)}\]\s*=\s*LAYOUT\s*\('
        m = re.search(pat, content)
        if not m:
            continue
        start = m.end()
        depth = 1
        i = start
        while i < len(content) and depth > 0:
            if content[i] == '(':
                depth += 1
            elif content[i] == ')':
                depth -= 1
            i += 1
        body = content[start:i - 1]
        body = re.sub(r'//[^\n]*', '', body)
        body = re.sub(r'/\*.*?\*/', '', body, flags=re.DOTALL)
        keycodes = [kc.strip() for kc in body.split(',') if kc.strip()]
        layers[name] = keycodes

    return layer_names, layers


def esc(s):
    return (s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
             .replace("'", '&apos;').replace('"', '&quot;'))


def key_color(layer_name, row, col, kc):
    """Return (fill, stroke, text_color) for a key."""
    # Top row: solid fill in the target layer's color
    if row == 0:
        target = TOP_ROW_TARGET.get(col)
        color = LAYER_COLOR.get(target, '#444')
        return color, color, '#0e0e0e'

    # Transparent/no-op keys
    if kc in ('_______', 'KC_NO'):
        return '#1a1a1a', '#333', '#555'

    # Modifier-stamp layers: give middle grid a subtle layer-color outline
    if layer_name in ('_HYPR', '_LCAG', '_MEH') and 0 < row < 5:
        return '#2d2d2d', LAYER_COLOR[layer_name], '#e0e0e0'

    # Default
    return '#2d2d2d', '#555', '#e0e0e0'


def render_key(x, y, w, h, fill, stroke, text_color, label):
    elems = [f'  <rect x="{x}" y="{y}" width="{w}" height="{h}" rx="6" '
             f'fill="{fill}" stroke="{stroke}" stroke-width="1.5"/>']
    if label:
        fs = 15 if len(label) <= 3 else (12 if len(label) <= 5 else 10)
        elems.append(f'  <text x="{x + w/2}" y="{y + h/2}" text-anchor="middle" '
                     f'dominant-baseline="central" fill="{text_color}" '
                     f'font-size="{fs}px" font-family="monospace">{esc(label)}</text>')
    return elems


def render_layer(layer_name, keycodes, x0, y0):
    elems = []
    title = LAYER_TITLE.get(layer_name, layer_name)
    title_color = LAYER_COLOR.get(layer_name, '#e0e0e0')
    elems.append(f'  <text x="{x0}" y="{y0 + 16}" fill="{title_color}" '
                 f'font-size="15px" font-weight="bold" font-family="monospace">'
                 f'{esc(title)}</text>')

    y = y0 + LAYER_TITLE_H
    idx = 0
    for row in range(6):
        h = TOP_H if row == 0 else NORMAL_H
        x = x0
        for col in range(4):
            kc = keycodes[idx]
            # Bottom-left is 2.25u wide
            if row == 5 and col == 0:
                w = int(UNIT * 2.25) + 2 * GAP
            else:
                w = UNIT
            fill, stroke, text_color = key_color(layer_name, row, col, kc)
            elems.extend(render_key(x, y, w, h, fill, stroke, text_color, label_for(kc)))
            x += w + GAP
            idx += 1
        y += h + GAP
        if row in (1, 4):      # physical gap between row 2/3 and row 5/6
            y += ROW_GAP_EXTRA

    height_used = y - y0 - GAP
    return elems, height_used


def render_legend(x, y):
    items = [
        (LAYER_COLOR['_NUM_ON'],  '_NUM_ON  (Num Lock on)'),
        (LAYER_COLOR['_NUM_OFF'], '_NUM_OFF (Num Lock off)'),
        (LAYER_COLOR['_HYPR'],    '_HYPR    Ctrl+Alt+Shift+Super'),
        (LAYER_COLOR['_LCAG'],    '_LCAG    Ctrl+Alt+Super'),
        (LAYER_COLOR['_MEH'],     '_MEH     Ctrl+Alt+Shift'),
    ]
    elems = [f'  <text x="{x}" y="{y}" fill="#aaa" font-size="12px" '
             f'font-family="monospace" font-weight="bold">Layer colors</text>']
    for i, (color, label) in enumerate(items):
        ly = y + 14 + i * 18
        elems.append(f'  <rect x="{x}" y="{ly}" width="14" height="12" '
                     f'fill="{color}" rx="2"/>')
        elems.append(f'  <text x="{x + 22}" y="{ly + 6}" fill="#aaa" '
                     f'font-size="10px" font-family="monospace" '
                     f'dominant-baseline="central">{esc(label)}</text>')
    return elems


def main():
    keymap_path = sys.argv[1] if len(sys.argv) > 1 else 'keymap.c'
    output_path = sys.argv[2] if len(sys.argv) > 2 else 'keymap_diagram.svg'

    layer_names, layers = parse_keymap(keymap_path)
    render_list = [n for n in layer_names if n in layers and n in LAYER_TITLE]

    margin = 24
    grid_w = 4 * UNIT + 3 * GAP
    legend_w = 260

    # Measure a layer's actual height by dry-rendering
    _, layer_height = render_layer(render_list[0], layers[render_list[0]], 0, 0)
    layer_gap = 22

    content_w = grid_w + 40 + legend_w
    total_height = margin * 2 + len(render_list) * layer_height + (len(render_list) - 1) * layer_gap
    width = margin * 2 + content_w

    elems = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {total_height}" '
             f'font-family="system-ui, -apple-system, sans-serif">']
    elems.append(f'  <rect width="{width}" height="{total_height}" fill="#141414"/>')

    y = margin
    for name in render_list:
        layer_elems, h = render_layer(name, layers[name], margin, y)
        elems.extend(layer_elems)
        y += h + layer_gap

    legend_x = margin + grid_w + 40
    elems.extend(render_legend(legend_x, margin + 4))

    elems.append('</svg>')

    with open(output_path, 'w') as f:
        f.write('\n'.join(elems))
    print(f'Written to {output_path}', file=sys.stderr)


if __name__ == '__main__':
    main()
