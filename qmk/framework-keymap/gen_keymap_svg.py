#!/usr/bin/env python3
"""Generate an SVG keyboard layout diagram from a Framework 16 ANSI QMK keymap.c"""

import re
import sys

# Physical layout: each row is a list of (width, label_from_keycode)
# Widths are in abstract units; 1.0 = standard key
ROW_SIZES = [14, 14, 14, 13, 12, 11]
ROW_WIDTHS = [
    # Function row (half-height)
    [1.25, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.75],
    # Number row
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2],
    # Top row
    [1.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.5],
    # Home row
    [1.75, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2.5],
    # Shift row
    [2.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3],
    # Bottom row: 11 keys, arrows split visually
    [1.25, 1, 1, 1, 6, 1, 1, 1.25, 1.25, 1.25, 1.25],
]

# Key unit size in pixels
UNIT = 44
GAP = 4
FN_ROW_HEIGHT = 24
KEY_HEIGHT = 42
HALF_KEY_HEIGHT = 20  # for split up/down arrows

# Human-readable labels for QMK keycodes
KC_LABELS = {
    'KC_ESC': 'Esc', 'KC_MUTE': 'Mute', 'KC_VOLD': 'Vol-', 'KC_VOLU': 'Vol+',
    'KC_MPRV': 'Prev', 'KC_MPLY': 'Play', 'KC_MNXT': 'Next', 'KC_BRID': 'Bri-',
    'KC_BRIU': 'Bri+', 'KC_SCRN': 'Scrn', 'KC_AIRP': 'Airp', 'KC_PSCR': 'PrtSc',
    'KC_MSEL': 'MSel', 'KC_DEL': 'Del', 'KC_GRV': '`', 'KC_MINS': '-',
    'KC_EQL': '=', 'KC_BSPC': 'Bksp', 'KC_TAB': 'Tab', 'KC_QUOT': "'",
    'KC_COMM': ',', 'KC_DOT': '.', 'KC_SLSH': '/', 'KC_BSLS': '\\',
    'KC_LBRC': '[', 'KC_RBRC': ']', 'KC_SCLN': ';', 'KC_ENT': 'Enter',
    'KC_LSFT': 'Shift', 'KC_RSFT': 'Shift', 'KC_LCTL': 'Ctrl', 'KC_RCTL': 'Ctrl',
    'KC_LGUI': 'GUI', 'KC_LALT': 'Alt', 'KC_RALT': 'Alt', 'KC_SPC': 'Space',
    'KC_LEFT': '\u2190', 'KC_RGHT': '\u2192', 'KC_UP': '\u2191', 'KC_DOWN': '\u2193',
    'KC_CAPS': 'Caps', 'KC_INS': 'Ins', 'KC_PAUS': 'Pause', 'KC_BRK': 'Brk',
    'KC_SCRL': 'ScrLk', 'KC_HOME': 'Home', 'KC_END': 'End',
    'KC_PGUP': 'PgUp', 'KC_PGDN': 'PgDn', 'KC_SYSREQ': 'SysRq',
    'FN_LOCK': 'FnLk', 'QK_BOOT': 'BOOT', 'BL_STEP': 'BL Step',
    'BL_BRTG': 'BLTog', 'RGB_TOG': 'RGB', 'RGB_MOD': 'RMod', 'RGB_RMOD': 'RMod',
    'RGB_HUI': 'Hue+', 'RGB_HUD': 'Hue-', 'RGB_SAI': 'Sat+', 'RGB_SAD': 'Sat-',
    'RGB_SPI': 'Spd+', 'RGB_SPD': 'Spd-', 'RGB_VAI': 'Brt+', 'RGB_VAD': 'Brt-',
    '_______': '', 'KC_NO': '',
}
for i in range(1, 13):
    KC_LABELS[f'KC_F{i}'] = f'F{i}'
for i in range(10):
    KC_LABELS[f'KC_{i}'] = str(i)
for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
    KC_LABELS[f'KC_{c}'] = c


def label_for(kc):
    """Convert a QMK keycode to a display label."""
    if kc in KC_LABELS:
        return KC_LABELS[kc]
    m = re.match(r'MO\((\w+)\)', kc)
    if m:
        name = m.group(1).lstrip('_')
        return f'FN' if 'FN' in name or 'FM' in name else name
    m = re.match(r'TG\((\w+)\)', kc)
    if m:
        name = m.group(1).lstrip('_')
        return f'TG\n{name}'
    m = re.match(r'TO\((\w+)\)', kc)
    if m:
        name = m.group(1).lstrip('_')
        return f'TO\n{name}'
    m = re.match(r'DF\((\w+)\)', kc)
    if m:
        name = m.group(1).lstrip('_')
        return f'DF\n{name}'
    return kc


# --- Color rules ---
# LED indices from ansi.c matrix-to-LED mapping
LED_INDEX = {
    'home_left':  43,  # K34: U (Dvorak) / F (QWERTY)
    'home_right': 49,  # K37: H (Dvorak) / J (QWERTY)
    'caps':       44,  # K30: Caps Lock position
    'lshift':     32,  # K44
    'rshift':     92,  # K57
    'lctrl':      34,  # K58
    'fn':         93,  # K59
    'gui':        46,  # K127
    'lalt':       47,  # K60
    'ralt':       84,  # K62
    'rctrl':      88,  # K64
    'tg_qw':       5,  # K17 position on FN layer
    'boot':       55,  # K29 position on FN layer
}

# Map LAYOUT position index to LED index (from matrix)
# Computed from the ansi.h LAYOUT macro -> matrix positions -> LED indices
LAYOUT_POS_TO_LED = {
    # Function row (positions 0-13)
    0: 25, 1: 21, 2: 19, 3: 18, 4: 20, 5: 22, 6: 24, 7: 26, 8: 67, 9: 74, 10: 68, 11: 66, 12: 70, 13: 73,
    # Number row (14-27)
    14: 16, 15: 15, 16: 13, 17: 12, 18: 11, 19: 9, 20: 14, 21: 10, 22: 17, 23: 69, 24: 61, 25: 63, 26: 62, 27: 65,
    # Top row (28-41)
    28: 1, 29: 5, 30: 3, 31: 2, 32: 4, 33: 7, 34: 8, 35: 6, 36: 58, 37: 59, 38: 60, 39: 57, 40: 54, 41: 55,
    # Home row (42-54)
    42: 44, 43: 36, 44: 41, 45: 37, 46: 43, 47: 39, 48: 40, 49: 49, 50: 50, 51: 51, 52: 48, 53: 52, 54: 56,
    # Shift row (55-66)
    55: 32, 56: 27, 57: 29, 58: 31, 59: 33, 60: 35, 61: 76, 62: 77, 63: 78, 64: 75, 65: 79, 66: 92,
    # Bottom row (67-77) — note: arrows are special
    67: 34, 68: 93, 69: 46, 70: 47, 71: 94, 72: 84, 73: 88, 74: 89, 75: 81, 76: 90, 77: 91,
}

# Colors as hex strings
COLORS = {
    'cyan':     '#00b4b4',
    'orange':   '#ff7800',
    'purple':   '#8000ff',
    'blue':     '#0050ff',
    'green':    '#00b400',
    'amber':    '#ffa000',
    'dim_wht':  '#646464',
    'yellow':   '#ffff00',
    'red':      '#ff0000',
    'key':      '#2d2d2d',
    'fn_key':   '#1a3a4a',
    'dim':      '#1a1a1a',
}

# Position index -> color name mapping for each layer type
def get_key_color(pos_idx, layer_name, kc, is_qwerty_active=False):
    """Return (fill_color, stroke_color) for a key based on position and layer."""
    led = LAYOUT_POS_TO_LED.get(pos_idx)

    # Check specific LED-based colors
    # Home row index fingers
    if led in (LED_INDEX['home_left'], LED_INDEX['home_right']):
        if is_qwerty_active:
            return COLORS['orange'], '#ff9944'
        return COLORS['cyan'], '#44dddd'

    # FN layer specials
    if layer_name in ('_FN', '_FM'):
        if led == LED_INDEX['tg_qw'] and kc.startswith('TG('):
            return COLORS['yellow'], '#cccc00'
        if led == LED_INDEX['boot'] and kc == 'QK_BOOT':
            return COLORS['red'], '#cc0000'
        if led == LED_INDEX['caps'] and kc == 'KC_CAPS':
            return COLORS['green'], '#44aa44'

    # Modifier colors (by LED index)
    if led in (LED_INDEX['caps'], LED_INDEX['lctrl'], LED_INDEX['rctrl']):
        return COLORS['blue'], '#4488ff'
    if led in (LED_INDEX['lshift'], LED_INDEX['rshift']):
        return COLORS['green'], '#44aa44'
    if led in (LED_INDEX['lalt'], LED_INDEX['ralt']):
        return COLORS['amber'], '#cc8800'
    if led == LED_INDEX['gui']:
        return COLORS['dim_wht'], '#888888'
    if led == LED_INDEX['fn']:
        return COLORS['purple'], '#aa44ff'

    # Transparent / empty keys on overlay layers
    if kc in ('_______', 'KC_NO') and layer_name not in ('_DVORAK',):
        return '#1a1a1a', '#333333'

    # Default
    return COLORS['key'], '#555555'


def parse_keymap(filename):
    """Parse keymap.c and extract layer names and keycodes."""
    with open(filename) as f:
        content = f.read()

    # Extract enum layer names
    enum_match = re.search(r'enum\s+_layers\s*\{([^}]+)\}', content)
    layer_names = [n.strip().rstrip(',') for n in enum_match.group(1).split('\n') if n.strip() and not n.strip().startswith('//')]

    # Extract LAYOUT contents for each layer
    layers = {}
    for name in layer_names:
        # Find the start of LAYOUT(
        pattern = rf'\[{re.escape(name)}\]\s*=\s*LAYOUT\s*\('
        m = re.search(pattern, content)
        if m:
            start = m.end()
            # Find matching closing paren by counting depth
            depth = 1
            i = start
            while i < len(content) and depth > 0:
                if content[i] == '(':
                    depth += 1
                elif content[i] == ')':
                    depth -= 1
                i += 1
            body = content[start:i-1]
            # Remove comments
            body = re.sub(r'//[^\n]*', '', body)
            body = re.sub(r'/\*.*?\*/', '', body, flags=re.DOTALL)
            keycodes = [kc.strip() for kc in body.split(',') if kc.strip()]
            layers[name] = keycodes

    # Extract subtitle info from comments
    subtitle_parts = []
    if any('KC_LCTL' == kc for layer in layers.values() for kc in layer):
        # Check if caps position has ctrl
        for name, keycodes in layers.items():
            if len(keycodes) > 42 and keycodes[42] == 'KC_LCTL':
                subtitle_parts.append('Caps Lock \u2192 LCtrl')
                break
    for name, keycodes in layers.items():
        for kc in keycodes:
            if kc.startswith('TG(') and 'QWERTY' in kc:
                subtitle_parts.append("FN+' \u2192 Toggle QWERTY")
                break
    for name, keycodes in layers.items():
        if 'QK_BOOT' in keycodes:
            subtitle_parts.append('FN+\\ \u2192 Bootloader')
            break
    for name, keycodes in layers.items():
        if name in ('_FN', '_FM') and len(keycodes) > 42 and keycodes[42] == 'KC_CAPS':
            subtitle_parts.append('FN+Caps \u2192 Caps Lock')
            break

    return layers, layer_names, subtitle_parts


def render_row_keys(keys, widths, x_start, y, height, layer_name, pos_offset, is_qwerty=False):
    """Render a row of keys, returning SVG elements."""
    elems = []
    x = x_start
    for i, (kc, w) in enumerate(zip(keys, widths)):
        pos_idx = pos_offset + i
        pw = w * UNIT + (w - 1) * GAP if w > 1 else UNIT
        fill, stroke = get_key_color(pos_idx, layer_name, kc, is_qwerty)
        label = label_for(kc)

        elems.append(f'    <rect x="{x}" y="{y}" width="{pw}" height="{height}" '
                     f'fill="{fill}" stroke="{stroke}" rx="4"/>')

        if label:
            lines = label.split('\n')
            if len(lines) == 1:
                fs = 10 if len(label) <= 5 else 8
                elems.append(f'    <text x="{x + pw/2}" y="{y + height/2}" '
                             f'text-anchor="middle" dominant-baseline="central" '
                             f'fill="#e0e0e0" font-size="{fs}px">{esc(label)}</text>')
            else:
                elems.append(f'    <text x="{x + pw/2}" y="{y + height/2 - 6}" '
                             f'text-anchor="middle" dominant-baseline="central" '
                             f'fill="#e0e0e0" font-size="10px">{esc(lines[0])}</text>')
                elems.append(f'    <text x="{x + pw/2}" y="{y + height/2 + 6}" '
                             f'text-anchor="middle" dominant-baseline="central" '
                             f'fill="#888888" font-size="8px">{esc(lines[1])}</text>')

        # Annotation for caps->ctrl remap
        if pos_idx == 42 and kc == 'KC_LCTL' and layer_name in ('_DVORAK', '_QWERTY'):
            elems.append(f'    <text x="{x + pw/2}" y="{y + height/2 + 8}" '
                         f'text-anchor="middle" fill="#888" font-size="7px">(Caps)</text>')
            # Adjust main label up
            elems[-2] = elems[-2].replace(f'y="{y + height/2}"', f'y="{y + height/2 - 5}"')

        x += pw + GAP
    return elems, x


def esc(s):
    """Escape XML special characters."""
    return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;').replace("'", '&apos;').replace('"', '&quot;')


def render_layer(layer_name, keycodes, x_base, y_base, title, is_qwerty=False, compact=False):
    """Render a full layer as SVG elements."""
    elems = []
    elems.append(f'  <text x="{x_base}" y="{y_base}" fill="#e0e0e0" font-size="16px" '
                 f'font-weight="bold">{esc(title)}</text>')

    y = y_base + 12
    pos = 0
    row_heights = [FN_ROW_HEIGHT, KEY_HEIGHT, KEY_HEIGHT, KEY_HEIGHT, KEY_HEIGHT, KEY_HEIGHT]
    if compact:
        row_heights = [FN_ROW_HEIGHT, 32, 32, 32, 24, 10]

    for row_idx, (size, widths) in enumerate(zip(ROW_SIZES, ROW_WIDTHS)):
        row_keys = keycodes[pos:pos + size]
        h = row_heights[row_idx]

        # Bottom row special: arrows have split up/down
        if row_idx == 5 and len(row_keys) >= 11:
            # Render keys before arrow cluster (Ctrl, FN, GUI, Alt, Space, RAlt, RCtl)
            main_keys = row_keys[:7]
            main_widths = widths[:7]
            row_elems, x_end = render_row_keys(main_keys, main_widths, x_base, y, h,
                                                layer_name, pos, is_qwerty)
            elems.extend(row_elems)

            # Left arrow (pos+7)
            kc = row_keys[7]
            pw = widths[7] * UNIT
            fill, stroke = get_key_color(pos + 7, layer_name, kc, is_qwerty)
            label = label_for(kc)
            elems.append(f'    <rect x="{x_end}" y="{y}" width="{pw}" height="{h}" '
                         f'fill="{fill}" stroke="{stroke}" rx="4"/>')
            if label:
                elems.append(f'    <text x="{x_end + pw/2}" y="{y + h/2}" text-anchor="middle" '
                             f'dominant-baseline="central" fill="#e0e0e0" font-size="10px">{esc(label)}</text>')
            x_arr = x_end + pw + GAP

            # Up arrow - top half (pos+8)
            kc_up = row_keys[8]
            pw_arr = widths[8] * UNIT
            fill_up, stroke_up = get_key_color(pos + 8, layer_name, kc_up, is_qwerty)
            label_up = label_for(kc_up)
            elems.append(f'    <rect x="{x_arr}" y="{y}" width="{pw_arr}" height="{HALF_KEY_HEIGHT}" '
                         f'fill="{fill_up}" stroke="{stroke_up}" rx="4"/>')
            if label_up:
                elems.append(f'    <text x="{x_arr + pw_arr/2}" y="{y + HALF_KEY_HEIGHT/2}" text-anchor="middle" '
                             f'dominant-baseline="central" fill="#e0e0e0" font-size="10px">{esc(label_up)}</text>')

            # Down arrow - bottom half (pos+9)
            kc_dn = row_keys[9]
            fill_dn, stroke_dn = get_key_color(pos + 9, layer_name, kc_dn, is_qwerty)
            label_dn = label_for(kc_dn)
            y_dn = y + HALF_KEY_HEIGHT + 2
            elems.append(f'    <rect x="{x_arr}" y="{y_dn}" width="{pw_arr}" height="{HALF_KEY_HEIGHT}" '
                         f'fill="{fill_dn}" stroke="{stroke_dn}" rx="4"/>')
            if label_dn:
                elems.append(f'    <text x="{x_arr + pw_arr/2}" y="{y_dn + HALF_KEY_HEIGHT/2}" text-anchor="middle" '
                             f'dominant-baseline="central" fill="#e0e0e0" font-size="10px">{esc(label_dn)}</text>')

            # Right arrow (pos+10)
            x_right = x_arr + pw_arr + GAP
            kc_right = row_keys[10]
            pw_right = widths[10] * UNIT
            fill_r, stroke_r = get_key_color(pos + 10, layer_name, kc_right, is_qwerty)
            label_r = label_for(kc_right)
            elems.append(f'    <rect x="{x_right}" y="{y}" width="{pw_right}" height="{h}" '
                         f'fill="{fill_r}" stroke="{stroke_r}" rx="4"/>')
            if label_r:
                elems.append(f'    <text x="{x_right + pw_right/2}" y="{y + h/2}" text-anchor="middle" '
                             f'dominant-baseline="central" fill="#e0e0e0" font-size="10px">{esc(label_r)}</text>')
        else:
            row_elems, _ = render_row_keys(row_keys, widths, x_base, y, h,
                                           layer_name, pos, is_qwerty)
            elems.extend(row_elems)

        y += h + GAP
        pos += size

    return elems, y


def render_legend(x, y):
    """Render a color legend."""
    items = [
        (COLORS['cyan'], 'Home index (Dvorak)'),
        (COLORS['orange'], 'Home index (QWERTY)'),
        (COLORS['blue'], 'Ctrl'),
        (COLORS['green'], 'Shift'),
        (COLORS['amber'], 'Alt'),
        (COLORS['dim_wht'], 'GUI'),
        (COLORS['purple'], 'FN'),
        (COLORS['yellow'], 'TG QWERTY (FN)'),
        (COLORS['red'], 'BOOT (FN)'),
        (COLORS['key'], 'Normal key'),
        ('#1a1a1a', 'Transparent'),
    ]
    elems = []
    elems.append(f'  <text x="{x}" y="{y}" fill="#aaa" font-size="12px">LED Colors</text>')
    for i, (color, label) in enumerate(items):
        iy = y + 14 + i * 18
        elems.append(f'  <rect x="{x}" y="{iy}" width="14" height="12" fill="{color}" '
                     f'stroke="#555" rx="2"/>')
        elems.append(f'  <text x="{x + 22}" y="{iy + 6}" fill="#888" font-size="9px" '
                     f'text-anchor="start" dominant-baseline="central">{esc(label)}</text>')
    return elems


def main():
    keymap_file = sys.argv[1] if len(sys.argv) > 1 else 'keymap.c'
    output_file = sys.argv[2] if len(sys.argv) > 2 else 'keymap_diagram.svg'

    layers, layer_names, subtitle_parts = parse_keymap(keymap_file)

    # Decide which layers to render
    render_list = []
    for name in layer_names:
        if name in layers:
            if name == '_DVORAK':
                render_list.append((name, 'Dvorak Base Layer (_DVORAK)', False, False))
            elif name == '_FN':
                render_list.append((name, 'FN Layer (hold FN)', False, True))
            elif name == '_QWERTY':
                render_list.append((name, 'QWERTY Overlay (toggle FN+\')', True, True))

    # Calculate total height
    margin = 20
    header_height = 50
    layer_spacing = 15
    # Each full layer: ~12 + 24 + 5*(42+4) + some padding
    full_layer_height = 260
    compact_layer_height = 180
    total_height = header_height + margin
    for _, _, _, compact in render_list:
        total_height += (compact_layer_height if compact else full_layer_height) + layer_spacing
    total_height += margin

    width = 900

    elems = []
    elems.append(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {width} {total_height}" '
                 f'font-family="monospace" font-size="11">')
    elems.append(f'  <rect width="{width}" height="{total_height}" fill="#1a1a1a" rx="8"/>')

    # Subtitle
    subtitle = ' | '.join(subtitle_parts)
    elems.append(f'  <text x="{margin}" y="{margin + 15}" fill="#666" font-size="9px">{esc(subtitle)}</text>')

    y = margin + header_height - 20

    for layer_name, title, is_qwerty, compact in render_list:
        layer_elems, y_end = render_layer(layer_name, layers[layer_name],
                                          margin, y, title, is_qwerty, compact)
        elems.extend(layer_elems)
        y = y_end + layer_spacing

    # Legend
    elems.extend(render_legend(width - 170, margin + 40))

    elems.append('</svg>')

    svg = '\n'.join(elems)
    with open(output_file, 'w') as f:
        f.write(svg)
    print(f'Written to {output_file}', file=sys.stderr)


if __name__ == '__main__':
    main()
