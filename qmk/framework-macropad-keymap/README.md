# Framework 16 RGB Macropad Keymap

Custom QMK keymap for the Framework Laptop 16 RGB macropad module (6×4 grid, RP2040, IS31FL3743A driver).

Designed to pair with a companion Emacs config ([`emacs/macropad.el`](../../emacs/macropad.el)) — three of the five layers emit a fixed modifier combo plus a letter A–P across the 4×4 middle grid, and Emacs picks up whatever bindings it cares about.

See the [top-level `qmk/README.md`](../README.md) for build, flash, and diagram-generation instructions.

## Layers

| Idx | Enum | Role |
|---|---|---|
| 0 | `_NUM_ON` | Numpad (matches OS Num Lock = on) |
| 1 | `_NUM_OFF` | Numpad-off / navigation (OS Num Lock = off) |
| 2 | `_HYPR` | 4×4 grid emits `HYPR(A..P)` = Ctrl+Alt+Shift+Super+letter |
| 3 | `_LCAG` | 4×4 grid emits `LCAG(A..P)` = Ctrl+Alt+Super+letter |
| 4 | `_MEH` | 4×4 grid emits `MEH(A..P)` = Ctrl+Alt+Shift+letter |

See [`keymap_diagram.svg`](keymap_diagram.svg) for a visual reference.

## Top row — layer switches on every layer

| Slot | Keycode | Behavior |
|---|---|---|
| H1 | `KC_NUM` | Toggles OS Num Lock and moves to `_NUM_ON` or `_NUM_OFF` to match. |
| H2 | `TO(_HYPR)` | Switch to HYPR layer. |
| H3 | `TO(_LCAG)` | Switch to LCAG layer. |
| H4 | `TO(_MEH)` | Switch to MEH layer. |

The RGB theme color of the active layer's top-row key lights up; other top-row keys stay dark.

## Bottom row

- `_NUM_ON`: `0`, `0`, `.`, `Enter` (full numpad bottom row).
- All other layers: `Home`, `PgUp`, `PgDn`, `End`.

## Middle 4×4 grid

### `_NUM_ON` (Num Lock on)

Standard numpad layout:
```
Num  /  *  =
 7   8  9  -
 4   5  6  +
 1   2  3  Enter
```

### `_NUM_OFF` (Num Lock off)

All middle-grid keys are transparent (`_______`) and fall through to `_NUM_ON`'s numpad keycodes. The OS reinterprets `KC_P4..KC_P9` as `←`, `↓`, `→`, `Home`, `↑`, `PgUp` while Num Lock is off, so you effectively get a nav-cluster.

### `_HYPR` / `_LCAG` / `_MEH` — alphabetic stamp

The 4×4 grid emits letters A through P in reading order with a fixed modifier combo:

```
A  B  C  D
E  F  G  H
I  J  K  L
M  N  O  P
```

Emacs binds whichever combinations it wants; keys with no binding on the host just do nothing.

## RGB color scheme

| Layer | Top-row key lit | Color |
|---|---|---|
| `_NUM_ON` | H1 | Cyan |
| `_NUM_OFF` | H1 | Green |
| `_HYPR` | H2 | Purple |
| `_LCAG` | H3 | Blue |
| `_MEH` | H4 | Amber |

All other LEDs are off (any RGB matrix animation is suppressed by the indicator function).

## Design notes

- **Layer switching is explicit, not modifier-hold.** `TO(...)` on H2/H3/H4 sets the layer persistently. Tap to enter, tap H1 (or H2–H4 again) to switch back.
- **`KC_NUM` is intercepted by `process_record_user`**, not driven by `led_update_user`. The keypress triggers `layer_move` directly, so layer changes aren't vulnerable to spurious host LED-state echoes that would otherwise yank you off `_HYPR`/`_LCAG`/`_MEH`.
- **`led_update_user` only acts when already on a numpad layer**, so external Num Lock changes don't hijack the Emacs-oriented layers.
- **Layer enum indices align with the keyboard-level `_NUMLOCK=0` / `_FN=1`** defined in `keyboards/framework/macropad/macropad.h`, so the board's built-in `keyboard_post_init_user` still does the right thing on boot.
