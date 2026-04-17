# Framework 16 ANSI Dvorak Keymap

Custom QMK keymap for the Framework Laptop 16 keyboard module (ANSI layout).

See the [top-level `qmk/README.md`](../README.md) for build, flash, and diagram-generation instructions.

## Layers

- **`_DVORAK`** — Base layer; Dvorak with Caps Lock remapped to LCtrl.
- **`_FN`** — Hold FN: F-keys, RGB/backlight controls, navigation.
- **`_FN_LOCK`** — Persistent F-key mode (FN+Esc from `_FN` to toggle on; FN+Esc from `_FM` to toggle off).
- **`_FM`** — Accessed via MO from `_FN_LOCK`; restores media keys and navigation while F-Lock is active.
- **`_QWERTY`** — Toggleable overlay (FN+Tab from `_FN`).

See [`keymap_diagram.svg`](keymap_diagram.svg) for a visual reference.

## Notable bindings

| Combo | Action |
|---|---|
| FN + Esc | Toggle FN Lock |
| FN + Tab | Toggle QWERTY overlay |
| FN + Caps position | Caps Lock |
| FN + `\` | Enter bootloader (`QK_BOOT`) |
| FN + Brt+/Brt- | RGB indicator brightness |

## RGB indicators

Only specific keys are lit — all others are off.

| Key | Color |
|---|---|
| Home index fingers | Cyan (Dvorak) / Orange (QWERTY) |
| Ctrl | Blue |
| Shift | Green |
| Alt | Amber |
| GUI | Dim white |
| FN | Purple |
| TG QWERTY (FN layer) | Yellow |
| QK_BOOT (FN layer) | Red |
