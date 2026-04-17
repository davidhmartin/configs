# Framework 16 ANSI Dvorak Keymap

Custom QMK keymap for the Framework Laptop 16 keyboard module (ANSI layout).

## Layout

- **Base layer:** Dvorak with Caps Lock remapped to LCtrl
- **FN layer:** F-keys, RGB/backlight controls, navigation
- **FN Lock:** Persistent F-key mode (FN+Esc to toggle)
- **QWERTY overlay:** Toggle with FN+Tab

See `keymap_diagram.svg` for a visual reference.

### Notable bindings

| Combo | Action |
|---|---|
| FN + Esc | Toggle FN Lock |
| FN + Tab | Toggle QWERTY overlay |
| FN + Caps position | Caps Lock |
| FN + \\ | Enter bootloader (QK_BOOT) |
| FN + Brt+/Brt- | RGB indicator brightness |

### RGB indicators

Only specific keys are lit — all others are off:

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

## Building

```bash
# Clone Framework's QMK fork
git clone https://github.com/FrameworkComputer/qmk_firmware.git
cd qmk_firmware
git checkout v0.3.1  # or latest tag
git submodule update --init --recursive

# Install the keymap
mkdir -p keyboards/framework/ansi/keymaps/david
cp /path/to/this/repo/keymap.c keyboards/framework/ansi/keymaps/david/
cp /path/to/this/repo/rules.mk keyboards/framework/ansi/keymaps/david/

# Build (requires arm-none-eabi-gcc)
make framework/ansi:david
```

Output: `framework_ansi_david.uf2`

## Flashing

1. Enter bootloader mode: press FN+\\ (or `sudo qmk_hid via --bootloader`)
2. A USB mass storage device appears
3. Copy the `.uf2` file to the mounted device
4. Keyboard reboots automatically

## Regenerating the diagram

```bash
python3 gen_keymap_svg.py keymap.c keymap_diagram.svg
```
