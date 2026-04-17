# QMK keymaps for the Framework 16

Custom QMK keymaps for the Framework Laptop 16 keyboard module and RGB macropad module.

- [`framework-keymap/`](framework-keymap/) — ANSI Dvorak main keyboard
- [`framework-macropad-keymap/`](framework-macropad-keymap/) — 4×6 RGB macropad (emacs-tuned layers)

Each keymap directory has its own README covering bindings and layout. This README covers the shared build/flash workflow.

## Prerequisites

- `arm-none-eabi-gcc` (ARM cross-compiler toolchain)
- `python3` (≥ 3.7)
- `git`, `make`
- An unrelated-enough machine to survive the 2+ GB firmware tree

## One-time setup

Clone Framework's QMK fork into `qmk_firmware/` (pinned to the known-good tag `v0.3.1`) and apply any required patches:

```sh
./setup
```

The script is idempotent — re-run it to update or recover after manual changes. It:

1. Clones `https://github.com/FrameworkComputer/qmk_firmware.git` (or `git fetch --tags` if already cloned),
2. Checks out `$QMK_REF` (edit `setup` to bump),
3. Runs `make git-submodule`,
4. Applies every patch in `patches/` (silently skipping any that don't apply cleanly — useful when bumping `$QMK_REF`).

The `qmk_firmware/` directory is `.gitignore`d; `setup` is the source of truth.

## Building a keymap

Each keymap directory ships an `install` script that copies `keymap.c` + `rules.mk` into the QMK tree:

```sh
./framework-keymap/install              # or framework-macropad-keymap/install
cd qmk_firmware
make framework/ansi:david               # or framework/macropad:david
```

Output lands at `qmk_firmware/framework_ansi_david.uf2` (or `framework_macropad_david.uf2`).

## Flashing

1. Enter the board's bootloader:
   - **Main keyboard:** press `FN+\` (or run `sudo qmk_hid via --bootloader`).
   - **Macropad:** hold the "2" and "6" keys while re-attaching the module, or press the bootmagic key at power-on.
2. The device appears as a USB mass-storage drive.
3. Drag the `.uf2` file to the mount.
4. The device reboots automatically.

## Regenerating layout diagrams

Each keymap directory has its own generator (the layouts are structurally too different to share one script):

```sh
cd framework-keymap           && python3 gen_keymap_svg.py
cd framework-macropad-keymap  && python3 gen_macropad_svg.py
```

Generated `*.svg` files are checked in so GitHub renders them in the keymap READMEs.

## Patches (`patches/`)

Small, focused patches applied by `setup` after checkout. Each is a `git format-patch`-style `.patch` file.

- `0001-python-3.12-ast-Num-fix.patch` — QMK `v0.3.1` predates Python 3.12's removal of `ast.Num`; without this, `info.json` processing fails and the build errors with "Platform not defined."

When bumping `$QMK_REF`, a patch that no longer applies cleanly will be skipped (with a message). Revisit `patches/` — it may no longer be needed, or may need to be refreshed.

## Layout

```
qmk/
├── README.md                 # (this file)
├── setup                     # clone/update qmk_firmware, apply patches
├── patches/                  # patches applied by setup
├── qmk_firmware/             # (gitignored, populated by setup)
├── framework-keymap/         # main keyboard (ANSI Dvorak)
│   ├── README.md
│   ├── install
│   ├── keymap.c
│   ├── rules.mk
│   ├── gen_keymap_svg.py
│   └── keymap_diagram.svg
└── framework-macropad-keymap/
    ├── README.md
    ├── install
    ├── keymap.c
    ├── rules.mk
    ├── gen_macropad_svg.py
    └── keymap_diagram.svg
```
