# Framework 16 Macropad Cheat Sheet

## Overview

A multi-layer macropad configuration for Emacs-centric development, with layers for cursor navigation, structured code navigation, and window management.

**Physical Layout**: 6 rows × 4 columns  
**Active Layers**: 0 (Default), 2 (Code Nav), 3 (Numpad), 4 (Window Nav), 5 (Window Mutate)

---

## Layer Switching

| Key Position | Action |
|--------------|--------|
| Top row | Layer switch buttons (TO) |
| Bottom row | Layer switches + toggles between L4/L5 |

---

## Layer 0 — Default

Basic cursor navigation, similar to stardard keypad in nav mode.

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  layer0     │  layer2     │  layer4     │  layer3     │  ← Layer switches
├─────────────┼─────────────┼─────────────┼─────────────┤
│ prev-buffer │   ibuffer   │    dired    │ next-buffer │  ← Shared (HYPR)
├─────────────┼─────────────┼─────────────┼─────────────┤
│    PgUp     │      ↑      │    PgDn     │ imenu-multi │
├─────────────┼─────────────┼─────────────┼─────────────┤
│    ←        │      ↓      │     →       │   imenu     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│   Home      │   layer2    │    End      │   imenu     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  layer2     │  layer4     │             │             │  ← Layer switches
└─────────────┴─────────────┴─────────────┴─────────────┘
```

---

## Layer 2 — Structured Code Navigation

Navigate by functions and expressions, not characters. 

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  layer0     │ layer2      │  layer4     │ layer3      │
├─────────────┼─────────────┼─────────────┼─────────────┤
│ prev-buffer │   ibuffer   │    dired    │ next-buffer │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  expand-    │  ↑ begin-   │  mark-      │ imenu-multi │
│  region     │  defun      │  defun      │             │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  ← back-    │  ↓ end-     │  forward- → │  imenu      │
│  sexp       │  defun      │  sexp       │             │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  deadgrep   │  consult-   │  consult-   │  imenu      │
│             │  ripgrep    │  find       │             │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  layer2     │  layer4     │             │             │
└─────────────┴─────────────┴─────────────┴─────────────┘
```

### Layer 2 Quick Reference

|-----|---------------|----------|
| H-a | `previous-buffer` | Switch to previous buffer |
| H-b | `ibuffer` | Open buffer list |
| H-c | `dired` | Open directory editor |
| H-d | `next-buffer` | Switch to next buffer |
| H-e | `er/expand-region` | Expand selection outward |
| H-f | `treesit-beginning-of-defun` | Jump to start of function |
| H-g | `mark-defun` | Select entire function |
| H-h | `consult-imenu-multi` | imenu of all buffers in project  |
| H-i | `treesit-backward-sexp` | Jump to previous expression |
| H-j | `treesit-end-of-defun` | Jump to end of function |
| H-k | `treesit-forward-sexp` | Jump to next expression |
| H-l | `consult-imenu` | Jump to symbol in buffer |
| H-m | `deadgrep` | Persistent ripgrep search buffer |
| H-n | `consult-ripgrep` | Quick project-wide text search |
| H-o | `consult-find` | Find files by name |
| H-p | `consult-imenu` | Jump to symbol in buffer |

---

## Layer 3 — Numpad

Standard numeric keypad layout with NumLock toggle. Rarely used.

---

## Layer 4 — Window Navigation

Move focus between windows without changing layout.

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  layer0     │ layer2      │ layer4      │ layer3      │
├─────────────┼─────────────┼─────────────┼─────────────┤
│             │             │             │             │
├─────────────┼─────────────┼─────────────┼─────────────┤
│             │  windmove   │             │  winner-    │
│             │     ↑       │             │  redo ↷     │
│             │     L-f     │             │     L-h     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  windmove   │  windmove   │  windmove   │  winner-    │
│     ←       │     ↓       │     →       │  undo ↶     │
│     L-i     │     L-j     │     L-k     │     L-l     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│             │             │             │             │
├─────────────┼─────────────┼─────────────┼─────────────┤
│   layer0    │ layer5      │ layer5      │ layer5      │  ← Toggle Window Mutate
└─────────────┴─────────────┴─────────────┴─────────────┘
```

### Layer 4 Quick Reference

| Key | Emacs Binding | Function |
|-----|---------------|----------|
| L-f | `windmove-up` | Move focus to window above |
| L-i | `windmove-left` | Move focus to window left |
| L-j | `windmove-down` | Move focus to window below |
| L-k | `windmove-right` | Move focus to window right |
| L-h | `winner-redo` | Redo window layout change |
| L-l | `winner-undo` | Undo window layout change |

---

## Layer 5 — Window Mutation

Resize, split, and delete windows.

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  layer0     │ layer2      │ layer4      │ layer3      │
├─────────────┼─────────────┼─────────────┼─────────────┤
│             │             │             │             │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  split-     │  enlarge    │  split-     │  winner-    │
│  below ┬    │  vert ↑     │  right ┤    │  redo ↷     │
│     M-e     │     M-f     │     M-g     │     M-h     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  shrink     │  shrink     │  enlarge    │  winner-    │
│  horiz ←    │  vert ↓     │  horiz →    │  undo ↶     │
│     M-i     │     M-j     │     M-k     │     M-l     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│  maximize   │  balance    │  delete-    │  delete-    │
│  window     │  windows =  │  window ✕   │  other ✕✕   │
│     M-m     │     M-n     │     M-o     │     M-p     │
├─────────────┼─────────────┼─────────────┼─────────────┤
│   layer0    │ layer4      │  layer4     │   layer4    │  ← To Window Nav
└─────────────┴─────────────┴─────────────┴─────────────┘
```

### Layer 5 Quick Reference

| Key | Emacs Binding | Function |
|-----|---------------|----------|
| M-e | `split-window-below` | Split horizontally (new window below) |
| M-f | `enlarge-window` | Make window taller |
| M-g | `split-window-right` | Split vertically (new window right) |
| M-h | `winner-redo` | Redo window layout change |
| M-i | `shrink-window-horizontally` | Make window narrower |
| M-j | `shrink-window` | Make window shorter |
| M-k | `enlarge-window-horizontally` | Make window wider |
| M-l | `winner-undo` | Undo window layout change |
| M-m | `maximize-window` | Maximize current window |
| M-n | `balance-windows` | Make all windows equal size |
| M-o | `delete-window` | Close current window |
| M-p | `delete-other-windows` | Close all other windows |

---

## Modifier Legend

| Abbreviation | QMK Macro | Modifiers | Emacs Notation |
|--------------|-----------|-----------|----------------|
| H- (HYPR) | `HYPR()` | Ctrl+Alt+Shift+Super | `C-M-S-s-` |
| L- (LCAG) | `LCAG()` | Ctrl+Alt+Super | `C-M-s-` |
| M- (MEH) | `MEH()` | Ctrl+Alt+Shift | `C-M-S-` |

---

## Design Principles

1. **Spatial consistency**: Arrow-key positions have directional meaning across layers
2. **Semantic layering**: L4 = read-only window ops, L5 = mutating window ops
3. **Shared row**: Buffer switching (row 2) available on multiple layers via HYPR
4. **Undo/redo anchored**: Winner-undo/redo in same position on both window layers
5. **Progressive disclosure**: Master L0 and L2 first, add L4/L5 as needed
