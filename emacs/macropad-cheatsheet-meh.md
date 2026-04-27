## Window Mutation

Resize, split, and delete windows.

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  numpad     │ codenav     │  win nav    │ win mutate  │
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
│  Home       │  PgUp       │ PgDn        │ End         │
└─────────────┴─────────────┴─────────────┴─────────────┘
```

### Quick Reference

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

