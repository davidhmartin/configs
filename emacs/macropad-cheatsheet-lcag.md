## Window Navigation

Move focus between windows without changing layout.

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  numpad     │ codenav     │  win nav    │ win mutate  │
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
│  Home       │  PgUp       │ PgDn        │ End         │
└─────────────┴─────────────┴─────────────┴─────────────┘
```

### Quick Reference

| Key | Emacs Binding | Function |
|-----|---------------|----------|
| L-f | `windmove-up` | Move focus to window above |
| L-i | `windmove-left` | Move focus to window left |
| L-j | `windmove-down` | Move focus to window below |
| L-k | `windmove-right` | Move focus to window right |
| L-h | `winner-redo` | Redo window layout change |
| L-l | `winner-undo` | Undo window layout change |

