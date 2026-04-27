## Structured Code Navigation

Navigate by functions and expressions, not characters. 

```
┌─────────────┬─────────────┬─────────────┬─────────────┐
│  numpad     │ codenav     │  win nav    │ win mutate  │
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
│  Home       │  PgUp       │ PgDn        │ End         │
└─────────────┴─────────────┴─────────────┴─────────────┘
```

### Quick Reference

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

