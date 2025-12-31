;;; hydras.el --- Hydra definitions for organized key bindings -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains all hydra definitions for improved discoverability
;; and organization of key bindings.

;;; Code:

;; Main entry point hydra - accessible via a single key
(defhydra hydra-main (:color teal :hint nil)
  "
^Windows^           ^Files^             ^Development^       ^Git^               ^Other^
^-------^           ^-----^             ^-----------^       ^---^               ^-----^
_w_: window mgmt    _f_: file ops       _d_: dev tools      _g_: git/magit      _t_: themes
_b_: buffers        _p_: project        _l_: LSP/eglot      _m_: merge tools    _h_: help
_q_: quit           _r_: recent files   _e_: errors         _x_: elixir/mix     _s_: search
                                        _c_: claude code    _v_: terminals
"
  ("w" hydra-windows/body)
  ("b" hydra-buffers/body)
  ("f" hydra-files/body)
  ("p" hydra-project/body)
  ("r" consult-recent-file)
  ("d" hydra-dev/body)
  ("l" hydra-lsp/body)
  ("e" hydra-errors/body)
  ("g" hydra-git/body)
  ("m" hydra-merge/body)
  ("x" hydra-elixir/body)
  ("c" hydra-claude/body)
  ("v" hydra-terminal/body)
  ("t" consult-theme)
  ("h" hydra-help/body)
  ("s" hydra-search/body)
  ("q" nil))

;; Window management
(defhydra hydra-windows (:color red :hint nil)
  "
^Navigation^        ^Split^             ^Resize^              ^Other^
^----------^        ^-----^             ^------^              ^-----^
_<left>_: left      _2_: split below    _S-<left>_: shrink h  _d_: delete
_<down>_: down      _3_: split right    _S-<down>_: shrink v  _D_: delete others
_<up>_: up          _u_: undo layout    _S-<up>_: enlarge v   _s_: swap
_<right>_: right    _r_: redo layout    _S-<right>_: enlarge h _=_: balance
_w_: ace-window     _p_: pin/unpin      _f_: move->frame      _F_: copy->frame
                                        _q_: quit
"
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("w" ace-window :color teal)
  ("2" split-window-below :color teal)
  ("3" split-window-right :color teal)
  ("u" winner-undo)
  ("r" winner-redo)
  ("S-<left>" shrink-window-horizontally)
  ("S-<down>" shrink-window)
  ("S-<up>" enlarge-window)
  ("S-<right>" enlarge-window-horizontally)
  ("d" delete-window :color teal)
  ("D" delete-other-windows :color teal)
  ("s" ace-swap-window :color teal)
  ("=" balance-windows :color teal)
  ("p" my/toggle-window-dedicated)
  ("f" my/move-buffer-to-new-frame :color teal)
  ("F" my/open-buffer-in-new-frame :color teal)
  ("q" nil))

;; Buffer management
(defhydra hydra-buffers (:color teal :hint nil)
  "
^Switch^            ^Actions^           ^List^
^------^            ^-------^           ^----^
_b_: switch         _k_: kill           _l_: list all
_n_: next           _K_: kill others    _i_: ibuffer
_p_: previous       _s_: save           _r_: revert
_o_: other window   _S_: save all       ^ ^
"
  ("b" consult-buffer)
  ("n" next-buffer)
  ("p" previous-buffer)
  ("o" switch-to-buffer-other-window)
  ("k" kill-buffer)
  ("K" kill-other-buffers)
  ("s" save-buffer)
  ("S" save-some-buffers)
  ("l" list-buffers)
  ("i" ibuffer)
  ("r" revert-buffer)
  ("q" nil))

;; File operations
(defhydra hydra-files (:color teal :hint nil)
  "
^Find^              ^Actions^           ^Directories^
^----^              ^-------^           ^-----------^
_f_: find file      _s_: save           _d_: dired
_r_: recent         _S_: save as        _D_: dired other
_F_: find at point  _R_: rename         _j_: dired jump
^ ^                 _c_: copy           ^ ^
"
  ("f" find-file)
  ("r" consult-recent-file)
  ("F" find-file-at-point)
  ("s" save-buffer)
  ("S" write-file)
  ("R" rename-file)
  ("c" copy-file)
  ("d" dired)
  ("D" dired-other-window)
  ("j" dired-jump)
  ("q" nil))

;; Project management (Projectile)
(defhydra hydra-project (:color teal :hint nil)
  "
^Switch^            ^Find^              ^Actions^           ^Search^
^------^            ^----^              ^-------^           ^------^
_p_: project        _f_: file           _c_: compile        _s_: search
_b_: buffer         _d_: directory      _t_: test           _r_: replace
_w_: window         _g_: grep           _i_: invalidate     _o_: occur
^ ^                 ^ ^                 _k_: kill buffers   ^ ^
"
  ("p" projectile-switch-project)
  ("b" projectile-switch-to-buffer)
  ("w" projectile-switch-project-action)
  ("f" projectile-find-file)
  ("d" projectile-find-dir)
  ("g" consult-git-grep)
  ("s" consult-ripgrep)
  ("r" projectile-replace)
  ("o" projectile-multi-occur)
  ("c" projectile-compile-project)
  ("t" projectile-test-project)
  ("i" projectile-invalidate-cache)
  ("k" projectile-kill-buffers)
  ("q" nil))

;; Development tools
(defhydra hydra-dev (:color teal :hint nil)
  "
^Navigate^          ^Eval^              ^Debug^             ^Format^
^--------^          ^----^              ^-----^             ^------^
_._: find def       _e_: eval region    _b_: toggle break   _f_: format buffer
_,_: go back        _E_: eval buffer    _d_: debug          _r_: format region
_/_: find refs      _s_: eval sexp      _c_: clear breaks   _n_: indent region
_m_: imenu          ^ ^                 ^ ^                 ^ ^
"
  ("." xref-find-definitions)
  ("," xref-go-back)
  ("/" xref-find-references)
  ("m" consult-imenu)
  ("e" eval-region)
  ("E" eval-buffer)
  ("s" eval-last-sexp)
  ("b" (lambda () (interactive) (message "Breakpoint toggle - mode dependent")))
  ("d" (lambda () (interactive) (message "Debug - mode dependent")))
  ("c" (lambda () (interactive) (message "Clear breakpoints - mode dependent")))
  ("f" (lambda () (interactive) (if (fboundp 'eglot-format-buffer) (eglot-format-buffer) (message "No formatter available"))))
  ("r" (lambda () (interactive) (if (fboundp 'eglot-format) (eglot-format) (message "No formatter available"))))
  ("n" indent-region)
  ("q" nil))

;; LSP/Eglot operations
(defhydra hydra-lsp (:color teal :hint nil)
  "
^Navigate^          ^Actions^           ^Server^            ^Info^
^--------^          ^-------^           ^------^            ^----^
_._: definition     _r_: rename         _s_: start          _h_: doc at point
_i_: implementation _a_: code actions   _S_: shutdown       _H_: signature
_t_: type def       _f_: format         _R_: restart        _e_: eldoc
_d_: declaration    _o_: organize       ^ ^                 ^ ^
"
  ("." xref-find-definitions)
  ("i" eglot-find-implementation)
  ("t" eglot-find-typeDefinition)
  ("d" eglot-find-declaration)
  ("r" eglot-rename)
  ("a" eglot-code-actions)
  ("f" eglot-format-buffer)
  ("o" eglot-code-action-organize-imports)
  ("s" eglot)
  ("S" eglot-shutdown)
  ("R" eglot-reconnect)
  ("h" eldoc-doc-buffer)
  ("H" eglot-signature-eldoc-function)
  ("e" eldoc)
  ("q" nil))

;; Error navigation
(defhydra hydra-errors (:color red :hint nil)
  "
^Navigate^          ^Actions^           ^Display^
^--------^          ^-------^           ^-------^
_n_: next error     _l_: list errors    _b_: show buffer
_p_: prev error     _c_: clear errors   _q_: quit
_N_: next flymake   ^ ^                 ^ ^
_P_: prev flymake   ^ ^                 ^ ^
"
  ("n" next-error)
  ("p" previous-error)
  ("N" flymake-goto-next-error)
  ("P" flymake-goto-prev-error)
  ("l" flymake-show-buffer-diagnostics :color teal)
  ("c" flymake-start :color teal)
  ("b" flymake-show-buffer-diagnostics :color teal)
  ("q" nil))

;; Git operations
(defhydra hydra-git (:color teal :hint nil)
  "
^Magit^             ^Actions^           ^Hunks^             ^Files^
^-----^             ^-------^           ^-----^             ^-----^
_s_: status         _c_: commit         _n_: next hunk      _f_: find file
_l_: log            _a_: amend          _p_: prev hunk      _F_: find rev
_b_: branch         _r_: revert                             _d_: diff file
_d_: diff           _P_: push                               ^ ^
"
  ("s" magit-status)
  ("l" magit-log)
  ("b" magit-branch)
  ("d" magit-diff)
  ("c" magit-commit)
  ("a" magit-commit-amend)
  ("r" magit-revert)
  ("P" magit-push)
  ("n" (lambda () (interactive) (message "Git hunk navigation - install git-gutter or similar")))
  ("p" (lambda () (interactive) (message "Git hunk navigation - install git-gutter or similar")))
  ("f" magit-find-file)
  ("F" magit-find-file-other-window)
  ("q" nil))

;; Merge conflict resolution
(defhydra hydra-merge (:color teal :hint nil)
  "
^Navigate^          ^Resolve^           ^Actions^
^--------^          ^-------^           ^-------^
_n_: next conflict  _a_: keep A         _s_: save
_p_: prev conflict  _b_: keep B         _k_: kill buffer
^ ^                 _c_: keep current   _q_: quit
^ ^                 _A_: keep all A     ^ ^
^ ^                 _B_: keep all B     ^ ^
"
  ("n" (lambda () (interactive) (search-forward "<<<<<<< " nil t)))
  ("p" (lambda () (interactive) (search-backward "<<<<<<< " nil t)))
  ("a" (lambda () (interactive) (message "Merge resolution - use smerge-mode or ediff")))
  ("b" (lambda () (interactive) (message "Merge resolution - use smerge-mode or ediff")))
  ("c" (lambda () (interactive) (message "Merge resolution - use smerge-mode or ediff")))
  ("A" (lambda () (interactive) (message "Merge resolution - use smerge-mode or ediff")))
  ("B" (lambda () (interactive) (message "Merge resolution - use smerge-mode or ediff")))
  ("s" save-buffer)
  ("k" kill-buffer)
  ("q" nil))

;; Help and documentation
(defhydra hydra-help (:color teal :hint nil)
  "
^Describe^          ^Info^              ^Apropos^           ^Other^
^--------^          ^----^              ^-------^           ^-----^
_f_: function       _i_: info           _a_: apropos        _m_: man
_v_: variable       _I_: info emacs     _A_: apropos cmd    _w_: where-is
_k_: key            ^ ^                 _c_: apropos var    _d_: debug
_o_: mode           ^ ^                 ^ ^                 _r_: reload init
"
  ("f" helpful-callable)
  ("v" helpful-variable)
  ("k" helpful-key)
  ("o" describe-mode)
  ("i" info)
  ("I" info-emacs-manual)
  ("a" apropos)
  ("A" apropos-command)
  ("c" apropos-variable)
  ("m" man)
  ("w" where-is)
  ("d" toggle-debug-on-error)
  ("r" (lambda () (interactive) (load-file user-init-file)))
  ("q" nil))

;; Elixir/Mix operations
(defhydra hydra-elixir (:color teal :hint nil)
  "
^Mix^               ^Test^              ^IEx^               ^Format^
^---^               ^----^              ^---^               ^------^
_c_: compile        _t_: test           _i_: IEx            _f_: format buffer
_r_: run            _T_: test file      _I_: IEx project    _m_: mix format
_d_: deps.get       _a_: test at point  _s_: send region    ^ ^
_n_: new task       ^ ^                 ^ ^                 ^ ^
"
  ("c" (compile "mix compile"))
  ("r" (compile "mix run"))
  ("d" (compile "mix deps.get"))
  ("n" (lambda () (interactive) (compile (read-string "Mix command: " "mix "))))
  ("t" (compile "mix test"))
  ("T" (lambda () (interactive) (compile (format "mix test %s" (buffer-file-name)))))
  ("a" (lambda () (interactive) (compile (format "mix test %s:%d" (buffer-file-name) (line-number-at-pos)))))
  ("i" (lambda () (interactive)
         (let ((default-directory (if (fboundp 'projectile-project-root)
                                      (projectile-project-root)
                                    default-directory)))
           (eat "iex"))))
  ("I" (lambda () (interactive)
         (let ((default-directory (if (fboundp 'projectile-project-root)
                                      (projectile-project-root)
                                    default-directory)))
           (eat "iex -S mix"))))
  ("s" (lambda () (interactive) (message "Send to IEx - eval region in IEx buffer")))
  ("f" eglot-format-buffer)
  ("m" (compile "mix format"))
  ("q" nil))

;; Search operations - cheatsheet showing F5 shortcuts
(defhydra hydra-search (:color teal :hint nil)
  "
^Buffer^            ^Project (F5)^      ^Navigate^          ^Replace^
^------^            ^------------^      ^--------^          ^-------^
_s_: search (C-s)   _d_: deadgrep       _i_: imenu          _r_: replace
_o_: occur          _p_: ripgrep (S-)   _g_: goto line      _R_: query-replace
_k_: keep lines     _f_: find file (C-) _e_: errors         _q_: quit
"
  ("s" consult-line)
  ("o" occur)
  ("k" consult-keep-lines)
  ("d" deadgrep)
  ("p" consult-ripgrep)
  ("f" consult-find)
  ("i" consult-imenu)
  ("g" consult-goto-line)
  ("e" consult-compile-error)
  ("r" replace-string)
  ("R" query-replace)
  ("q" nil))

;; Claude Code IDE operations
(defhydra hydra-claude (:color teal :hint nil)
  "
^Invoke^            ^Send^              ^Quick Actions^     ^Session^
^------^            ^----^              ^-------------^     ^-------^
_i_: invoke         _r_: send region    _e_: fix error      _c_: clear
_m_: menu           _b_: send buffer    _a_: ask about      _k_: kill session
_p_: with prompt    _f_: send file      _d_: debug help     _q_: quit
^ ^                 ^ ^                 _x_: explain        ^ ^
"
  ("i" claude-code-ide-invoke)
  ("m" claude-code-ide-menu)
  ("p" (lambda () (interactive)
         (claude-code-ide-invoke (read-string "Ask Claude: "))))
  ("r" claude-code-ide-send-region)
  ("b" claude-code-ide-send-buffer)
  ("f" my/claude-send-buffer-with-prompt)
  ("e" my/claude-send-error)
  ("a" my/claude-ask-about-code)
  ("d" (lambda () (interactive)
         (my/claude-ask-about-code)
         (insert "Help me debug this: ")))
  ("x" (lambda () (interactive)
         (call-interactively 'my/claude-ask-about-code)))
  ("c" claude-code-ide-clear)
  ("k" claude-code-ide-kill)
  ("q" nil))

;; Terminal operations
(defhydra hydra-terminal (:color teal :hint nil)
  "
^VTerm^             ^Eat^               ^Actions^           ^Navigation^
^-----^             ^---^               ^-------^           ^----------^
_t_: vterm          _e_: eat            _k_: kill terminal  _b_: switch term
_T_: vterm other    _E_: eat project    _r_: rename buffer  _n_: next buffer
_p_: vterm project  ^ ^                 _c_: clear buffer   _p_: prev buffer
^ ^                 ^ ^                 _q_: quit           ^ ^
"
  ("t" vterm)
  ("T" vterm-other-window)
  ("p" (lambda () (interactive)
         (let ((default-directory (if (fboundp 'projectile-project-root)
                                      (projectile-project-root)
                                    default-directory)))
           (vterm))))
  ("e" eat)
  ("E" eat-project)
  ("k" kill-buffer-and-window)
  ("r" rename-buffer)
  ("c" (lambda () (interactive)
         (when (or (derived-mode-p 'vterm-mode)
                   (derived-mode-p 'eat-mode))
           (vterm-clear))))
  ("b" (lambda () (interactive)
         (consult-buffer '(:predicate (lambda (buf)
                                        (with-current-buffer buf
                                          (or (derived-mode-p 'vterm-mode)
                                              (derived-mode-p 'eat-mode))))))))
  ("n" next-buffer)
  ("p" previous-buffer)
  ("q" nil))

;; Utility function for kill-other-buffers (referenced in buffers hydra)
(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Global key binding to access main hydra
(global-set-key (kbd "C-c h") 'hydra-main/body)

(provide 'hydras)
;;; hydras.el ends here
