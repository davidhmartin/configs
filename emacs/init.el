;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Emacs configuration focused on LSP-based development workflows
;; with support for Elixir, Rust, and Go development.

;;; Code:

;; Frame size constants
(defconst my/default-frame-width 120
  "Default width for new frames in characters.")
(defconst my/default-frame-height 60
  "Default height for new frames in lines.")


;; Performance optimizations
(setq gc-cons-threshold (* 50 1000 1000))

;; Bootstrap straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure use-package to use straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Fix PATH in GUI Emacs
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))




;;; Basic Emacs Configuration

;; Window dedication toggle
(defun my/toggle-window-dedicated ()
  "Toggle whether current window is dedicated to its buffer."
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "not " "")
             (buffer-name (window-buffer window)))))

(global-set-key (kbd "C-c w d") 'my/toggle-window-dedicated)

;; UI preferences
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

;; Default frame size
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize nil)
(setq default-frame-alist
      (append `((width . ,my/default-frame-width)
                (height . ,my/default-frame-height))
              default-frame-alist))
(setq initial-frame-alist
      (append `((width . ,my/default-frame-width)
                (height . ,my/default-frame-height))
              initial-frame-alist))

;; Force new frames to resize after creation
(defun my/resize-frame-on-create (frame)
  "Force FRAME to the desired size after creation."
  (run-with-timer
   0.1 nil
   (lambda ()
     (with-selected-frame frame
       (set-frame-parameter frame 'fullscreen nil)
       (set-frame-width frame my/default-frame-width)
       (set-frame-height frame my/default-frame-height)))))

(add-hook 'after-make-frame-functions 'my/resize-frame-on-create)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Font configuration
(set-face-attribute 'default nil :font "Fira Code" :height 120)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Open current buffer in new frame
(defun my/open-buffer-in-new-frame ()
  "Open the current buffer in a new frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (select-frame (make-frame `((width . ,my/default-frame-width)
                                (height . ,my/default-frame-height))))
    (switch-to-buffer buffer)))

(defun my/move-buffer-to-new-frame ()
  "Move the current buffer to a new frame, closing it in the original frame."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window)))
    (select-frame (make-frame `((width . ,my/default-frame-width)
                                (height . ,my/default-frame-height))))
    (switch-to-buffer buffer)
    (with-selected-frame (window-frame window)
      (delete-window window))))

;; Configure specific buffers to appear in side windows
(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (window-parameters . ((no-delete-other-windows . t)))))

(add-to-list 'display-buffer-alist
             '("\\*eat\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 15)
               (window-parameters . ((no-delete-other-windows . t)))))


;;; Package Configuration

;; Theme persistence
(defvar my/theme-file (expand-file-name "current-theme" user-emacs-directory)
  "File to store the current theme.")

(defun my/save-current-theme ()
  "Save the current theme to file."
  (when custom-enabled-themes
    (with-temp-file my/theme-file
      (prin1 (car custom-enabled-themes) (current-buffer)))))

(defun my/load-saved-theme ()
  "Load the saved theme from file."
  (when (file-exists-p my/theme-file)
    (let ((saved-theme (with-temp-buffer
                         (insert-file-contents my/theme-file)
                         (read (current-buffer)))))
      (when (and saved-theme (symbolp saved-theme))
        (load-theme saved-theme t)))))

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Load saved theme or default to doom-one
(my/load-saved-theme)
(unless custom-enabled-themes
  (load-theme 'doom-one t))

;; Auto-save theme after using consult-theme
(advice-add 'consult-theme :after
            (lambda (&rest _)
              (my/save-current-theme)))

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Which Key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Hydra for organized key bindings
(use-package hydra
  :config
  (load (expand-file-name "hydras.el" user-emacs-directory)))

;; Enable basic completion system
(setq completion-cycle-threshold 3)
(setq completions-detailed t)
(setq tab-always-indent 'complete)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))))

;; Orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Modern completion with Vertico + Consult + Marginalia
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-M-j" . consult-buffer)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Configure consult-line to work properly
  (setq consult-line-start-from-top t))

;; Embark for contextual actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; Helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'default))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Magit
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Corfu completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Cape for completion-at-point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Eglot LSP client
(use-package eglot
  :hook ((elixir-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rustic-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-sync-connect nil)     ; Don't block Emacs while connecting
  (eglot-report-progress t)    ; Show progress in modeline
  :config
  ;; Configure Elixir language servers for both elixir-mode and elixir-ts-mode
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((elixir-mode elixir-ts-mode) . ,(eglot-alternatives
                                   '("~/.local/bin/elixir-ls/language_server.sh"
				     "expert")))))

  (setq-default eglot-workspace-configuration
                '((:rust-analyzer . (:cargo (:buildScripts (:enable t))
                                     :procMacro (:enable t)
                                     :diagnostics (:disabled ["unresolved-proc-macro"
                                                              "unresolved-macro-call"]))))))

;; Tree-sitter configuration
(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt))

;; Language Modes

;; Elixir with tree-sitter support
(use-package elixir-mode)

(use-package elixir-ts-mode
  :mode "\\.exs?\\'"
  :hook (elixir-ts-mode . eglot-ensure))

;; Rust
(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . consult-imenu)
              ("M-?" . xref-find-references)
              ("C-c C-c l" . flymake-show-buffer-diagnostics)
              ("C-c C-c a" . eglot-code-actions)
              ("C-c C-c r" . eglot-rename)
              ("C-c C-c q" . eglot-shutdown)
              ("C-c C-c Q" . eglot-shutdown-all))
  :config
  (setq rustic-format-on-save t))


;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; Terminal emulators

;; VTerm - Fast terminal emulator
(use-package vterm
  :commands vterm
  :bind (("C-c t" . vterm)
         ("C-c T" . vterm-other-window))
  :custom
  (vterm-max-scrollback 10000)
  (vterm-shell (getenv "SHELL"))
  (vterm-kill-buffer-on-exit t)
  :config
  ;; Don't query about killing vterm buffers
  (defun my/vterm-kill-buffer-advice (&rest _)
    "Kill vterm buffer without confirmation."
    (let ((kill-buffer-query-functions nil))
      (kill-buffer)))
  (advice-add 'vterm--sentinel :after #'my/vterm-kill-buffer-advice))

;; Eat - Alternative terminal emulator
(use-package eat
  :bind (("C-c v" . eat)
         ("C-c C-v" . eat-project))
  :custom
  (eat-term-name "xterm-256color")
  (eat-term-shell-integration-directory
   (expand-file-name "straight/repos/eat/integration" user-emacs-directory)))

;; Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))


;; Claude Code IDE
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind (("C-c C-'" . claude-code-ide-menu)
         ("C-c c i" . claude-code-ide-invoke)
         ("C-c c c" . claude-code-ide-clear)
         ("C-c c k" . claude-code-ide-kill)
         ("C-c c s" . claude-code-ide-send-region)
         ("C-c c b" . claude-code-ide-send-buffer)
         ("C-c c e" . my/claude-send-error)
         ("C-c c a" . my/claude-ask-about-code))
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'vterm)

  ;; Helper function to send errors to Claude
  (defun my/claude-send-error ()
    "Send the current error or diagnostic to Claude Code for help."
    (interactive)
    (let ((error-msg (or (and (bound-and-true-p flymake-mode)
                              (flymake--diag-text (car (flymake-diagnostics (point)))))
                         (thing-at-point 'line t))))
      (when error-msg
        (claude-code-ide-invoke
         (format "Help me fix this error:\n\n%s\n\nContext: %s:%d"
                 error-msg
                 (buffer-file-name)
                 (line-number-at-pos))))))

  ;; Helper function to ask Claude about code at point
  (defun my/claude-ask-about-code ()
    "Ask Claude Code about the code at point or in region."
    (interactive)
    (let* ((code (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'defun t)))
           (question (read-string "Ask Claude: " "Explain this code: ")))
      (when code
        (claude-code-ide-invoke
         (format "%s\n\n```%s\n%s\n```\n\nFile: %s:%d"
                 question
                 (symbol-name major-mode)
                 code
                 (buffer-file-name)
                 (line-number-at-pos))))))

  ;; Helper function to send buffer with custom message
  (defun my/claude-send-buffer-with-prompt ()
    "Send current buffer to Claude with a custom prompt."
    (interactive)
    (let ((prompt (read-string "What should Claude do with this buffer? ")))
      (claude-code-ide-invoke
       (format "%s\n\n```%s\n%s\n```\n\nFile: %s"
               prompt
               (symbol-name major-mode)
               (buffer-substring-no-properties (point-min) (point-max))
               (buffer-file-name))))))

;; Display buffer configuration for Claude Code
(add-to-list 'display-buffer-alist
             '("\\*claude-code.*\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right)
               (slot . 1)
               (window-width . 100)
               (window-parameters . ((no-delete-other-windows . t)))))

(add-to-list 'display-buffer-alist
             '("\\*Claude Code Terminal\\*"
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (slot . 1)
               (window-height . 20)
               (window-parameters . ((no-delete-other-windows . t)))))


;; Restore gc-cons-threshold
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "fd22a3aac273624858a4184079b7134fb4e97104d1627cb2b488821be765ff17"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
