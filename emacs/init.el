;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Modern Emacs configuration focused on LSP-based development workflows
;; with support for Elixir, Rust, and Go development.

;;; Code:

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

;;; Basic Emacs Configuration

;; UI preferences
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)

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

;; Theme switching with preview using consult
(defun my/consult-theme ()
  "Select and preview themes using consult."
  (interactive)
  (let* ((themes (mapcar #'symbol-name (custom-available-themes)))
         (current (car custom-enabled-themes))
         (current-name (when current (symbol-name current))))
    (consult--read
     themes
     :prompt "Theme: "
     :state (lambda (action theme)
              (when theme
                (pcase action
                  ('preview
                   (mapc #'disable-theme custom-enabled-themes)
                   (load-theme (intern theme) t))
                  ('return
                   (mapc #'disable-theme custom-enabled-themes)
                   (load-theme (intern theme) t)
                   (my/save-current-theme)
                   (message "Loaded and saved theme: %s" theme)))))
     :default current-name
     :require-match t
     :category 'theme
     :lookup #'consult--lookup-candidate)))

(global-set-key (kbd "C-c t") 'my/consult-theme)

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
  :config
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer . (:cargo (:buildScripts (:enable t))
                                     :procMacro (:enable t)
                                     :diagnostics (:disabled ["unresolved-proc-macro"
                                                              "unresolved-macro-call"]))))))

;; Language Modes

;; Elixir
(use-package elixir-mode)

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

;; Flymake (built-in syntax checking)
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; Terminal emulator
(use-package eat
  :bind (("C-c v" . eat)
         ("C-c C-v" . eat-project))
  :custom
  (eat-term-name "xterm-256color"))

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


;; Claude Code
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :bind
  ("<f12>" . claude-code-send-escape)
  :config
  (claude-code-mode))


;; Restore gc-cons-threshold
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
