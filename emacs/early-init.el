;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before init.el and before the package system
;; and GUI are initialized. Use it for performance optimizations.

;;; Code:

;; Increase gc-cons-threshold during startup for faster loading
;; This will be restored in init.el after startup completes
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Allow package.el to initialize packages at startup
(setq package-enable-at-startup t)

;; Disable UI elements early (before frame creation) for faster startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Disable mode-line during init (will be restored by doom-modeline)
(setq-default mode-line-format nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time
(setq native-comp-deferred-compilation nil) ; Emacs 28
(setq native-comp-jit-compilation nil)      ; Emacs 29+

;; Silence native compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Prevent loading of site-start.el
(setq site-run-file nil)

;; Disable bidirectional text scanning for performance
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors
;; or regions in non-focused windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts. Keep it disabled.
(setq inhibit-compacting-font-caches t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config.
(advice-add #'x-apply-session-resources :override #'ignore)

;; Set frame parameters early
(setq default-frame-alist
      (append '((width . 120)
                (height . 60)
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil))
              default-frame-alist))

(provide 'early-init)
;;; early-init.el ends here
