;;; macropad-1.el -- Key bindings for programmable macropad

;;; Commentary:
;; This file maps key codes from QMK-programmable keyboard or macropad.
;; It assumes a 4x4 grid of keys, each mapped to a hyper-key-modded letter,
;; as follows:
;;
;;  +-----+-----+-----+-----+
;;  | H-a | H-b | H-c | H-d |
;;  +-----+-----+-----+-----+
;;  | H-e | H-f | H-g | H-h |
;;  +-----+-----+-----+-----+
;;  | H-i | H-j | H-k | H-l |
;;  +-----+-----+-----+-----+
;;  | H-m | H-n | H-o | H-p |
;;  +-----+-----+-----+-----+
;;
;; Note that the qmk devices emit ctrl-meta-super-shift for the HYPR modifier,
;; so we use "C-M-S-s" instead of "H".

(global-set-key (kbd "C-M-S-s-a") #'previous-buffer)
(global-set-key (kbd "C-M-S-s-b") #'ibuffer)
(global-set-key (kbd "C-M-S-s-c") #'dired)
(global-set-key (kbd "C-M-S-s-d") #'next-buffer)

(global-set-key (kbd "C-M-S-s-e") #'er/expand-region)
(global-set-key (kbd "C-M-S-s-f") #'beginning-of-defun)
(global-set-key (kbd "C-M-S-s-g") #'mark-defun)
(global-set-key (kbd "C-M-S-s-h") #'consult-imenu-multi)

(global-set-key (kbd "C-M-S-s-i") #'backward-sexp)
(global-set-key (kbd "C-M-S-s-j") #'end-of-defun)
(global-set-key (kbd "C-M-S-s-k") #'forward-sexp)
(global-set-key (kbd "C-M-S-s-l") #'consult-imenu)

(global-set-key (kbd "C-M-S-s-m") #'deadgrep)
(global-set-key (kbd "C-M-S-s-n") #'consult-ripgrep)
(global-set-key (kbd "C-M-S-s-o") #'consult-find)
(global-set-key (kbd "C-M-S-s-p") #'consult-imenu)


;; window navigation layer

(global-set-key (kbd "C-M-s-a") nil)
(global-set-key (kbd "C-M-s-b") nil)
(global-set-key (kbd "C-M-s-c") nil)
(global-set-key (kbd "C-M-s-d") nil)

(global-set-key (kbd "C-M-s-e") nil)
(global-set-key (kbd "C-M-s-f") #'windmove-up)
(global-set-key (kbd "C-M-s-g") nil)
(global-set-key (kbd "C-M-s-h") #'winner-redo)

(global-set-key (kbd "C-M-s-i") #'windmove-left)
(global-set-key (kbd "C-M-s-j") #'windmove-down)
(global-set-key (kbd "C-M-s-k") #'windmove-right)
(global-set-key (kbd "C-M-s-l") #'winner-undo)

(global-set-key (kbd "C-M-s-m") nil)
(global-set-key (kbd "C-M-s-n") nil)
(global-set-key (kbd "C-M-s-o") nil)
(global-set-key (kbd "C-M-s-p") nil)


;; window mutation layer
(global-set-key (kbd "C-M-S-a") nil)
(global-set-key (kbd "C-M-S-b") nil)
(global-set-key (kbd "C-M-S-c") nil)
(global-set-key (kbd "C-M-S-d") nil)

(global-set-key (kbd "C-M-S-e") #'split-window-below)
(global-set-key (kbd "C-M-S-f") #'enlarge-window)
(global-set-key (kbd "C-M-S-g") #'split-window-right)
(global-set-key (kbd "C-M-S-h") #'winner-redo)

(global-set-key (kbd "C-M-S-i") #'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-j") #'shrink-window)
(global-set-key (kbd "C-M-S-k") #'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-l") #'winner-undo)

(global-set-key (kbd "C-M-S-m") #'maximize-window)
(global-set-key (kbd "C-M-S-n") #'balance-windows)
(global-set-key (kbd "C-M-S-o") #'delete-window)
(global-set-key (kbd "C-M-S-p") #'delete-other-windows)
