;; -*- lexical-binding: t; -*-

;; Disable package.el for straight.el
;; (setq package-enable-at-startup nil)

(defun mason-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs"
  (progn
    (setq mode-line-format nil)
    (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
    (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
    ))

;; (mason-avoid-initial-flash-of-light)
