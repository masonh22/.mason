;; -*- lexical-binding: t; -*-
(provide 'setup-mason)

;; Disable gc, etc. to speed up startup, but re-enable them later.
;; From https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/early-init.el
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 100)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2)))

;; This includes all necessary config modules
(require 'modeline-mason)
(require 'config-mason)
(require 'packages-mason)
