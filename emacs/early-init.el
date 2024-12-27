;; -*- lexical-binding: t; -*-

;; Disable package.el for straight.el
(unless (version< emacs-version "25.1")
  (setq package-enable-at-startup nil))
