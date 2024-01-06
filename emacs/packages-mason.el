(provide 'packages-mason)

;; Use straight.el
(unless (version<= emacs-version "25.1")
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
  (when (version< emacs-version "29.1")
    ;; use-package is included starting with v29.1
    (straight-use-package 'use-package))
  (setq straight-use-package-by-default t)
  (setq straight-vc-git-default-clone-depth 1))

;; really an if-then-else, but idk how to put multiple terms in the "then" block
(when (version< emacs-version "25.1")
  (require 'package)
  (add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-and-compile
    (setq use-package-always-ensure t
          use-package-expand-minimally t)))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package hl-todo
  :config
  (global-hl-todo-mode +1))

;; eglot TODO use-package, but straight.el clones this...
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
(setq eglot-ignored-server-capabilities
      '(:hoverProvider
        :inlayHintProvider))
(add-hook 'c-ts-mode 'eglot-ensure)
(add-hook 'c++-ts-mode 'eglot-ensure)
(add-hook 'typescript-ts-mode 'eglot-ensure)
(add-hook 'tsx-ts-mode 'eglot-ensure)
;(add-hook 'tuareg-mode 'eglot-ensure)

(use-package rainbow-delimiters
  :hook c++-ts-mode
  :hook c-ts-mode
  :hook emacs-lisp-mode
  :hook tsx-ts-mode
  :hook typescript-ts-mode)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history 'nil))

;; TODO remove in v29, replaced by elgot; TODO get lean to work with eglot
;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (lean4-mode . lsp)
;;   :commands (lsp))

;; this clones eglot...
;; (use-package eglot
;;   :hook (typescript-ts-mode . eglot-ensure))
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

(use-package ace-window
  :bind
  ("M-o" . 'ace-window))

;; ivy config
(use-package counsel
  :init
  (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :bind
  (("\C-s" . 'swiper)
   ("C-M-s" . 'isearch-forward)
   ("C-c s" . 'swiper-thing-at-point)))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

;; ripgrep frontend (for projectile)
(use-package rg)

(use-package projectile
  :init
  (projectile-mode +1)
  :custom
  (projectile-project-root-functions
   '(projectile-root-local
     projectile-root-marked
     projectile-root-bottom-up))
  (projectile-mode-line-function
   '(lambda ()
      (if (projectile-project-p)
          (format " Proj[%s:%s]"
                  (projectile-project-name)
                  (projectile-project-type))
        "")))
  (projectile-indexing-method 'alien)
  ;; (projectile-globally-ignored-directories
  ;;  (append '("*node_modules"
  ;;            "*build"
  ;;            "*\\.git")
  ;;          projectile-globally-ignored-directories))
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

(use-package haskell-mode)

(use-package rust-mode
  :custom
  (rust-indent-offset 2))

(use-package proof-general
  :custom
  (proof-splash-enable 'nil))

(use-package company
  :hook typescript-ts-mode
  :hook tsx-ts-mode
  :hook c++-ts-mode
  :hook c-ts-mode)

(use-package company-coq
  :hook coq-mode)

;; Major mode for OCaml programming
(use-package tuareg
  :mode
  ("\\.ocamlinit\\'" . tuareg-mode)
  :config ; TODO custom doesn't work here for some reason
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-mode-name "🐫")
  :hook
  (tuareg-mode . set-ocaml-error-regexp)
  (tuareg-mode . eglot-ensure))

;; Major mode for editing Dune project files
(use-package dune)

;; this requires dash f flycheck lsp-mode magit-section and s...
;; dash: a modern list library
;; f: a modern API for working with files and directories
;; flycheck: syntax checking
;; lsp-mode: language server protocol
;; magit-section: UI stuff
;; s: the long lost string manipulation library
(unless (version<= emacs-version "25.1")
  (use-package lean4-mode
    :straight (lean4-mode
               :type git
               :host github
               :repo "leanprover/lean4-mode"
               :files ("*.el" "data"))
    ;; defer loading the package until required (?)
    :commands (lean4-mode)
    :config
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.lake\\'")))

;; highlight chars
(use-package highlight-chars
  :hook
  (c++-ts-mode . hc-highlight-tabs)
  (c++-ts-mode . hc-highlight-trailing-whitespace)
  (c-ts-mode . hc-highlight-tabs)
  (c-ts-mode . hc-highlight-trailing-whitespace)
  (tuareg-mode . hc-highlight-tabs)
  (tuareg-mode . hc-highlight-trailing-whitespace)
  (rust-mode . hc-highlight-tabs)
  (rust-mode . hc-highlight-trailing-whitespace))
