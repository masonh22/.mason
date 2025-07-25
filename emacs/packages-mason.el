;; -*- lexical-binding: t; -*-
(provide 'packages-mason)

(use-package package
  :config
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

(use-package use-package-ensure
  :custom
  (use-package-always-ensure t))

(use-package doom-themes
  :init
  ;; Disable other themes
  (mapc #'disable-theme custom-enabled-themes)
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-one t)
  ;; Improves org-mode's native fontification
  (doom-themes-org-config)
  ;; Flash the mode-line when the Emacs bell rings
  (doom-themes-visual-bell-config))

(use-package solaire-mode
  :defer t
  :hook (after-init . solaire-global-mode))

(use-package hl-todo
  :defer t
  :hook (after-init . global-hl-todo-mode)
  :config
  (add-to-list 'hl-todo-keyword-faces
               '("MASON" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces
               '("JMH" . "#cc9393")))

(when (version<= "29.1" emacs-version)
  (use-package eglot
    :ensure nil
    :defer t
    :config
    (add-to-list 'eglot-server-programs
                 '((c++-mode c-mode) . ("clangd" "--header-insertion=never")))
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                     ( :check (:command "clippy")
                                       :procMacro (:enable t)
                                       :cargo ( :buildScripts (:enable t)
                                                :features "all")))))
    :custom
    (eglot-ignored-server-capabilities
     '(:hoverProvider
       :inlayHintProvider))
    :hook
    ((c-ts-mode . eglot-ensure)
     (c++-ts-mode . eglot-ensure)
     (typescript-ts-mode . eglot-ensure)
     (tsx-ts-mode . eglot-ensure)
     (rust-ts-mode . eglot-ensure)))

  (use-package treesit
    :ensure nil
    :defer 2
    :custom
    (treesit-font-lock-level 3))

  (use-package treesit-auto
    :hook (after-init . global-treesit-auto-mode)
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)))

(use-package flymake
  :ensure nil
  :defer t
  :bind
  ("M-n" . 'flymake-goto-next-error)
  ("M-p" . 'flymake-goto-prev-error))

(use-package org
  :ensure nil
  :custom
  (org-directory "~/Notes/org")
  (org-default-notes-file (concat org-directory "/notes.org")))

(use-package rainbow-delimiters
  :defer t
  :hook
  ((c++-ts-mode
    c-ts-mode
    emacs-lisp-mode ; TODO emacs-lisp-mode or tuareg?
    tsx-ts-mode
    typescript-ts-mode) . rainbow-delimiters-mode))

(use-package undo-tree
  :defer t
  :hook (after-init . global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history 'nil))

(use-package ace-window
  :defer 1
  :bind
  ("M-o" . 'ace-window))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :defer 1
  :hook after-init
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package vertico
  :defer 1
  :hook after-init
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20)        ;; Show more candidates
  ;; (vertico-resize t)        ;; Grow and shrink the Vertico minibuffer
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  )

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)               ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                ;; Enable auto completion
  (corfu-auto-delay 0.2)        ;; Delay for auto completion
  (corfu-auto-prefix 3)         ;; Minimum length of prefix for auto completion
  (corfu-count 4)               ;; Maximum number of candidates to show
  (corfu-quit-no-match t)       ;; Quit if there is no match
  (corfu-preview-current t)     ;; Enable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the first candidate
  (corfu-on-exact-match 'quit)  ;; Configure handling of exact matches
  :hook
  ((c++-ts-mode)
   (rust-ts-mode)
   (tsx-ts-mode)
   (typescript-ts-mode)
   (tuareg-mode))
  :bind
  (:map corfu-map
        ;; Use tab to enter completion
        ("RET" . nil)
        ("TAB" . corfu-insert)
        ([tab] . corfu-insert)))

;; A few more useful configurations for vertico and corfu...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; tab first indents, then tries to complete
  ;; (setq tab-always-indent 'complete)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3))

;; use the `orderless' completion style.
(use-package orderless
  :defer 1
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
  :defer 1
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)
         ("C-S-s" . isearch-forward)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; (setq consult-preview-key nil)
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Disable preview for switching buffers and grep
  (consult-customize
   consult-buffer consult-grep
   consult-git-grep consult-ripgrep
   :preview-key nil)

  ;; TODO want to disable vertico--allow-prompt in buffer selection
  ;; TODO want to select a different starting index in consult-buffer: show
  ;; other open buffers above the first choice

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package embark
  :defer t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h b" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; TODO try crux

;; ripgrep frontend
(use-package rg
  :defer t)

(use-package projectile
  :defer 2
  :hook after-init
  :custom
  (projectile-project-root-functions
   '(projectile-root-local
     projectile-root-marked
     projectile-root-bottom-up))
  (projectile-indexing-method 'alien)
  ;; :bind-keymap ("C-c p" . projectile-command-map)
  )

(use-package haskell-mode
  :defer t)

;; Major mode for OCaml programming
(use-package tuareg
  :defer t
  :mode
  ("\\.ocamlinit\\'" . tuareg-mode)
  :config ; TODO custom doesn't work here for some reason
  (setq tuareg-match-patterns-aligned t)
  (setq tuareg-mode-name "🐫")
  :hook
  (tuareg-mode . set-ocaml-error-regexp)
  (tuareg-mode . eglot-ensure))

;; Major mode for editing Dune project files
(use-package dune
  :defer t)

(use-package rust-mode
  :defer t)

(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face trailing tabs tab-mark))
  :hook (prog-mode))

(use-package expand-region
  :defer 2
  :bind
  ("C-=" . er/expand-region))

(use-package magit
  :defer t)

;; emacs start-up profiler
(use-package esup
  :defer t)

(use-package vterm
  :ensure nil
  :defer t
  :custom
  (vterm-buffer-name-string "*vterm* %s")
  (vterm-kill-buffer-on-exit t))

;; Latex setup.
;; See https://docs.doomemacs.org/latest/modules/lang/latex/
;; and https://michaelneuper.com/posts/efficient-latex-editing-with-emacs/

(use-package auctex
  :defer t
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t))

(use-package latex-preview-pane
  :defer t)
