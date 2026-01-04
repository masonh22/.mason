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

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  (persp-sort 'access)
  (persp-modestring-short t)
  :hook
  (after-init . (lambda ()
                  (persp-mode)
                  ;; For consult: only show buffers in the current perspective.
                  ;; Note that narrowing (`b SPC`) will still show all buffers.
                  (with-eval-after-load 'consult
                    (consult-customize consult--source-buffer :hidden t :default nil)
                    (add-to-list 'consult-buffer-sources persp-consult-source)))))

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
                                     ( :check (:command "check")
                                       :procMacro (:enable t)
                                       :cargo ( :buildScripts (:enable t)
                                                :targetDir t
                                                :features "all")))))
    :custom
    (eglot-ignored-server-capabilities
     '(:hoverProvider
       :inlayHintProvider))
    (eglot-autoshutdown t)
    :hook
    ((c-ts-mode . eglot-ensure)
     (c++-ts-mode . eglot-ensure)
     (typescript-ts-mode . eglot-ensure)
     (tsx-ts-mode . eglot-ensure)
     (rust-ts-mode . eglot-ensure)
     (tuareg-mode . eglot-ensure)))

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
  ;; :hook
  ;; (org-mode . org-indent-mode)
  :custom
  (org-directory "~/Notes/org")
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-hide-leading-stars t))

;; setq-local tab-line-exclude in *scratch* and *org* buffer
(use-package tab-line
  :ensure nil
  :custom
  (tab-line-new-button-show nil)
  :hook
  (after-init . global-tab-line-mode)
  :bind
  ("C-<tab>" . tab-line-switch-to-next-tab)
  ("C-S-<tab>" . tab-line-switch-to-prev-tab)
  ("C-<iso-lefttab>" . tab-line-switch-to-prev-tab)
  ("C-c w" . tab-line-close-current-tab)
  :config
  (defun tab-line-close-current-tab ()
    "Close the currently selected tab in tab-line-mode.  This mimics the
behavior of `tab-line-close-tab' but works on the current buffer without
requiring a mouse event."
    (interactive)
    (let* ((buffer (current-buffer))
           ;; Get the list of tabs currently displayed
           (tabs (funcall tab-line-tabs-function))
           ;; Find the specific tab object corresponding to the current buffer.
           ;; Tabs can be raw buffers or alists (e.g. if using grouping).
           (tab (seq-find (lambda (tab)
                            (eq buffer
                                (if (bufferp tab)
                                    tab
                                  (cdr (assq 'buffer tab)))))
                          tabs))
           (close-function (unless (bufferp tab) (cdr (assq 'close tab)))))
      (cond
       ((not tab)
        (message "Current buffer is not in the tab line"))
       ((functionp close-function)
        (funcall close-function))
       ((eq tab-line-close-tab-function 'kill-buffer)
        (kill-buffer buffer))
       ((eq tab-line-close-tab-function 'bury-buffer)
        (bury-buffer))
       ((functionp tab-line-close-tab-function)
        (funcall tab-line-close-tab-function tab))))
    (force-mode-line-update)))

(use-package org-roam
  :custom
  (org-roam-directory "~/Notes/roam")
  (org-roam-mode-selections
   '((org-roam-backlinks-section :unique t)
    org-roam-reflinks-section)))

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

(use-package avy
  :defer t
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2))

(use-package spatial-navigate
  :defer t
  :bind
  ("M-<up>" . 'spatial-navigate-backward-vertical-bar)
  ("M-<down>" . 'spatial-navigate-forward-vertical-bar))

;; (use-package buffer-name-relative
;;   :init
;;   (buffer-name-relative-mode)
;;   :custom
;;   (buffer-name-relative-prefix '("<" . ">/"))
;;   (buffer-name-relative-fallback nil))

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

(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  (tempel-path (format "%s/templates/*.eld" setup-mason-dir))
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))
  (defun tempel-setup-exclusive-capf ()
    (setq-local completion-at-point-functions
                '(tempel-complete)))
  :hook
  (org-mode . tempel-setup-exclusive-capf)
  (rust-ts-mode . tempel-setup-capf)
  (sh-base-mode . tempel-setup-capf))

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
   (tuareg-mode)
   (sh-base-mode)
   (org-mode))
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
  :bind-keymap
  ("C-x p" . projectile-command-map))

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
  (tuareg-mode . set-ocaml-error-regexp))

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
  :defer t
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package with-editor
  :defer t
  :hook
  ((shell-mode
    eshell-mode
    vterm-mode) . with-editor-export-editor))

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

(use-package nix-ts-mode
  :defer t)

(use-package cursory
  :vc (:url "https://github.com/masonh22/cursory.git"
            :rev :newest)
  :custom
  (cursory-presets
   '((t ;; default
      :cursor-type box
      :cursor-color nil
      :blink-cursor-mode 1
      :blink-cursor-blinks 10
      :blink-cursor-interval 0.5
      :blink-cursor-delay 0.5
      :cursor-in-non-selected-windows t)
     (bar
      :cursor-type (bar . 2))
     (incarnate
      :inherit bar)
     (god
      :cursor-color "magenta"
      :blink-cursor-mode -1)))
  :hook
  (after-init . (lambda () (cursory-set-preset 't))))

(use-package god-mode
  :custom
  (god-mode-enable-function-key-translation nil)
  :bind
  ("<escape>" . god-mode-all)
  (:map god-local-mode-map
        ("i" . incarnate)
        ("[" . backward-paragraph)
        ("]" . forward-paragraph)
        ("r" . replace-string)
        ("O" . spacer))
  ("C-x C-1" . delete-other-windows)
  ("C-x C-2" . split-window-below)
  ("C-x C-3" . split-window-right)
  ("C-x C-0" . delete-window)
  ("C-x C-o" . other-window)
  ("C-x C-b" . consult-buffer)
  :config
  ;; see https://idiomdrottning.org/on-top-of-emacs-god-mode
  (defun spacer (times)
    (interactive "p")
    (dotimes (x times) (insert " ")))
  (defun incarnate ()
    (interactive)
    (cl-assert (bound-and-true-p god-local-mode) "incarnate mode outside of god mode!")
    (cl-assert (not (bound-and-true-p incarnate-mode)) "recursive incarnate mode!")
    (cl-assert (not (bound-and-true-p cursor-color-is-dirty)) "incarnate cursor is dirty!")
    (when (bound-and-true-p god-local-mode)
      (god-local-mode 0)
      (incarnate-mode)
      ;; HACK: cursor color and blink can't be set locally
      (setq cursor-color-is-dirty t)
      (cursory-set-local-preset 'incarnate)))
  (defun unincarnate (&optional from-hook)
    (interactive)
    (cond ((bound-and-true-p incarnate-mode)
           (setq cursor-color-is-dirty nil)
           (incarnate-mode -1)
           (god-local-mode 1))
          ((and from-hook (bound-and-true-p cursor-color-is-dirty))
           ;; HACK: revert global properties modified by incarnate
           (setq cursor-color-is-dirty nil)
           (let* ((styles (cursory--get-preset-properties 'god))
                  (color-value (plist-get styles :cursor-color))
                  (blink (plist-get styles :blink-cursor-mode)))
             (cursory--set-cursor color-value)
             (blink-cursor-mode blink)))))
  (define-minor-mode incarnate-mode
    "As normal but toggle to God mode on RET"
    :lighter " God-Inc"
    :keymap '(("\r" . unincarnate)
              ([escape] . unincarnate)))
  ;; Exit incarnate mode when switching buffers
  (add-hook 'window-selection-change-functions 'unincarnate)
  :hook
  (god-mode-enabled . (lambda ()
                        (cursory-set-preset 'god)))
  (god-mode-disabled . (lambda ()
                         (unless (bound-and-true-p cursor-color-is-dirty)
                           (cursory-set-preset 't)))))
