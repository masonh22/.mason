;; -*- lexical-binding: t; -*-
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
  (global-hl-todo-mode +1)
  (add-to-list 'hl-todo-keyword-faces
               '("MASON" . "#cc9393"))
  (add-to-list 'hl-todo-keyword-faces
               '("JMH" . "#cc9393")))

(unless (version< emacs-version "29.1")
  (use-package eglot
    :straight nil
    :defer t
    :config
    (add-to-list 'eglot-server-programs
                 '((c++-mode c-mode) . ("clangd" "--header-insertion=never")))
    (add-to-list 'eglot-server-programs
                 '((rust-ts-mode) . ("rust-analyzer" :initializationOptions
                                     (:check (:command "clippy")))))
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
    :straight nil
    :custom
    (treesit-font-lock-level 3))

  (use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    ;; https://github.com/tree-sitter/tree-sitter-cpp/issues/271
    (add-to-list 'treesit-auto-recipe-list
                 (make-treesit-auto-recipe
                  :lang 'cpp
                  :ts-mode 'c++-ts-mode
                  :remap 'c++-mode
                  :url "https://github.com/tree-sitter/tree-sitter-cpp"
                  :revision "v0.22.0"
                  :ext "\\.cpp\\'"))
    (global-treesit-auto-mode)))

(use-package rainbow-delimiters
  :hook (c++-ts-mode
         c-ts-mode
         emacs-lisp-mode ; TODO emacs-lisp-mode or tuareg?
         tsx-ts-mode
         typescript-ts-mode))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history 'nil))

(use-package ace-window
  :bind
  ("M-o" . 'ace-window))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
;;   ;; (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;;   ;; :init
;;   ;; (global-corfu-mode)
;;   :hook
;;   ((c++-ts-mode)
;;    (tsx-ts-mode)
;;    (typescript-ts-mode)
;;    (tuareg-mode))
;;   :bind
;;   (:map corfu-map
;;         ;; Unbind RET
;;         ("RET" . nil)
;;         ;; TAB-and-Go
;;         ("TAB" . corfu-next)
;;         ([tab] . corfu-next)
;;         ("S-TAB" . corfu-previous)
;;         ([backtab] . corfu-previous)))

;; (use-package corfu-candidate-overlay
;;   :after corfu
;;   :hook
;;   (corfu-mode)
;;   :bind
;;   ("C-<tab>" . completion-at-point))

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
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Example configuration for Consult
(use-package consult
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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package company
  :custom
  (company-idle-delay
   (lambda () (if (company-in-string-or-comment) nil 0.2)))
  (company-selection-wrap-around t)
  (company-tooltip-limit 4)
  (company-tooltip-flip-when-above t)
  ;; (company-tooltip-minimum-width 30)
  ;; (company-tooltip-maximum-width 30)
  (company-tooltip-width-grow-only)
  (company-frontends
   '(company-pseudo-tooltip-unless-just-one-frontend
     company-preview-if-just-one-frontend
     company-echo-metadata-frontend))
  (company-format-margin-function #'company-text-icons-margin)
  (company-text-icons-format "%s ")
  ;; TODO consider using company-echo-strip-common-frontent
  :hook
  ((c++-ts-mode)
   (tsx-ts-mode)
   (typescript-ts-mode)
   (tuareg-mode)))

;; TODO try crux

;; ripgrep frontend
(use-package rg)

(use-package projectile
  :init
  (projectile-mode +1)
  :custom
  (projectile-project-root-functions
   '(projectile-root-local
     projectile-root-marked
     projectile-root-bottom-up))
  (projectile-indexing-method 'alien)
  ;; :bind
  ;; (:map projectile-mode-map
  ;;       ("C-c p" . projectile-command-map))
  )

(use-package haskell-mode
  :defer t)

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

;; highlight chars
(use-package highlight-chars
  :hook
  (c++-ts-mode . hc-highlight-tabs)
  (c++-ts-mode . hc-highlight-trailing-whitespace)
  (c-ts-mode . hc-highlight-tabs)
  (c-ts-mode . hc-highlight-trailing-whitespace)
  (tuareg-mode . hc-highlight-tabs)
  (tuareg-mode . hc-highlight-trailing-whitespace)
  (rust-ts-mode . hc-highlight-tabs)
  (rust-ts-mode . hc-highlight-trailing-whitespace)
  (shell-script-mode . hc-highlight-tabs)
  (shell-script-mode . hc-highlight-trailing-whitespace))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package magit)
