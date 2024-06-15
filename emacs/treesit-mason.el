;; -*- lexical-binding: t; -*-
(provide 'treesit-mason)

(when (version<= emacs-version "29.1")
  (error "Native tree-sitter integration requires at least v29.1"))

;; TODO move this to my-packages.el once use-package/straight works with this
;; TODO customize with treesit-font-lock-rules
(require 'treesit)
(setq treesit-font-lock-level 3)
(setq treesit-language-source-alist
      '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        ;; (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
        ))

;; only need to run this once?
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; (use-package treesit
;;   :ensure nil ;; emacs built-in
;;   :config
;;   (setq treesit-font-lock-level 4)
;;   (setq treesit-language-source-alist
;;         '((cpp "https://github.com/tree-sitter/tree-sitter-cpp"))))

(add-to-list 'auto-mode-alist
             '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist
             '("\\.tsx\\'" . tsx-ts-mode))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (bash-mode . bash-ts-mode)
        (rust-mode . rust-ts-mode)))
