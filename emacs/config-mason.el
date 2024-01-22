(provide 'config-mason)

;; disable splash screen
(setq inhibit-splash-screen t)

;; disable tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; disable backup files
(setq make-backup-files nil)

;; closing a client instance that started on a file won't kill that buffer
;; TODO make sure C-x # still kills that buffer!
(setq server-kill-new-buffers nil)

;; use a visible bell instead of an audible one
(setq visible-bell 1)

;; Line numbers
;; (global-display-line-numbers-mode)

;; always auto revert
(global-auto-revert-mode 1)

;; no tabs!
(setq-default indent-tabs-mode nil)

;; fill column text wrapping
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; modeline
;; (setq display-time-default-load-average nil)
;; (display-time)
(line-number-mode)
(column-number-mode)

;; Restore cursor to last place in a file when reopening
(save-place-mode 1)

;; Scrolling
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-preserve-screen-position t
      scroll-margin 2
      scroll-conservatively 1)
;; smooth scrolling
(pixel-scroll-precision-mode)

;; keybindings
(global-set-key (kbd "C-c r") 'replace-string)

;; org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; 75 mb
(setq large-file-warning-threshold 75000000)

;; windmove to S-<arrow keys>
(windmove-default-keybindings)

;; OSC directory tracking
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(add-hook 'comint-output-filter-functions #'comint-osc-process-output)
(require 'shell) ;; TODO when do these get required?
(require 'dirtrack)
(shell-dirtrack-mode nil)
(dirtrack-mode nil)

;; smart beginning of line
(defun smart-beginning-of-line ()
  "Toggle point between first non-whitespace character and beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key "\C-a" 'smart-beginning-of-line)

;;  - better error and backtrace matching for ocaml
(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
           2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

;; OCaml configuration
(face-spec-set
 'tuareg-font-lock-constructor-face
 '((((class color) (background light)) (:foreground "SaddleBrown"))
   (((class color) (background dark)) (:foreground "burlywood1"))))

(put 'upcase-region 'disabled nil)
