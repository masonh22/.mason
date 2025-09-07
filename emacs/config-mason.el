;; -*- lexical-binding: t; -*-
(provide 'config-mason)

;; TODO a few things are slow here. I added "TODO slow" comments to them. They
;; can be addressed by #5 here: https://blog.d46.us/advanced-emacs-startup/

;; disable splash screen
(setq inhibit-splash-screen t)

;; disable GUI graphical boxes
(setopt use-dialog-box nil)

;; disable tool, scroll, and menu bars TODO slow, .02s
(when (fboundp 'tool-bar-mode) ;; not available on nox
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) ;; not available on nox
  (scroll-bar-mode -1))
(menu-bar-mode -1)

;; set the font explicitly
(set-face-attribute 'default nil
                    :family "monospace"
                    :height 130)

;; use utf-8 in terminals
(set-terminal-coding-system 'utf-8)

;; disable backup files
(setq make-backup-files nil)

;; closing a client instance that started on a file won't kill that buffer
;; TODO make sure C-x # still kills that buffer!
;; (setq server-kill-new-buffers nil)

;; use a visible bell instead of an audible one
(setq visible-bell 1)

;; Line numbers
;; (global-display-line-numbers-mode)

;; always auto revert TODO slow!
(global-auto-revert-mode 1)

;; no tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; fill column text wrapping
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; modeline
;; (setq display-time-default-load-average nil)
;; (display-time)
(line-number-mode t)
(column-number-mode t)

;; Frame title: projectile project or file name
(setq frame-title-format
      '(:eval
        (let ((project-name (projectile-project-name))
              (project-type (projectile-project-type)))
          (if (or project-type
                  (and project-name
                       (not (string= project-name "-"))))
              (list
               "  "
               (format "%s%s"
                       (or project-name "-")
                       (if project-type
                           (format " (%s)" project-type)
                         "")))
            (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
              "%b")))))

;; Restore cursor to last place in a file when reopening
(save-place-mode 1)

;; Scrolling
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-preserve-screen-position t
      scroll-margin 2
      scroll-conservatively 100000)

;; Smooth-scrolling
;; (when (version<= "29" emacs-version)
;;   (pixel-scroll-precision-mode t))

;; keybindings
(global-set-key (kbd "C-c r") 'replace-string)

;; confirm quit
(setq confirm-kill-emacs 'y-or-n-p)

;; 75MB
(setq large-file-warning-threshold 75000000)

;; windmove to S-<arrow keys> TODO slow!
(windmove-default-keybindings)

;; OSC directory tracking
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(add-hook 'comint-output-filter-functions #'comint-osc-process-output)
;; TODO disable these
;; (shell-dirtrack-mode nil)
;; (dirtrack-mode nil)

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

;; Always load newest byte code
(setq load-prefer-newer t)

;; isearch
(setq search-whitespace-regexp ".*?")
(setq isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil)

;; *org* buffer
(defcustom initial-org-message (purecopy "\
#+title: Scratch buffer

")
  "The message to display in the initial '*org*' buffer"
  :type 'string
  :group 'startup)

(defun create-org-buffer (&optional name)
  "Create an return the '*org*' buffer."
  (let ((org-buffer (get-buffer-create
                     (concat "*org*" (when name (format " (%s)" name))))))
    (with-current-buffer org-buffer
      (org-mode)
      (insert initial-org-message)
      (goto-char (point-max))
      (set-buffer-modified-p nil))
    org-buffer))

(defun init-org-buffer ()
  "Create the '*org' buffer and switch to it if the current buffer is
'*scratch'.

This is intended to be called from 'after-init-hook'."
  (let ((org-buffer (create-org-buffer)))
    (when (string= (buffer-name) "*scratch*")
      (switch-to-buffer org-buffer))))

(add-hook 'after-init-hook 'init-org-buffer)

;; Also create the org buffer when using perspective
(with-eval-after-load 'perspective
  (defun persp-init-org-buffer ()
    "Create and switch to the '*org*' buffer, unless this is the initial
perspective."
    (let ((name (persp-name (persp-curr))))
      (unless (string= name persp-initial-frame-name)
        (persp-setup-for name
          (switch-to-buffer (create-org-buffer name))))))
  (add-hook 'persp-created-hook 'persp-init-org-buffer))

(defun enable-superword-in-find-file ()
  "Enable superword-mode in the minibuffer for find-file.
This makes it so that e.g. M-DEL deletes entire directory names."
  (when (eq this-command 'find-file)
    (superword-mode 1)))

(add-hook 'minibuffer-setup-hook 'enable-superword-in-find-file)

;; Subword mode changes the definition of a word so that word-based commands
;; stop inside symbols with mixed uppercase and lowercase letters,
;; e.g. "GtkWidget", "EmacsFrameClass", "NSGraphicsContext".
;;
;; Here we call these mixed case symbols ‘nomenclatures’.  Each capitalized (or
;; completely uppercase) part of a nomenclature is called a ‘subword’.  Here are
;; some examples:
;;
;;  Nomenclature           Subwords
;;  ===========================================================
;;  GtkWindow          =>  "Gtk" and "Window"
;;  EmacsFrameClass    =>  "Emacs", "Frame" and "Class"
;;  NSGraphicsContext  =>  "NS", "Graphics" and "Context"
(add-hook 'prog-mode-hook 'subword-mode)

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(defun disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(let* ((local-bin (expand-file-name "~/.local/bin"))
       (current-path (getenv "PATH")))
  (when (file-directory-p local-bin)
    ;; Add ~/.local/bin to exec-path if it's not already there
    ;; 't' adds it to the front of the list for higher precedence
    (unless (member local-bin exec-path)
      (add-to-list 'exec-path local-bin t))

    ;; Add ~/.local/bin to the PATH environment variable for subprocesses
    ;; Only add if it's not already in the PATH
    (unless (string-match-p (regexp-quote local-bin) current-path)
      (setenv "PATH" (concat local-bin ":" current-path)))))
