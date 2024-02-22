(provide 'modeline-mason)

;; Default mode-line
(defvar-local default-mode-line
    '("%e"
      (:propertize
       (""
        mode-line-front-space ;; '-' for terminals, ' ' for gui
        mode-line-mule-info   ;; information on character sets
        mode-line-client      ;; adds an '@' if this is emacsclient
        mode-line-modified    ;; Modified and read-only status
        mode-line-remote      ;; '-' for local, '@' for remote
        )
       display (min-width (5.0)))
      mode-line-frame-identification  ;; Frame title
      mode-line-buffer-identification ;; Buffer name
      "   "
      mode-line-position   ;; Line, column, and position
      mode-line-modes      ;; Major and minor modes
      mode-line-misc-info  ;; misc things
      mode-line-end-spaces ;; A line of dashes for terminals
      ))

;; My mode-line, inspired by https://protesilaos.com/emacs/dotemacs
;; want to include
;; - file status
;; - file name
;; - line number and column
;; - major mode
;; - minor modes with status, like projectile project, flymake, maybe eglot
;;   - only show this for mode-line-window-selected-p

(defun mason-buffer-id-face ()
  (cond
   ((mode-line-window-selected-p)
    'mode-line-buffer-id)))

(defvar-local mason-buffer-identification
    '(:eval
      (propertize (buffer-name)
                  'face (mason-buffer-id-face)
                  'mouse-face 'mode-line-highlight))
  "Mode line construct for identifying the buffer being displayed.
The current buffer is propertized with 'mode-line-buffer-id' face.")

(defun mason-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "∘"))))
    (propertize indicator 'face 'shadow)))

(defvar-local mason-major-mode
    (list
     "  "
     "%["
     '(:eval
       (concat
        (mason-major-mode-indicator)
        " "
        (propertize
         ;; mode-name ; doesn't work with tuareg!
         (capitalize (string-replace "-mode" "" (symbol-name major-mode)))
         'mouse-face 'mode-line-highlight)))
     "%]")
  "Mode line construct for major mode.")

(defvar-local mason-process
    (list '("" mode-line-process))
  "Mode line construct for running process indicator.")

(defvar-local mason-flymake
    '(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         "  "
         flymake-mode-line-exception
         flymake-mode-line-counters)))
  "Mode line construct for flymake info.")

;; Remove eglot from misc info
(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local mason-eglot
    '(:eval
      (when (and (featurep 'eglot)
                 (mode-line-window-selected-p))
        (list
         "  "
         '(eglot--managed-mode eglot--mode-line-format)))))

(defvar-local mason-projectile
    '(:eval
      (let ((project-name (projectile-project-name))
            (project-type (projectile-project-type)))
        (when (or project-type
                  (and project-name
                       ((not string= project-name "-"))))
          (list
           "  "
           (format "[%s%s]"
                   (or project-name "-")
                   (if project-type
                       (format ":%s" project-type)
                     "")))))))

(defvar-local mason-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        (list
         "  "
         mode-line-misc-info)))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;; TODO not sure if this is necessary
(when (version< emacs-version "30")
  (defvar-local mode-line-format-right-align "")
  (put 'mode-line-format-right-align 'risky-local-variable t))

(dolist (construct '(mason-buffer-identification
                     mason-major-mode
                     mason-process
                     mason-flymake
                     mason-eglot
                     mason-projectile
                     mason-misc-info
                     ))
  (put construct 'risky-local-variable t))

(unless (version< emacs-version "30")
  (setq mode-line-right-align-edge 'right-margin))

(defvar-local mason-mode-line
    '("%e"
      (:propertize
       (""
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        )
       display (min-width (5.0)))
      '(:eval (buffer-name))
      "  "
      mason-buffer-identification
      mason-major-mode
      mason-process
      mason-projectile

      mode-line-format-right-align
      mason-flymake
      mason-eglot ;; TODO eglot and projectile seem redundant... find a way to integrate them
      mason-misc-info
      ))

(setq-default mode-line-format mason-mode-line)
