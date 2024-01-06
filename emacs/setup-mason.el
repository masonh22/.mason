(provide 'setup-mason)

;; This includes all necessary config modules
(unless (version< emacs-version "29.1")
  (require 'treesit-mason))
(require 'config-mason)
(require 'packages-mason)
