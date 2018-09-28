;;; ui --- ui defines components of the emacs UI
;;; Commentary:
;;; Code:

;; for some reason I can't set Go Regular here.. Moved it to Xdefaults
;;(set-default-font "Go Regular:pixelsize=15")
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(global-font-lock-mode 1)

(setq initial-scratch-message (concat
			       (shell-command-to-string "fortune -o | sed -e 's/^/;; /g'")
			       "\n\n"))

(setq whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80)

(set-face-attribute 'default t :height 100)

;;(load-theme 'base16-default-light 1)
(load-theme 'nord 1)
(require 'spaceline-config)
(spaceline-emacs-theme)

(ivy-mode 1)

(setq x-selection-timeout 10)

(provide 'ui)

;;; ui.el ends here
