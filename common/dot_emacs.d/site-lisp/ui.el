;;; ui --- ui defines components of the emacs UI
;;; Commentary:
;;; Code:

(add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono-12" ))
(set-face-attribute 'default t :font "Fantasque Sans Mono-12")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Hopefully this helps with the OpenBSD org-capture lockups
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq inhibit-startup-screen t
      initial-scratch-message
      (concat
       (shell-command-to-string "fortune | sed -e 's/^/;; /g'")
       "\n\n")
      whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80
      x-selection-timeout 10)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(global-font-lock-mode 1)
(display-battery-mode 1)
(display-time-mode 1)
(global-git-gutter-mode +1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'base16-nord 1)
;;(load-theme 'eink 1)

(provide 'ui)

;;; ui.el ends here
