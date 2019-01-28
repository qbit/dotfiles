;;; ui --- ui defines components of the emacs UI
;;; Commentary:
;;; Code:


(setq inhibit-startup-screen t)
(setq initial-scratch-message
      (concat
       (shell-command-to-string "fortune -o | sed -e 's/^/;; /g'")
       "\n\n"))
(setq whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80)
(setq x-selection-timeout 10)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(global-font-lock-mode 1)
(display-battery-mode 1)
(ivy-mode 1)
(global-git-gutter-mode +1)

;;; Nord theme
;;; - currently there is a bug that prevents loading as daemon:
;;;   https://github.com/arcticicestudio/nord-emacs/issues/59
;;(require 'spaceline-config)
;;(spaceline-emacs-theme)
;; (load-theme 'nord 1)

;; (load-theme 'base16-default-dark 1)

(require 'moe-theme)
(require 'powerline)
(require 'moe-theme-switcher) ;; Gives us dark at night, light during the day

(provide 'ui)

;;; ui.el ends here
