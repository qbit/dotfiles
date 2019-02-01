;;; keyboard.el --- various key bindings for mah emacs
;;; Copyright (C) 2017 Aaron Bieber <aaron@bolddaemon.com>
;;; Commentary:
;;; Code:

(global-set-key (kbd "<backtab>") 'hippie-expand)
(global-set-key "\M-g" 'magit-status)

(counsel-mode)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c z") 'counsel-fzf)

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(provide 'keyboard)

;; keyboard.el ends here
