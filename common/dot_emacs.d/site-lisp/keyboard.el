
;;; keyboard.el --- various key bindings for mah emacs
;;; Copyright (C) 2017 Aaron Bieber <aaron@bolddaemon.com>
;;; Commentary:
;;; Code:

(require 'ido-completing-read+)
(global-set-key (kbd "<backtab>") 'hippie-expand)
(global-set-key "\M-g" 'magit-status)

;;(counsel-mode)
;;(global-set-key "\C-s" 'swiper)
;;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
;;(global-set-key (kbd "C-c z") 'counsel-fzf)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-record-commands t)
(ido-mode 1)
(ido-ubiquitous-mode 1)

;(global-set-key "\C-s" 'swiper)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(provide 'keyboard)

;; keyboard.el ends here
