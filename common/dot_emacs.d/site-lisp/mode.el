;;; mode --- configuration for various writing modes
;;; Commentary:
;;; Code:

(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")
			(java-mode . "java")
			(awk-mode . "awk")
			(other . "OpenBSD")))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (which-func-mode 1))))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook
	  (lambda ()
	    (set-background-color "white")
	    (set-foreground-color "black")
	    (turn-on-flyspell)
	    (auto-fill-mode 1)
	    ))

(add-hook 'before-save-hook (lambda ()
			      (delete-trailing-whitespace)
			      ))

(add-hook 'before-save-hook 'gofmt-before-save)
(global-git-gutter-mode +1)

(provide 'mode)

;;; mode.el ends here
