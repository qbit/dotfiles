;;; config --- emacs specific configurations
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(setq auto-mode-alist
      (append
       (list
        '("\\.gpg$" . sensitive-minor-mode)
        )
       auto-mode-alist))

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

(setq x-select-enable-clipboard-manager nil)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")
			(java-mode . "java")
			(awk-mode . "awk")
			(other . "OpenBSD")))

(setq lsp-enable-flycheck t
      lsp-enable-eldoc t
      lsp-enable-completion-at-point t)

(provide 'config)

;;; config.el ends here
