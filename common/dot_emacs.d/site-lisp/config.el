;;; config --- emacs specific configurations
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

(setq x-select-enable-clipboard-manager nil)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(setq lsp-enable-flycheck t
      lsp-enable-eldoc t
      lsp-enable-completion-at-point t)

(provide 'config)

;;; config.el ends here
