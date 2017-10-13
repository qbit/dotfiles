;;; config --- emacs specific configurations
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(provide 'config)

;;; config.el ends here
