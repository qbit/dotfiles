;;; config --- emacs specific configurations
;;; Commentary:
;;; Code:

(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(provide 'config)

;;; config.el ends here
