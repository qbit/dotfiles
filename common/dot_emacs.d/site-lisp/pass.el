;;; pass --- pass config for emacs
;;; Commentary:
;;; Code:

(if (file-directory-p "/usr/local/share/examples/password-store/emacs/")
    (add-to-list 'load-path "/usr/local/share/examples/password-store/emacs/"))

(require 'password-store)

(provide 'pass)
