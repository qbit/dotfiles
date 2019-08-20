(package-initialize)

;;(toggle-debug-on-quit)
;;(toggle-debug-on-error)

(load "server")
(unless (server-running-p) (server-start))

(setq custom-file "~/.emacs.d/site-lisp/custom.el")

(let ((default-directory user-emacs-directory))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '("./site-lisp")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(require 'notifications)
(require 'functions)
(require 'tls)
(require 'pkgs)
(require 'config)
(require 'hooks)
(require 'keyboard)
(require 'orgie)
(if (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
    (require 'email))
(if (file-directory-p "/usr/local/share/examples/password-store/emacs/")
    (require 'pass))
(require 'ui)

(if (file-exists-p "~/.emacs.d/diary")
    (diary))
(eshell)
