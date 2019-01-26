(package-initialize)

;;(toggle-debug-on-quit)
;;(toggle-debug-on-error)

(setq custom-file "~/.emacs.d/site-lisp/custom.el")

(let ((default-directory user-emacs-directory))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '("./site-lisp")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(require 'functions)
(require 'tls)
(require 'pkgs)
(require 'config)
(require 'mode)
(require 'hooks)
(require 'keyboard)
(require 'orgie)
(require 'email)
(require 'pass)
(require 'ui)
