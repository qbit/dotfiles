(defun install-if-missing (lst) 
  "Install missing packages on init"
  (when lst
    (let ((pkg (car lst))
	  (rest (cdr lst)))
    (unless (package-installed-p pkg nil)
      (package-install pkg))
    (install-if-missing rest))))

(require 'package)

(setq package-archives '(("gnu" .
			  "http://elpa.gnu.org/packages/")
			 ("marmalade" .
			  "http://marmalade-repo.org/packages/")
			 ("org" .
			  "http://orgmode.org/elpa/")))
			 
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(install-if-missing 
 '(
   erlang
   git-gutter
   guru-mode
   js2-mode
   magit
   magit-tramp
   nlinum
   org
   rainbow-mode
   ruby-mode
   rust-mode
   scpaste
   tronesque-theme
   zenburn-theme
))

(provide 'pkgmgr)
