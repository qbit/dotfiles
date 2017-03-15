(defun install-if-missing (lst)
  "Install missing packages on init"
  (when lst
    (let ((pkg (car lst))
	  (rest (cdr lst)))
      (unless (package-installed-p pkg nil)
	(package-install pkg))
      (install-if-missing rest))))

(require 'package)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python2.7 -m certifi")))))
  (setq tls-program
	(list
	 (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
		 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

(setq package-archives '(("melpa" .
			  "https://melpa.org/packages/")
			 ("org" .
			  "http://orgmode.org/elpa/") ;; Get you some https guys!
			 ("gnu" .
			  "https://elpa.gnu.org/packages/")
			 ("marmalade" .
			  "https://marmalade-repo.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(install-if-missing
 '(
   arduino-mode
   flycheck
   geiser
   git-gutter
   go-autocomplete
   go-eldoc
   go-mode
   golint
   haskell-mode
   helm
   jinja2-mode
   js2-mode
   magit
   moe-theme
   nlinum
   nyan-mode
   org-plus-contrib
   ox-reveal
   php-mode
   powerline
   psci
   racket-mode
   rust-mode
   scad-mode
   scpaste
   web-mode
   ))

(provide 'pkgmgr)
