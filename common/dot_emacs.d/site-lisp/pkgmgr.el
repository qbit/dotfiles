(defun install-if-missing (lst)
  "Install missing packages on init"
  (when lst
    (let ((pkg (car lst))
	  (rest (cdr lst)))
      (unless (package-installed-p pkg nil)
	(package-install pkg))
      (install-if-missing rest))))

(require 'package)
(setq package-archives '(("melpa" .
			  "http://melpa.org/packages/")
			 ("gnu" .
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
   aggressive-indent
   arduino-mode
   erlang
   emms
   fireplace
   flycheck
   fsharp-mode
   geiser
   git-gutter
   go-autocomplete
   go-eldoc
   go-mode
   golint
   guru-mode
   haskell-mode
   helm
   helm-lobsters
   js2-mode
   magit
   moe-theme
   nlinum
   nyan-mode
   org
   ox-reveal
   powerline
   psci
   purescript-mode
   rainbow-mode
   ruby-mode
   rust-mode
   scpaste
   tronesque-theme
   weather-metno
   zenburn-theme
   ))

(provide 'pkgmgr)
