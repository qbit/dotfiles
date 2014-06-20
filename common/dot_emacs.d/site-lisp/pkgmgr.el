(defun install-if-missing (pkg) 
  "Install missing packages on init"
  (unless (package-installed-p pkg nil)
    (package-install pkg)))

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/")
	     '("mlpa" .
	       "http://melpa.milkbox.net/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(install-if-missing 'erlang)
(install-if-missing 'git-gutter)
(install-if-missing 'js2-mode)
(install-if-missing 'magit)
(install-if-missing 'magit-tramp)
(install-if-missing 'nlinum)
(install-if-missing 'org)
(install-if-missing 'rainbow-mode)
(install-if-missing 'ruby-mode)
(install-if-missing 'slime)
(install-if-missing 'tronesque-theme)
(install-if-missing 'zenburn-theme)

(provide 'pkgmgr)
