(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/")
	     '("melpa" .
	       "http://melpa.milkbox.net/packages/"))
(package-initialize)

(require 'nlinum)

(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)

(load-theme 'zenburn 1)
;(load-theme 'tronesque t)

(evil-mode 1)

(require 'slime-autoloads)
    (setq inferior-lisp-program "sbcl")

(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  "))
