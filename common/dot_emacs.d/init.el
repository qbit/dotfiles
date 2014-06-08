(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

;(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)

(require 'package)
(add-to-list 'package-archives 
	     '("melpa" .
	       "http://melpa.milkbox.net/packages/")
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

(load-theme 'zenburn 1)

(evil-mode 1)

(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:modified-sign "  ") ;; two space
 '(git-gutter:added-sign "++")    ;; multiple character is OK
 '(git-gutter:deleted-sign "--"))

(require 'nlinum)
;(global-linum-mode 1)
;(setq linum-format "%d ")
