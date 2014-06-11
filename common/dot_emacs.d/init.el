(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))

(file-exists-p "/usr/local/go/misc/emacs/go-mode-load.el")
	       (add-to-list 'auto-mode-alist '("go$" . go-mode))

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

(progn
(load-theme 'zenburn 1)
(evil-mode 1)
(global-linum-mode 1)
(setq linum-format "%d "))

