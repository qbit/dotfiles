(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(file-exists-p "/usr/local/go/misc/emacs/go-mode-load.el")
	       (add-to-list 'auto-mode-alist '("go$" . go-mode))

(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/")
	     '("melpa" .
	       "http://melpa.milkbox.net/packages/"))
(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'nlinum)

;(if (daemonp)
;    (add-hook 'after-make-frame-functions
;              (lambda (frame)
;                (load-theme 'tronesque t)))
;    (load-theme 'tronesque t))


;(load-theme 'zenburn 1)
;(load-theme 'tango-dark 1)
;(load-theme 'tronesque t)
;(load-theme 'solarized-dark t)
(load-theme 'flatland t)

(evil-mode 1)

(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  "))
