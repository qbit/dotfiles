(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(file-exists-p "/usr/local/go/misc/emacs/go-mode-load.el")
	       (add-to-list 'auto-mode-alist '("go$" . go-mode))

(let ((default-directory user-emacs-directory))
      (normal-top-level-add-subdirs-to-load-path))

(defvar site-lisp-directory (concat user-emacs-directory "site-lisp/"))

(file-exists-p "~quicklisp/slime-helper.el")
	       (load (expand-file-name "~/quicklisp/slime-helper.el")
	       (setq inferior-lisp-program "sbcl"))

(require 'pkgmgr)
;(require 'externals)

(tool-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(rainbow-mode +1)

(require 'nlinum)

;(load-theme 'zenburn 1)
;(load-theme 'tango-dark 1)
;(load-theme 'tronesque t)
;(load-theme 'solarized-dark t)
(load-theme 'flatland t)

;(evil-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<C-tab>") 'hippie-expand)

(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  "))
