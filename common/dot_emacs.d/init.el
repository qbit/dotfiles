(let ((default-directory user-emacs-directory))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '("./site-lisp")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(require 'pkgmgr)
;(require 'externals)
(require 'keycuts)
(require 'orger)
(require 'browse)

(require 'guru-mode)
(require 'nlinum)

(set-face-attribute 'default nil :height 100)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq backup-directory-alist '(("." . "~/.esaves")))
(setq inhibit-startup-screen t)
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>λ\n]*#?[]#$%>λ].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/localshare/emacs/site-lisp/mu4e/")
  (require 'mail))

(file-exists-p "~quicklisp/slime-helper.el")
	       (load (expand-file-name "~/quicklisp/slime-helper.el")
	       (setq inferior-lisp-program "sbcl"))

(guru-global-mode +1)

(powerline-default-theme)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(rainbow-mode +1)

;(load-theme 'zenburn 1)
;(load-theme 'tango-dark 1)
;(load-theme 'tronesque t)
;(load-theme 'solarized-dark t)
;(load-theme 'flatland t)
(require 'moe-theme)
;(load-theme 'moe-dark t)
(require 'moe-theme-switcher)
(powerline-moe-theme)

(global-aggressive-indent-mode)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<C-tab>") 'hippie-expand)

(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)
(setq scpaste-http-destination "http://akb.io" scpaste-scp-destination
"akb.io:/var/www/htdocs")

(setq scpaste-user-name "Aaron" scpaste-user-address
   "http://akb.io/")

(global-git-gutter-mode +1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  "))


(set-face-attribute 'default t :height 100)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
