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
(require 'keycuts)

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

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)
(setq scpaste-http-destination "http://akb.io" scpaste-scp-destination
"akb.io:/var/www/htdocs")

(setq scpaste-user-name "akb.io" scpaste-user-address
   "http://akb.io/")

(global-git-gutter-mode +1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(org-agenda-files (quote ("~/org/kahn/LinearAlgebra.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
