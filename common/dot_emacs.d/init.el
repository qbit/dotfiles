
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setenv "PATH"
	(concat
	 (getenv "PATH") ";"
	 "~/go/bin" ";"
	 "~/.go/bin" ";"
	 "~/node_bin/node_modules/.bin"))

(let ((default-directory user-emacs-directory))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '("./site-lisp")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(require 'pkgmgr)
;;(require 'externals)
(require 'keycuts)
(require 'location)
(require 'orger)
(require 'browse)

(require 'nlinum)

(require 'openbsd-knf-style)
(c-add-style "OpenBSD" openbsd-knf-style)
(setq c-default-style '((c-mode . "OpenBSD")
			(java-mode . "java")
			(awk-mode . "awk")
			(other . "OpenBSD")))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (which-func-mode 1))))

(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-hook 'mail-mode-hook
          (lambda ()
            (turn-on-flyspell)
	    (auto-fill-mode 1)
	    ))

(setq whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80)

;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(add-hook 'before-save-hook nil)

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

(global-whitespace-mode 1)
(global-font-lock-mode 1)

(nyan-mode 1)

(set-location-by-ip)

(set-face-attribute 'default nil :height 100)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/eink-emacs")

(setq backup-directory-alist '(("." . "~/.esaves")))
(setq inhibit-startup-screen t)
(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>λ\n]*#?[]#$%>λ].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/localshare/emacs/site-lisp/mu4e/")
  (require 'mail))

(file-exists-p "~quicklisp/slime-helper.el")
(load (expand-file-name "~/quicklisp/slime-helper.el")
      (setq inferior-lisp-program "sbcl"))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(ac-config-default)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(add-hook 'after-init-hook #'global-flycheck-mode)

;;(load-theme 'misterioso)
;;(load-theme 'zenburn 1)
;;(load-theme 'tango-dark 1)
;;(load-theme 'tronesque t)
;;(load-theme 'solarized-dark t)
;;(load-theme 'flatland t)
(require 'moe-theme)
;;(load-theme 'moe-dark t)
(require 'moe-theme-switcher)
;;(powerline-moe-theme)
;;(load-theme 'eink-light t)

(require 'powerline)
(powerline-default-theme)

;;(require 'golden-ratio)
;;(golden-ratio-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#ffffff" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#ffffff" :background "#666666" :box nil)))))

(defun comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

(global-set-key (kbd "C-;") #'comment-line)

(defalias 'yes-or-no-p 'y-or-n-p)
;;(global-set-key (kbd "<C-tab>") 'hippie-expand)

(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)
(setq scpaste-http-destination "http://akb.io" scpaste-scp-destination
      "akb.io:/var/www/htdocs")

(setq scpaste-user-name "Aaron" scpaste-user-address
      "http://akb.io/")

;; Go hooks
(add-hook 'before-save-hook 'gofmt-before-save)

(global-git-gutter-mode +1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   (quote
    ("092351ddcd5b95345bbea075361b28cf8a8567d95dc24448589c3a3692bf7d40" "0b591fefbcbb4ddeffc570233c3cd378a47c2535bf289b5a4533f64f22da2f87" default)))
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(global-aggressive-indent-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(package-selected-packages
   (quote
    (web-mode scpaste scad-mode rust-mode racket-mode psci powerline php-mode ox-reveal org-plus-contrib nyan-mode nlinum moe-theme magit lua-mode js2-mode jinja2-mode helm haskell-mode golint go-eldoc go-autocomplete git-gutter geiser flycheck arduino-mode)))
 '(purescript-mode-hook (quote (capitalized-words-mode turn-on-purescript-indent)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)))))

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(set-face-attribute 'default t :height 100)


