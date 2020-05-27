;;; init.el -- My emacs config
;;; Commentary:

;;; Code:
(require 'package)

(setq tls-checktrust t)
(setq gnutls-verify-error t)

(setenv "PATH" (concat (getenv "PATH") ":~/go/bin:~/ruby_bin/bin"))
(setenv "GEM_HOME" (expand-file-name "~/ruby_bin"))
(setenv "GEM_BIN" (expand-file-name "~/ruby_bin/bin"))
(setenv "BUNDLE_PATH" (expand-file-name "~/ruby_bin"))
(setenv "JAVA_HOME" "/usr/local/jdk-11/")
(setq exec-path (append exec-path '("~/go/bin:~/ruby_bin/bin")))

;;(setq package-enable-at-startup nil)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "melpa-stable" package-archives)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(require 'org-install)
(require 'org-habit)

;; Prevent custom file from cluttering SCM
(setq custom-file (make-temp-file ""))

;; Y is sufficient
(fset 'yes-or-no-p 'y-or-n-p)

;; I need all the spelling help I can get
;; (setq ispell-program-name "aspell")

;; Kill terminal buffers on exit so I din't have to kill the buffer after I exit.
(defadvice term-handle-exit
    (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;; ----------- UI ----------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'eink 1)

(setq-default fill-column 80)
(add-hook 'text-mode-hook (lambda ()
			    (auto-fill-mode 1)
			    (turn-on-flyspell)))

(setq inhibit-startup-screen t
      whitespace-style '(trailing lines space-before-tab)
      whitespace-line-column 80
      x-selection-timeout 10)

(if (file-executable-p "/usr/games/fortune")
    (setq initial-scratch-message
	  (concat
	   (shell-command-to-string "fortune | sed -e 's/^/;; /g'")
	   "\n\n")))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Hopefully this helps with the OpenBSD org-capture lockups
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(show-paren-mode +1)
(global-font-lock-mode 1)

(setq x-select-enable-clipboard-manager nil)

;; ----------- Configurations ------------

(setq backup-directory-alist '(("." . "~/.emacs-saves")))
(setq auto-mode-alist
      (append
       (list
        '("\\.gpg$" . sensitive-minor-mode)
        )
       auto-mode-alist))

(setq auth-sources
      '((:source "~/.authinfo.gpg")))


;; ----------- Packages -------------

(use-package keychain-environment)
(use-package all-the-icons
  :init
  (if (not
       (file-exists-p (concat (getenv "HOME") "/.local/share/fonts/all-the-icons.ttf")))
      (all-the-icons-install-fonts "t")))
(use-package all-the-icons-ivy)
(use-package counsel)
(use-package smex)
(use-package ivy
  :hook (after-init . ivy-mode)
  :bind
  ("C-s"     . swiper)
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x b"   . ivy-switch-buffer)
  ("C-c n"   . counsel-fzf))

(use-package magit
  :bind ("C-c m" . magit-status)
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq dired-use-ls-dired nil ;; No --dired on OpenBSD
	  treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-follow-after-init             t
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode))

(use-package company
  :config
  (setq company-tooltip-limit 20
	company-minimum-prefix-length 1
	company-idle-delay 0
	company-echo-delay 0)
  :hook (prog-mode . company-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package git-gutter
  :hook
  (after-init . global-git-gutter-mode))

(use-package go-add-tags)
(use-package go-mode
  :bind
  ("C-c t" . go-add-tags))
(use-package go-eldoc
  :hook
  (go-mode . go-eldoc-setup))
(use-package gorepl-mode
  :after go-mode)

(use-package lsp-mode
  :hook ((go-mode   . lsp-deferred)
	 (ruby-mode . lsp))
  :commands (lsp lsp-deferred))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/Docs/plantuml.jar"))

(use-package org
  :hook
  (org-mode . (lambda ()
		(turn-on-flyspell)
		(auto-revert-mode)
		(auto-fill-mode 1)))
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)
  :config
  (setq org-directory "~/org"
	org-agenda-files (file-expand-wildcards "~/org/*.org")
	org-journal-dir "~/org/journal/"
	org-log-done 'time
	org-export-with-planning t
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
			    (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
			    (sequence "|" "CANCELED(c)"))
	org-agenda-custom-commands '(("d" "Daily habits"
				      ((agenda ""))
				      ((org-agenda-show-log t)
				       (org-agenda-ndays 7)
				       (org-agenda-log-mode-items '(state))
				       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
				     ("o" "OpenBSD"
				      ((agenda ""))
				      ((org-adenda-show-log t)
				       (org-agenda-ndays 7)
				       (org-agenda-log-mode-items '(state))
				       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":OpenBSD:"))))
				     ("p" "OpenBSD Ports"
				      ((agenda ""))
				      ((org-adenda-show-log t)
				       (org-agenda-ndays 7)
				       (org-agenda-log-mode-items '(state))
				       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":ports:")))))
	org-capture-templates `(("t" "TODO"
				 entry (file+headline "~/org/todo.org" "TODOs")
				 ,(concat
				   "* TODO %?\n"
				   ":PROPERTIES:\n"
				   ":LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)\n"
				   ":END:\n") :prepend t)
				("f" "TODO with File"
				 entry (file+headline "~/org/todo.org" "TODOs")
				 ,(concat
				   "* TODO %?\n"
				   ":PROPERTIES:\n"
				   ":LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)\n"
				   ":END:\n"
				   "%i\n  %a") :prepend t)
				("b" "Bug"
				 entry (file+olp+datetree "~/org/bugs.org" "Bugs")
				 "* BUG %?\nEntered on %U\n  :PROPERTIES:\n  :FILE: %a\n  :END:\n" :prepend t)
				("p" "Protocol"
				 entry (file+headline "~/org/links.org" "Links")
				 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
				("L" "Protocol Link" entry (file+headline "~/org/links.org" "Links")
				 "* %? %:link\n%:description\n")
				("j" "Journal"
				 entry (file+olp+datetree "~/org/journal.org")
				 "* %?\nEntered on %U\n  %i\n  %a" :prepend t))))
(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/org")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package scpaste
  :init
  (setq scpaste-http-destination "https://suah.dev/p"
	scpaste-scp-destination "suah.dev:/var/www/htdocs/p"))

(use-package fish-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(load "server")
(unless (server-running-p) (server-start))
