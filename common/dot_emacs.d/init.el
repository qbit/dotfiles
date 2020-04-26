(require 'package)
(setq package-archives '(("org" .
			       "https://orgmode.org/elpa/")
			 ("gnu" .
			  	"https://elpa.gnu.org/packages/")
			     ("melpa" .
			           "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
      (package-install 'use-package))

(setq custom-file "~/.emacs.d/site-lisp/custom.el")

(let ((default-directory user-emacs-directory))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path)))
	   (append
	    (copy-sequence (normal-top-level-add-to-load-path '("./site-lisp")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

(if (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
    (require 'email))
(if (file-exists-p "~/.emacs.d/diary")
    (diary))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;(use-package base16-theme)
;;(use-package berrys-theme)
;;(use-package nord-theme)
(use-package moe-theme)

(use-package flycheck)
(use-package forge)
(use-package git-gutter)
(use-package go-mode)
(use-package ido-completing-read+)
(use-package lsp-mode)
(use-package magit)
(use-package org)
(use-package scpaste)
(use-package smex)
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/")
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-show-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert))))

(require 'notifications)
(require 'tls)
(require 'config)
(require 'hooks)
(require 'keyboard)
(require 'orgie)
(if (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
      (require 'email))
(require 'ui)

(load "server")
(unless (server-running-p) (server-start))
