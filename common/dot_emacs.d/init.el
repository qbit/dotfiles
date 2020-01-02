(package-initialize)

;;(toggle-debug-on-quit)
;;(toggle-debug-on-error)

(load "server")
(unless (server-running-p) (server-start))

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

(use-package base16-theme)
;;(use-package berrys-theme)
;;(use-package nord-theme)
;;(use-package moe-theme)

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
