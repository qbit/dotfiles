;;; pkgs --- installtion / package management
;;; Commentary:
;;; Code:

(setq pkgs-file-name
      (expand-file-name "~/.emacs.d/site-lisp/pkgs.el"))

(setq pkgs-mod-time
      (cadr (time-since
	     (elt (file-attributes pkgs-file-name) 5))))

(setq package-archives '(("org" .
			  "https://orgmode.org/elpa/")
			 ("melpa-stable" .
			  "https://stable.melpa.org/packages/")
			 ("melpa" .
			  "https://melpa.org/packages/")
			 ("gnu" .
			  "https://elpa.gnu.org/packages/"))
      package-pinned-packages '(
				(base16-theme     . "melpa-stable")
				(company-lsp      . "melpa-stable")
				(flycheck         . "melpa-stable")
				(forge            . "melpa-stable")
				(ido-completing-read+ . "melpa-stable")
				(git-gutter       . "melpa-stable")
				(go-eldoc         . "melpa-stable")
				(go-mode          . "melpa-stable")
				(lsp-mode         . "melpa-stable")
				(magit            . "melpa-stable")
				(org              . "org")
				(org-plus-contrib . "org")
				(rubocop          . "melpa-stable")
				(rust-mode        . "melpa-stable")
				(scpaste          . "melpa-stable")
				(slime            . "melpa-stable")
				(smex             . "melpa-stable")
				(web-mode         . "melpa-stable")
				(yaml-mode        . "melpa-stable")))

;;; Update our package contents if we have modified our pkgs.el file
;;; since the last day or if we don't have package-archive-contents
(cond ((< pkgs-mod-time 300) (package-refresh-contents))
      ((symbolp package-archive-contents) (package-refresh-contents)))

(install-if-missing package-pinned-packages)

(defun update-packages ()
  (interactive)
  (package-refresh-contents)
  ;; Need to switch to the updates buffer here
  (package-menu-mark-upgrades)
  (package-menu-execute 'noquery))

(provide 'pkgs)

;;; pkgs.el ends here
