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
			  "https://elpa.gnu.org/packages/")))

;;; Update our package contents if we have modified our pkgs.el file
;;; since the last day or if we don't have package-archive-contents
(cond ((< pkgs-mod-time 300) (package-refresh-contents))
      ((symbolp package-archive-contents) (package-refresh-contents)))

(setq my-packages
      '(
	(all-the-icons    . "melpa-stable")
	(base16-theme     . "melpa-stable")
	(counsel          . "melpa-stable")
	(flycheck         . "melpa-stable")
	(git-gutter       . "melpa-stable")
	(go-eldoc         . "melpa-stable")
	(go-mode          . "melpa-stable")
	(golint           . "melpa-stable")
	(ivy              . "melpa-stable")
	(magit            . "melpa-stable")
	(forge            . "melpa-stable")
	(neotree          . "melpa-stable")
	(nord-theme       . "melpa-stable")
	(org              . "org")
	(org-plus-contrib . "org")
	(rust-mode        . "melpa-stable")
	(scad-mode        . "melpa-stable")
	(scpaste          . "melpa-stable")
	(slime            . "melpa-stable")
	(spaceline        . "melpa-stable")
	(swiper           . "melpa-stable")
	(web-mode         . "melpa-stable")
	))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages 'my-packages))

(install-if-missing my-packages)

(provide 'pkgs)

;;; pkgs.el ends here
