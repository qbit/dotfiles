(defun install-if-missing (lst)
  "Install missing packages on init"
  (when lst
    (let ((pkg (car lst))
	  (rest (cdr lst)))
      (unless (package-installed-p pkg nil)
	(package-install pkg))
      (install-if-missing rest))))

(require 'package)

(setq tls-checktrust t)
(setq tls-program (list "nc -v -c -e %h %h %p"))
(setq tls-success "TLS handshake negotiated")
(setq gnutls-trustfiles (list "/etc/ssl/cert.pem"))
(setq gnutls-verify-error t)

(setq package-archives '(("melpa" .
			  "https://melpa.org/packages/")
			 ("org" .
			  "http://orgmode.org/elpa/") ;; Get you some https guys!
			 ("gnu" .
			  "https://elpa.gnu.org/packages/")
			 ("marmalade" .
			  "https://marmalade-repo.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((org-plus-contrib                  . "org")
	  (org                               . "org")
	  (helm                              . "melpa-stable")
	  (magit                             . "melpa-stable")
	  (flycheck                          . "melpa-stable")
	  (flycheck                          . "melpa-stable")
	  )))

(install-if-missing
 '(
   all-the-icons
   base16-theme
   flycheck
   git-gutter
   go-autocomplete
   go-eldoc
   go-mode
   golint
   helm
   jinja2-mode
   magit
   neotree
   nlinum
   nyan-mode
   org
   org-plus-contrib
   php-mode
   rust-mode
   scad-mode
   scpaste
   spaceline
   web-mode
   ))

(provide 'pkgmgr)
