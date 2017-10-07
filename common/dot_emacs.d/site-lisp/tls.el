;;; tls --- tls specific confurations
;;; Commentary:
;;; Code:

(setq tls-checktrust t)
(setq tls-program (list "nc -z -v -c -e %h %h %p"))
(setq tls-success "TLS handshake negotiated")
(setq gnutls-trustfiles (list "/etc/ssl/cert.pem"))
(setq gnutls-verify-error t)

(provide 'tls)

;;; tls.el ends here
