;;; tls --- tls specific confurations
;;; Commentary:
;;; Code:

(setq tls-checktrust t)
(cond
 ((string-equal system-type "darwin")
  (progn
    (message "On macOS, not using nc for tls-program")))
 ((string-equal system-type "berkeley-unix")
  (progn
    (message "On BSD, using nc for tls-program")
    (setq tls-program (list "nc -z -v -c -e %h %h %p"))
    (setq tls-success "TLS handshake negotiated"))))
(setq gnutls-trustfiles (list "/etc/ssl/cert.pem"))
(setq gnutls-verify-error t)

(provide 'tls)

;;; tls.el ends here
