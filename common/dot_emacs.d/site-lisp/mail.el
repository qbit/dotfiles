(setq
 user-mail-address "deftly@gmail.com"
 user-full-name "Aaron Bieber")

(require 'mu4e)
(require 'org-mu4e)
(require 'smtpmail)

(if (file-directory-p "/data/user_storage/qbit/Maildir")
    (setq mu4e-maildir (expand-file-name "/data/user_storage/qbit/Maildir")))

(setq mu4e-compose-signature "")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder "/[Gmail].Sent Mail")
(setq mu4e-trash-folder "/[Gmail].Trash")

(global-set-key (kdb "C-c e") 'mu4e-mark-execute-all)

(set mu4e-sent-messages-behavior 'delete)

(setq mu4e-headers-skip-duplicates t)
(setq mu4e-html2text-command "html2text -b 72")

(setq send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials
      '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials
      (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

(provide 'mail)
