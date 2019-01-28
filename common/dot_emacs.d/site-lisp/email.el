;;; email --- config for mu4e
;;; Commentary:
;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-maildir "~/Maildir/fastmail")
(setq mail-user-agent 'mu4e-user-agent)
(setq message-kill-buffer-on-exit t)

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent Items")
(setq mu4e-trash-folder  "/Trash")

(setq mu4e-maildir-shortcuts
      '( ("/INBOX"        . ?i)
	 ("/Archive"      . ?a)
	 ("/Sent Items"   . ?s)))

(setq org-mu4e-link-query-in-headers-mode nil)

(setq mu4e-attachment-dir
      (lambda (fname mtype)
	(cond
	 ((and fname (string-match "\\.diff$" fname))  "~/patches")
	 ((and fname (string-match "\\.diff.gz$" fname))  "~/patches")
	 (t "~/Downloads"))))

(setq mu4e-bookmarks
      `( ,(make-mu4e-bookmark
	   :name  "Unread messages"
	   :query "flag:unread AND NOT flag:trashed"
	   :key ?u)
	 ,(make-mu4e-bookmark
	   :name  "Today's messages"
	   :query "date:today..now"
	   :key ?t)
	 ,(make-mu4e-bookmark
	   :name  "Last 7 days"
	   :query "date:7d..now"
	   :key ?w)
	 ,(make-mu4e-bookmark
	   :name  "Hackers"
	   :query "list:hackers.openbsd.org"
	   :key ?h)
	 ,(make-mu4e-bookmark
	   :name  "Tech"
	   :query "list:tech.openbsd.org"
	   :key ?t)
	 ,(make-mu4e-bookmark
	   :name  "Ports"
	   :query "list:ports.openbsd.org"
	   :key ?p)))

(setq user-mail-address     "aaron@bolddaemon.com"
      user-full-name        "Aaron Bieber"
      mu4e-get-mail-command "mbsync fastmail")

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-user "qbit@fastmail.com"
      smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-default-smtp-server "smtp.fastmail.com"
      smtpmail-stream-type 'ssl)

(provide 'email)
