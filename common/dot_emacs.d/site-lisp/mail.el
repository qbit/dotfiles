(setq
 user-mail-address "aaron@bolddaemon.com"
 user-full-name "Aaron Bieber")

(require 'mu4e)
(require 'mu4e-contrib)
(require 'org-contacts)

(setq mu4e-org-contacts-file  (expand-file-name "~/org/contacts.org"))
(add-to-list 'mu4e-headers-actions
	     '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
	     '("org-contact-add" . mu4e-action-add-org-contact) t)

(setq mu4e-html2text-command 'mu4e-shr2text)
(setq mu4e-maildir (expand-file-name "~/Mail"))
(setq mu4e-get-mail-command "mbsync -q fastmail")
(setq mu4e-headers-include-related t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-use-fancy-chars t)
(setq mu4e-compose-signature-auto-include nil)

(add-hook 'message-mode-hook 'turn-on-flyspell 'append)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

(setq mu4e-contexts
      `( ,(make-mu4e-context
	   :name "fastmail"
	   :enter-func (lambda () (mu4e-message "Switch to FastMail"))
	   ;; leave-func not defined
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "aaron@bolddaemon.com")))
	   :vars '(  ( user-mail-address	     . "aaron@bolddaemon.com"  )
		     ( user-full-name	    . "Aaron Bieber" )))

	 ,(make-mu4e-context
	   :name "gmail"
	   :enter-func (lambda () (mu4e-message "Switch GMail"))
	   ;; leave-fun not defined
	   :match-func (lambda (msg)
			 (when msg
			   (mu4e-message-contact-field-matches msg
							       :to "deftly@gmail.com")))
	   :vars '(  ( user-mail-address	     . "deftly@gmail.com" )
		     ( user-full-name	    . "Aaron Bieber" )))))

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
;; '(setq mu4e-compose-context-policy nil)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(provide 'mail)
