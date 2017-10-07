;;; orgie --- emacs specific orgieurations
;;; Commentary:
;;; Code:

(require 'org)

(setq org-mobile-directory "~/org/MobileOrg")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a)")
	("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")
	("c" "Contacts" entry (file "~/org/contacts.org")
	       "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")))

(provide 'orgie)

;;; orgie.el ends here
