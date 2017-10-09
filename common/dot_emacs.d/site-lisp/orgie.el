;;; orgie --- emacs specific orgieurations
;;; Commentary:
;;; Code:

(require 'org)

(setq org-directory "~/new_org")
(setq org-agenda-files (file-expand-wildcards "~/new_org/*.org"))
(setq org-journal-dir "~/new_org/journal/")
(setq org-mobile-directory "~/new_org/MobileOrg")

(setq org-capture-templates
            `(("j" "Journal" entry (file+datetree ,(format-time-string "~/new_org/journal_%Y-%m.org"))
	       "* %?\n")))


(provide 'orgie)

;;; orgie.el ends here
