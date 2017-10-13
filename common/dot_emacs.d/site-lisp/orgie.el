;;; orgie --- emacs specific orgieurations
;;; Commentary:
;;; Code:

(require 'org)

(setq org-directory "~/new_org")
(setq org-agenda-files (file-expand-wildcards "~/new_org/*.org"))
(setq org-journal-dir "~/new_org/journal/")
(setq org-mobile-directory "~/new_org/MobileOrg")
(setq org-log-done 'time)

(setq org-capture-templates
      `(("t" "Todo"
	 entry (file+headline "~/new_org/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal"
	 entry (file+olp+datetree "~/new_org/journal-%Y-%m.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

(provide 'orgie)

;;; orgie.el ends here
