;;; orgie --- emacs specific orgieurations
;;; Commentary:
;;; Code:

(require 'org)

(setq org-directory "~/org")
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-journal-dir "~/org/journal/")
(setq org-mobile-directory "~/org/mobile")
(setq org-log-done 'time)

(setq org-capture-templates
      `(("t" "Todo"
	 entry (file+headline "~/org/gtd.org" "TODOs")
	 "* TODO %?\n  :PROPERTIES:\n  :LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)\n  :END:\n" :prepend t)
        ("j" "Journal"
	 entry (file+olp+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a" :prepend t)))

(provide 'orgie)

;;; orgie.el ends here
