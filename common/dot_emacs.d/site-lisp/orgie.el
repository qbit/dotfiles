;;; orgie --- emacs specific orgieurations
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-habit)

(setq org-directory "~/org")
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-journal-dir "~/org/journal/")
(setq org-mobile-directory "~/org/mobile")
(setq org-log-done 'time)

(setq org-capture-templates
      `(("t" "Todo"
	 entry (file+headline "~/org/todo.org" "TODOs")
	 ,(concat
	  "* TODO %?\n"
	  "SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"
	  ":PROPERTIES:\n"
	  ":FILE: %a \n"
	  ":LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)\n"
	  ":END:\n") :prepend t)
	("b" "Bug"
	 entry (file+olp+datetree "~/org/bugs.org" "Bugs")
	 "* BUG %?\nEntered on %U\n  :PROPERTIES:\n  :FILE: %a\n  :END:\n" :prepend t)
        ("j" "Journal"
	 entry (file+olp+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a" :prepend t)))

(provide 'orgie)

;;; orgie.el ends here
