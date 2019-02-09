;;; orgie --- emacs specific orgieurations
;;; Commentary:
;;; Code:

(require 'org)
(require 'org-habit)
(require 'org-protocol)

(setq x-selection-timeout 5)
(setq x-select-enable-clipboard-manager nil)

(setq org-directory "~/org")
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-journal-dir "~/org/journal/")

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))

;;; local
(setq org-mobile-directory "~/org/mobile")
;;; webdav
;;(setq org-mobile-directory "/davs:user@server:/path")

(setq org-mobile-inbox-for-pull "~/.org/mobileorg.org")

(setq org-log-done 'time)

(setq org-capture-templates
      `(("t" "TODO"
	 entry (file+headline "~/org/todo.org" "TODOs")
	 ,(concat
	  "* TODO %?\n"
	  ":PROPERTIES:\n"
	  ":LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)\n"
	  ":END:\n") :prepend t)
	("f" "TODO with File"
	 entry (file+headline "~/org/todo.org" "TODOs")
	 ,(concat
	  "* TODO %?\n"
	  ":PROPERTIES:\n"
	  ":LOGGING: TODO(!) WAIT(!) DONE(!) CANCELED(!)\n"
	  ":END:\n"
	  "%i\n  %a") :prepend t)
	("b" "Bug"
	 entry (file+olp+datetree "~/org/bugs.org" "Bugs")
	 "* BUG %?\nEntered on %U\n  :PROPERTIES:\n  :FILE: %a\n  :END:\n" :prepend t)
	("p" "Protocol"
	 entry (file+headline "~/org/links.org" "Links")
         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file+headline "~/org/links.org" "Links")
         "* %? %:link\n%:description\n")
        ("j" "Journal"
	 entry (file+olp+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a" :prepend t)))

(setq org-agenda-custom-commands
      '(("d" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
	("o" "OpenBSD"
	 ((agenda ""))
	 ((org-adenda-show-log t)
	  (org-agenda-ndays 7)
	  (org-agenda-log-mode-items '(state))
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":OpenBSD:"))))
	("p" "OpenBSD Ports"
	 ((agenda ""))
	 ((org-adenda-show-log t)
	  (org-agenda-ndays 7)
	  (org-agenda-log-mode-items '(state))
	  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":ports:"))))
	))

(provide 'orgie)

;;; orgie.el ends here
