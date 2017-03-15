(require 'org)
(require 'ox-publish)
(require 'ox-html)
(require 'ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0")

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
	(sequence "|" "CANCELED(c)")))

(setq org-agenda-files (directory-files "~/org/agenda" t "\\.org$"))

(setq orger-pub-to "/ssh:akb.io:/var/www/htdocs/org/")

(setq org-capture-templates
      '(
	("t" "Task"
	 entry (file+datetree "~/org/agenda/home.org")
	 "* %?")
	("b" "BoldDaemon Entry"
	 entry (file+headline "~/org/websites/bolddaemon/index.org" "Posts")
         "* %?\n %U\n %i\n"
         :empty-lines 1
	 :prepend 1)
	("m" "BoldDaemon Maker Slides"
	 entry (file+headline "~/org/websites/bd_maker/index.org" "Slides")
	 "* %?"
	 :empty-lines 1)
	))

(add-to-list 'org-capture-templates
             '("c" "Contacts" entry (file "~/org/contacts.org")
               "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:"))

(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "~/org/"
	 :base-extension "org"
	 :publishing-directory orger-pub-to
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4             ; Just the default for this project.
	 :auto-preamble t
	 )
	("org-static"
	 :base-directory "~/org/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory orger-pub-to
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("bd-static"
	 :base-directory "~/org/websites/bolddaemon/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|org"
	 :publishing-directory "/ssh:akb.io:/var/www/bolddaemon/"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("bd"
	 :base-directory "~/org/websites/bolddaemon/"
	 :base-extension "org"
	 :publishing-directory "/ssh:akb.io:/var/www/bolddaemon/"
	 :recursive t
  	 :publishing-function org-html-publish-to-html
	 :auto-preamble t
	 )
	("bd-maker"
	 ;; Path to your org files.
	 :base-directory "~/org/websites/bd_maker/"
	 :base-extension "org"

	 :publishing-directory "/ssh:akb.io:/var/www/bd_maker/"
	 :recursive t
	 :publishing-function org-reveal-publish-to-reveal
	 :html-extension "html"
	 )
	("bd-static-test"
	 :base-directory "~/org/websites/bolddaemon/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|org"
	 :publishing-directory "/ssh:akb.io:/var/www/bolddaemon/test"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
	("bd-test"
	 :base-directory "~/org/websites/bolddaemon/"
	 :base-extension "org"
	 :publishing-directory "/ssh:akb.io:/var/www/bolddaemon/test/"
	 :recursive t
 	 :publishing-function org-html-publish-to-html
	 :auto-preamble t
	 )
	("test-bolddaemon" :components ("bd-test" "bd-static-test"))
	("bolddaemon" :components ("bd" "bd-static"))
	("bd_maker" :components ("bd_maker"))
	("org" :components ("org-notes" "org-static"))
	))

(provide 'orger)
