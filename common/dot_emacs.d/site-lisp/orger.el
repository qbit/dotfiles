(require 'org)
(require 'ox-publish)
(require 'ox-html)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-log-done t)
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)

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
	))

(defun enable-vc ()
  (setq vc-handled-backends (RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)))

(defun disable-vc ()
  (setq vc-handled-backends nil))

(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "~/org/"
	 :base-extension "org"
	 :publishing-directory orger-pub-to
	 :recursive t
	 :preparation-function disable-vc
	 :completion-function: enable-vc
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
 	 :preparation-function disable-vc
	 :completion-function: enable-vc
  	 :publishing-function org-html-publish-to-html
	 :auto-preamble t
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
	 :preparation-function disable-vc
	 :completion-function: enable-vc
 	 :publishing-function org-html-publish-to-html
	 :auto-preamble t
	 )
	("test-bolddaemon" :components ("bd-test" "bd-static-test"))
	("bolddaemon" :components ("bd" "bd-static"))
	("org" :components ("org-notes" "org-static"))
	))

(provide 'orger)
