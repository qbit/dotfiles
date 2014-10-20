;; joinked from http://endlessparentheses.com/intelligent-browse-url.html

(defun endless/browse-url-best-browser (url &rest _)
  "Use a running browser or start the preferred one."
  (setq url (browse-url-encode-url url))
  (let ((process-environment
         (browse-url-process-environment))
        (command (endless/decide-browser)))
    (start-process (concat command " " url)
                   nil command url)))

;; Use this for all links. If you actually don't want some
;; links to be viewed externally, change this line here.
(setq browse-url-browser-function
      '(("." . endless/browse-url-best-browser)))

(defcustom endless/browser-list
  '("chrome"
    ("firefox\\|mozilla" . "firefox")
    "luakit")
  "List of browsers by order of preference.
Each element is a cons cell (REGEXP . EXEC-FILE).
If REGEXP matches the name of a currently running process and if
EXEC-FILE a valid executable, EXEC-FILE will be used to open the
given URL.

An element can also be a string, in this case, it is used as both
the REGEXP and the EXEC-FILE.

It is safe to have items referring to not-installed browsers,
they are gracefully ignored."
  :type '(repeat (choice string (cons regexp file))))

(defun endless/decide-browser ()
  "Decide best browser to use based on `endless/browser-list'."
  (let ((process-list
         (mapcar #'endless/process-name
                 (list-system-processes)))
        (browser-list endless/browser-list)
        browser out)
    ;; Find the first browser on the list that is open.
    (while (and browser-list (null out))
      (setq browser (car browser-list))
      (if (and (cl-member (car-or-self browser)
                          process-list :test 'string-match)
               (executable-find (cdr-or-self browser)))
          (setq out (cdr-or-self browser))
        (setq browser-list (cdr browser-list))))
    ;; Use the one we found, or the first one available.
    (or out (endless/first-existing-browser))))

(defun endless/first-existing-browser ()
  "Return the first installed browser in `endless/browser-list'."
  (require 'cl-lib)
  (cdr-or-self
   (car
    (cl-member-if
     (lambda (x) (executable-find (cdr-or-self x)))
     endless/browser-list))))

(defun endless/process-name (proc)
  (cdr (assoc 'comm (process-attributes proc))))

(defun car-or-self (x)
  "If X is a list, return the car. Otherwise, return X."
  (or (car-safe x) x))

(defun cdr-or-self (x)
  "If X is a list, return the cdr. Otherwise, return X."
  (or (cdr-safe x) x))

(provide 'browse)
