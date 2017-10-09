;;; functions.el --- a collections of functions I use.

;;; Copyright (C) 2017 Aaron Bieber <aaron@bolddaemon.com>

;;; Commentary:

;;; Code:

(package-initialize)

(defun install-if-missing (lst)
  "Install a list of packages if they are currently not installed"
  (mapcar (lambda (ele)
	    (let ((pkg (car ele)))
	      (unless (package-installed-p pkg nil)
		(package-install pkg))))
	  lst))

(provide 'functions)

;; functions.el ends here
