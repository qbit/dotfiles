;;; hooks --- init for hooks
;;; Commentary:
;;; Code:

(add-hook 'ruby-mode-hook 'lsp 'rubocop-mode)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	      (which-func-mode 1))))

;(add-hook 'before-save-hook (lambda ()
;			      (delete-trailing-whitespace)
;			      ))

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'hooks)
