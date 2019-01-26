;;; hooks --- init for hooks
;;; Commentary:
;;; Code:

(add-hook 'ruby-mode-hook 'lsp 'rubocop-mode)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(provide 'hooks)
