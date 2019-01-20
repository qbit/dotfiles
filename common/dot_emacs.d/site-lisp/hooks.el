;;; hooks --- init for hooks
;;; Commentary:
;;; Code:

(add-hook 'ruby-mode-hook 'lsp 'rubocop-mode)

(provide 'hooks)
