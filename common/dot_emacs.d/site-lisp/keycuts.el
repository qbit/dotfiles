(global-set-key (kbd "<backtab>") 'hippie-expand)
(global-set-key "\M-g" 'magit-status)

(global-set-key (kbd "M-x") 'helm-M-x)

(require 'rect-mark)
; add some smarts to the normal kill / paste stuff
; this way it knows when we are in rectangle mode!
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-w")
  '(lambda(b e) (interactive "r")
     (if rm-mark-active
       (rm-kill-region b e) (kill-region b e))))
(global-set-key (kbd "M-w")
  '(lambda(b e) (interactive "r")
     (if rm-mark-active
       (rm-kill-ring-save b e) (kill-ring-save b e))))
(global-set-key (kbd "C-x C-x")
  '(lambda(&optional p) (interactive "p")
     (if rm-mark-active
       (rm-exchange-point-and-mark p) (exchange-point-and-mark p))))

(global-set-key (kbd "C-c C-f") 'flop-frame)
(global-set-key (kbd "C-c C-s") 'flip-frame)

(provide 'keycuts)
