(global-set-key (kbd "<backtab>") 'hippie-expand)
(global-set-key "\M-g" 'magit-status)

;;(global-set-key (kbd "M-x")                          'undefined)
;;(global-set-key (kbd "M-x")                          'helm-M-x)
;;(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
;;(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

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
