(require 'erc-goodies)
(require 'erc-truncate)

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#devious" "#cobug" "#metabug" "#openbsd"))
      erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-rename-buffers t
      erc-nick "not_qbit"
      erc-full-name "Aaron"
      erc-prompt-for-password nil
      erc-compute-server "irc.freenode.net"
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-join-buffer 'bury
      erc-fill-column 200
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 15
      erc-max-buffer-size 10000)

(defun erc-cmd-UNAME (&rest ignore)
  "Display 'uname -a' of the system"
  (let ((uname-output
	 (replace-regexp-in-string
	  "^ \\|[ \n]+$" ""
	  (shell-command-to-string "uname -a"))))
    (erc-send-message uname-output)))

(defun erc-cmd-NODE (&rest ignore)
  "Show the version of NodeJS avaliable on the system"
  (let ((node-output
	 (replace-regexp-in-string
	  "^ \\|[ \n]+$" ""
	  (shell-command-to-string "node --version"))))
    (erc-send-message
     (concat "node.js: "node-output))))

