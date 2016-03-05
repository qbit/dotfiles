(require 'emms-setup)
(require 'emms-player-mpd)
(require 'emms-mode-line)
(require 'emms-playing-time)
(require 'emms-volume)
(require 'emms-browser)

(emms-standard)
(emms-default-players)

(setq emms-volume-change-function 'emms-volume-mpd-change)

(add-to-list 'emms-player-list 'emms-player-mpd)
(add-to-list 'emms-info-functions 'emms-info-mpd)

(emms-mode-line 1)
(emms-playing-time 1)

;;(setq emms-source-file-default-directory (expand-file-name "~/Music/"))
(setq emms-source-file-default-directory nil)

(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")

(provide 'music)


