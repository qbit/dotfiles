(require 'emms-setup)
(require 'emms-player-mpd)
(require 'emms-mode-line)
(require 'emms-playing-time)

(emms-standard)
(emms-default-players)

(add-to-list 'emms-player-list 'emms-player-mpd)
(emms-mode-line 1)
(emms-playing-time 1)

(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")

(emms-player-mpd-connect)

(provide 'music)




