(defun set-location-by-ip ()
  "Try to get Longitude and Latitude from GeoIP"
  (let ((url-request-method "GET"))
    (url-retrieve "http://geo.qbit.io/"
		  (lambda (status) ()
		    (set-location (split-string (buffer-substring (point-max) 81) ", "))))))

(defun set-location (str)
  "Set Longitude and Latitude for calendar"
  (setq calendar-latitude (string-to-number (car str)))
  (setq calendar-longitude (string-to-number (car (cdr str)))))

(set-location-by-ip)

(provide 'location)
