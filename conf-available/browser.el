;;; browser --- Configuration for WWW Browser
;;; Commentary:
;;; Code:

(nby/with-feature
 'w3m
 (custom-set-variables
  '(w3m-default-display-inline-images t)
  '(browse-url-browser-function 'w3m-browse-url)
  '(w3m-command "w3m"))

 (nby/with-feature
  'tomorrow-night-theme
  (color-theme-tomorrow--with-colors
   'night
   (custom-set-faces
    `(w3m-anchor ((t (:foreground ,blue))))
    `(w3m-current-anchor ((t (:foreground ,blue))))))))

;;; browser.el ends here
