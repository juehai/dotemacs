;;; browser --- Configuration for WWW Browser
;;; Commentary:
;;; Code:

(nby/with-feature
 'w3m
 (custom-set-variables
  '(w3m-default-display-inline-images t)
  '(browse-url-browser-function 'w3m-browse-url)
  '(w3m-command "w3m")))

;;; browser.el ends here
