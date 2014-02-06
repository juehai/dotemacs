;;; browser --- Configuration for WWW Browser
;;; Commentary:
;;; Code:

(nby/require 'w3m :package-name 'emacs-w3m)
(nby/with-feature
 'w3m
 (custom-set-variables
  '(w3m-default-display-inline-images t)
  '(browse-url-browser-function 'w3m-browse-url)
  '(w3m-command "w3m"))

 ;; RSS Browser with W3M. newsticker is an emacs builtin feature
 (require 'newsticker)

 (setq newsticker-html-renderer 'w3m-region)
 (setq newsticker-retrieval-interval 600)
 (setq newsticker-url-list-defaults nil)
 (global-set-key (kbd "C-c r") 'newsticker-treeview)

 (nby/with-feature
  'tomorrow-night-theme
  (color-theme-tomorrow--with-colors
   'night
   (custom-set-faces
    `(w3m-anchor ((t (:foreground ,blue))))
    `(w3m-current-anchor ((t (:foreground ,blue))))))))

;;; browser.el ends here
