;;; browser --- Configuration for WWW Browser
;;; Commentary:
;;; Code:

(nby/require 'w3m :package-name 'emacs-w3m)
(nby/with-feature
 'w3m
 (setq w3m-default-display-inline-images t
       ;; browse-url-browser-function 'w3m-browse-url
       w3m-command "w3m")

 ;; RSS Browser with W3M. newsticker is an emacs builtin feature
 (require 'newsticker)

 (setq newsticker-html-renderer 'w3m-region)
 (setq newsticker-retrieval-interval 600)
 (setq newsticker-url-list-defaults nil)
 (global-set-key (kbd "C-c r") 'newsticker-treeview)

 (nby/with-current-theme-colors
  (set-face-attribute 'w3m-anchor nil :foreground blue)
  (set-face-attribute 'w3m-current-anchor nil :foreground blue)))
;;; browser.el ends here
