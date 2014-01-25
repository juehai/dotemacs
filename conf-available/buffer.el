;;; buffer --- Configurations about buffer manipulating
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Buffer switch
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-x\C-a" #'(lambda () (interactive)
                               (switch-to-buffer (other-buffer))))


(nby/with-feature
 'bs
 (global-set-key "\C-x\C-b" 'bs-show))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Anything
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/anything-c-boring-bookmark-regex "^org-\\(capture\\|refile\\)-")
(defvar nby/anything-c-source-bookmarks
  `((name . "Bookmarks")
    (init . (lambda () (nby/require 'bookmark)))
    (candidates . (lambda ()
                    (remove-if (lambda (x) (string-match
                                            nby/anything-c-boring-bookmark-regex x))
                               (bookmark-all-names))))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun nby/anything-switch-to ()
  "My default anything list."
  (interactive)
  (anything
   :prompt "Switch to: "
   :candidate-number-limit 9
   :sources
   '(anything-c-source-buffers+
     anything-c-source-bm
     anything-c-source-fixme
     nby/anything-c-source-bookmarks
     anything-c-source-recentf
     anything-c-source-buffer-not-found
     )))


(defun nby/anything-info ()
  "An anything list for references."
  (interactive)
  (anything
   :prompt "Switch to: "
   :candidate-number-limit 3
   :sources
   '(anything-c-source-man-pages
     anything-c-source-info-emacs
     )))

(nby/with-feature
 'anything
 (when (require 'anything-config nil t)
   (custom-set-variables
    '(anything-c-boring-buffer-regexp
      "\\(\\` \\)\\|\\*anything\\|\\*ac-mode\\| \\*Echo Area\\| \\*Minibuf\\|\\*e?shell"))
   (global-set-key "\C-xb" 'nby/anything-switch-to)
   (global-set-key "\C-ci" 'nby/anything-info)))

;;; buffer.el ends here
