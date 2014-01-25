;;; init --- emacs configuration start file
;;; Commentary:
;;; Code:

(setq debug-on-error t)
;;; Path Variables
(defvar user-home-dir "~")
(defvar user-conf-dir (concat user-home-dir "/.emacs.d"))
(defvar user-info-file (concat user-home-dir "/.userinfo.el"))
(defvar user-local-file (concat user-home-dir "/.userlocal.el"))
(setq custom-file (concat user-home-dir "/.userlocal.el"))

(require 'nby (concat user-conf-dir "/lisp/nby.el"))

(load user-info-file nil nil t)

;; Add common search path
(nby/add-to-load-path "lisp/site-lisp")

;; Load all configurations in enabled directory
(let ((dir (format "%s/conf-enabled" user-conf-dir)))
  (dolist (el (directory-files dir))
    (when (nby/string-endswith el ".el")
      (load (nby/path-join dir el) nil nil t))))

(provide 'init)
;;; init.el ends here
