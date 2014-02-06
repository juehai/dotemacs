;;; init --- emacs configuration start file
;;; Commentary:
;;; Code:

(setq nby/startup-timestamp (float-time))
;; enable debugging when env variable set
(when (getenv "EMACS_DEBUG")
  (setq debug-on-error t
        el-get-verbose t))

;;; Path Variables
(defvar user-home-dir "~")
(defvar user-conf-dir (concat user-home-dir "/.emacs.d"))
(defvar user-info-file (concat user-home-dir "/.userinfo.el"))
(defvar user-custom-file (concat user-home-dir "/.usercustom.el"))
(defvar user-local-file (concat user-home-dir "/.userlocal.el"))
(setq custom-file user-custom-file)

(require 'nby (concat user-conf-dir "/lisp/nby.el"))

;; load user info before everything
(nby/load user-info-file)

;; Add common search path
(nby/add-to-load-path "lisp/site-lisp")

;; Load all configurations in enabled directory
(let ((dir (format "%s/conf-enabled" user-conf-dir)))
  (dolist (el (directory-files dir))
    (when (nby/string-endswith el ".el")
      (load (nby/path-join dir el) nil nil t))))

;; load user custom settings after everything
(nby/load user-custom-file)
(nby/load user-local-file)
(setq initial-scratch-message
      (concat initial-scratch-message
	      (format (concat
		       ";; It took %.2f seconds to start emacs.\n"
		       ";; CJK font: %s, Latin Font: %s\n"
		       "\n")
		      (- (float-time) nby/startup-timestamp)
		      nby/x-font-cjk
		      nby/x-font-latin)))
(provide 'init)
;;; init.el ends here
