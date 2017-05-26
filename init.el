;;; init --- emacs configuration start file
;;; Commentary:
;;; Code:

(require 'cl)

(setq nby/startup-timestamp (float-time))
;; enable debugging when env variable set
(when (getenv "EMACS_DEBUG")
  (setq debug-on-error t
        el-get-verbose t))

(defvar user-home-dir "~")

;;; Path Variables
(defun nby/first-exist (c)
  "Find first exist directory in list C."
  (car (remove-if-not'file-exists-p c)))

(defun nby/prepend-user-home-dir (c)
  "Prepend user home directory at each elem of C."
  (mapcar (lambda (x) (concat user-home-dir "/" x)) c))

(defvar user-conf-dir
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".emacs.d" "_emacs.d"))))
(defvar user-info-file
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".userinfo.el" "_userinfo.el"))))
(defvar user-custom-file
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".usercustom.el" "_usercustom.el"))))
(setq custom-file user-custom-file)
(defvar user-local-file
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".userlocal.el" "_userlocal.el"))))

;;; Set ELPA sources before loading packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'nby (concat user-conf-dir "/lisp/nby.el"))
(require 'nby-tools (concat user-conf-dir "/lisp/nby-tools.el"))

;; load user info before everything
(nby/load user-info-file)

;; initialize packaging system
(if (eq nby/packaging-system 'elpa)
  (progn
    (package-initialize)
    (unless (file-exists-p (nby/path-join user-conf-dir "elpa"))
      (package-refresh-contents))))

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
(provide 'init)
;;; init.el ends here
