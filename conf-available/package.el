;;; package --- Configurations for package management system
;;; Commentary:
;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defvar nby/packaging-system 'el-get
  "Indicate current packaging system in use")

(if (eq system-type 'windows-nt)
    (progn
      (message "Using ELPA Packaging System for Windows")
      (setq nby/packaging-system 'elpa)
      (package-initialize))
  (progn
    (message "Using el-get Packaging System for Linux")
      (setq nby/packaging-system 'el-get)
    (nby/add-to-load-path "lisp/vendor/el-get")
    (when (require 'el-get nil t)

      (custom-set-variables
       '(el-get-user-package-directory (nby/build-relative-path "el-get")))
      (add-to-list 'el-get-recipe-path (nby/build-relative-path "recipes"))
      (unless (file-exists-p el-get-user-package-directory)
        (make-directory el-get-user-package-directory))
      ;; install org-mode at very beginning.
      (nby/el-get-install 'org-mode)
      (nby/log-info "el-get user package directory set to %s" el-get-user-package-directory))))

;;; package.el ends here
