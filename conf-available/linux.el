;;; linux --- linux specified configurations
;;; Commentary:
;;; Code:


(when (eq system-type 'gnu/linux)
  ;; double bold font looks ugly on linux, so disable bold for all fonts.
  (mapc #'(lambda (x) (set-face-attribute x nil :weight 'normal)) (face-list)))

;;; linux.el ends here
