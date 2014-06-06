;;; sticky-windows-plus.el --- Make windows with sticky bit always stay visible
;;;
;;; Commentary:
;;;   Inspired by sticky-windows.el
;;;
;;; Author:
;;;     Jianing Yang (jianingy.yang@gmail.com)
;;; ChangeLog:
;;;    2014-06-05 (Jianing Yang) First Commit
;;;
;;; Code:

(defun sticky-windows-plus-toggle ()
  "Toggle window-dedicated flag."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window)))))


(defadvice delete-window (around sticky-windows-plus-delete-window activate)
  "Avoids windows with sticky-bit to be deleted."
  (interactive)
  (let ((window (selected-window)))
    (if (window-dedicated-p (selected-window))
	(error (concat "Window with sticky-bit cannot be deleted."
		       "Please use `sticky-windows-plus-toggle` "
		       "to unstick it first."))
      ad-do-it)))

(defadvice delete-other-windows
  (around sticky-windows-plus-delete-other-windows activate)
  "Delete all windows except those with sticky bit."
  (interactive)
  (mapcar (lambda (window)
	    (if (not (window-dedicated-p window))
		(delete-window window)))
	  (cdr (window-list))))

(provide 'sticky-windows-plus)

;;; sticky-windows-plus.el ends here
