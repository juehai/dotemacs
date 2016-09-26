;;; motd --- display a message of today
;;; Commentary:
;;; Code:


(nby/with-feature
 'boxquote
 (defun nby/get-motd ()
   "Get all motd text and do some decoration."
   (let* ((motds (directory-files (nby/build-relative-path "/motd") t ".txt"))
          (selected (random (length motds)))
          (motd (nth selected motds)))
     (with-temp-buffer
       (insert-file-contents motd)
       (save-excursion
         (goto-char (point-max))
         (insert "\n"))
       (lisp-interaction-mode)
       (insert "\n")
       (kill-line)
       ; (forward-line)
       (save-excursion
         (boxquote-region (point-min) (point-max)))
       (move-end-of-line nil)
       (yank)
       (comment-region (point-min) (point-max))
       (concat (buffer-string) "\n"))))


 (defun nby/show-motd ()
   "Display a random motd message."
   (interactive)
   (message (nby/get-motd)))

 (setq initial-scratch-message (nby/get-motd))
)

;;; motd.el ends here
