;;; motd --- display a message of today
;;; Commentary:
;;; Code:


(defun nby/get-motd ()
  "Get all motd text and do some decoration."
  (let* ((motds (directory-files (nby/build-relative-path "/motd") t ".txt"))
         (selected (random (length motds)))
         (motd (nth selected motds)))
    (with-temp-buffer
      (insert-file-contents motd)
      (if (< (count-lines (point-min) (point-max)) 2)
            (save-excursion
              (goto-char (point-max))
              (insert "\n\n")))
      (lisp-interaction-mode)
      (kill-line)
      (next-line)
      (save-excursion
        (boxquote-region (point) (point-max)))
      (move-end-of-line nil)
      (insert " Tip of the day: ")
      (yank)
      (comment-region (point-min) (point-max))
      (concat (buffer-string) "\n"))))


(defun nby/show-motd ()
  "Display a random motd message."
  (interactive)
  (message (nby/get-motd)))

(setq initial-scratch-message (nby/get-motd))

;;; motd.el ends here
