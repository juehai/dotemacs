;;; nby-tools --- my emacs xtools
;;; Commentary:
;;; Code:

(defun nby/serial-copy-line ()
  "Copy line with current number increased by one."
  (interactive)
  (save-excursion
    (kill-ring-save (line-beginning-position)
                    (line-end-position))
    (end-of-line)
    (next-line)
    (yank)
    (newline))
  (next-line)

  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(provide 'nby-tools)
;;; nby-tools ends here
