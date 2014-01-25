;;; lang-c --- Functions for coding C
;;; Commentary:
;;; Code:


;; for kernel coding
(defun nby/lang-c-linux-style ()
  "Setup linux kernel coding style."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (nby/local-set-variables
   '(c-basic-offset 8)
   '(tab-width 8)
   '(indent-tabs-mode t)))

(setq auto-mode-alist
      (cons '("linux.*/.*\\.[ch]$" . nby/lang-c-linux-style) auto-mode-alist))

;; for normal c coding
(defun nby/lang-c-google-style ()
  "Setup google coding style."
  (google-set-c-style)
  (google-make-newline-indent)
  (nby/local-set-variables
   '(c-basic-offset 4)
   '(tab-width 4)
   '(indent-tabs-mode nil)))

(nby/with-feature
 'google-c-style
 (add-hook 'c-mode-hook 'nby/lang-c-google-style)
 (add-hook 'c++-mode-hook 'nby/lang-c-google-style))

;;; lang-c.el ends here
