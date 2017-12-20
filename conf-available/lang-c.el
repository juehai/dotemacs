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

(nby/with-feature
 'company-irony
 (add-to-list 'company-backends 'company-irony)
 (add-hook 'c++-mode-hook (lambda ()
                            (irony-mode)
                            (irony-eldoc)))
 ;; Use irony's completion functions.
 (add-hook 'irony-mode-hook
           (lambda ()
             (define-key irony-mode-map [remap completion-at-point]
               'irony-completion-at-point-async)

             (define-key irony-mode-map [remap complete-symbol]
               'irony-completion-at-point-async)

             (irony-cdb-autosetup-compile-options))))



;;; lang-c.el ends here
