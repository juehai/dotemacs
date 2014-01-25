;;; lang-perl -- Configuration for perl language
;;; Commentary:
;;;
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:

;; use cperl-mode instead of perl-mode
(fset 'perl-mode 'cperl-mode)

;; auto mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook
 'cperl-mode-hook
 #'(lambda ()
     (nby/local-set-variables
      '(indent-tabs-mode nil)
      '(cperl-indent-level 4)
      '(compilation-scroll-output nil))
     (local-set-key "\C-m" 'newline-and-indent)
     (local-set-key (kbd "C-c C-c") 'eval-buffer-as-perl)))

(defun eval-buffer-as-perl ()
  "Run buffer content as perl program."
  (interactive)
  (save-buffer)
  (shell-command (concat "perl " (buffer-file-name))))

;;; lang-perl.el ends here
