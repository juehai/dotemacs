;;; lang-ruby --- Functions for coding ruby
;;; Commentary:
;;; Code:

(defun compile-ruby ()
  "Run buffer as ruby program."
  (interactive)
  (save-buffer t)
  (compile (concat "ruby '" (buffer-file-name) "'")))

(nby/with-feature
 'ruby-mode

 (nby/with-feature 'inf-ruby)
 (nby/with-feature 'ruby-compilation)

 (add-hook
  'ruby-mode-hook
  #'(lambda ()
      (local-set-key"\C-c\C-c" 'compile-ruby)
      (nby/local-set-variables
       '(ruby-deep-arglist        t)
       '(ruby-deep-indent-paren nil))))

 ;; auto-mode for rhtml
 (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
 (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))

 ;; auto-mode for ruby
 (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
 (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
 (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
 (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
 (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
 (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode)))

;;; lang-ruby.el ends here
