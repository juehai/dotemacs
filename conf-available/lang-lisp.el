;;; lang-lisp -- Configuration for Emacs LISP and Common LISP
;;; Commentary:
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:

(add-hook
 'emacs-lisp-mode-hook
 #'(lambda ()
     (local-set-key "\C-c\C-c" 'eval-buffer)
     (nby/local-set-variables '(indent-tabs-mode nil))))

(nby/with-feature
 'slime
 (slime-setup '(slime-fancy))
 (add-hook
  'slime-mode-hook
  #'(lambda ()
      (local-set-key "\C-c\C-q" 'slime-close-all-parens-in-sexp))))

;;; lang-lisp.el ends here
