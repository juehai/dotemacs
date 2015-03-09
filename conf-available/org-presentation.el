;;; org-presentation --- org-mode presentation configuration
;;; Commentary:
;;; Code:

(nby/add-to-load-path "lisp/vendor/org-reveal")
(nby/with-feature
 'ox-reveal
; (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
 (setq org-reveal-root "/")
 (defun nby/enable-org-reveal-auto-export ()
   (interactive)
   (add-hook 'after-save-hook 'org-reveal-export-to-html nil 'make-it-local)))

;;; org-presentation ends here
