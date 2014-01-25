;;; w3c -- Configuration for web coding including html/css
;;; Commentary:
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NXHTML
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'nxhtml
 (add-hook
  'nxml-mode-hook
  #'(lambda ()
      (local-set-key "\r" 'newline-and-indent)
      (nby/local-set-variables
       '(nxml-child-indent 4)
       '(tab-width         4)
       '(standard-indent   4)
       '(indent-tabs-mode nil)))))

(custom-set-variables '(css-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSS Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
'rainbow-mode
(add-hook 'css-mode-hook 'rainbow-mode))

;;; w3c.el ends here
