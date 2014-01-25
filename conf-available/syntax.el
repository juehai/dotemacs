;;; syntax -- Configuration for syntax checking
;;; Commentary:
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flycheck: Display syntax errors and warnings on the fly
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'flycheck
 (global-flycheck-mode t))

(global-set-key "\C-cn" 'next-error)
;;; syntax.el ends here
