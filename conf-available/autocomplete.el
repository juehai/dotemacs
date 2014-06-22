;;; autocomplete --- Configuration for auto complete features
;;; Commentary:
;;;   1. configuration for auto-complete
;;;   2. configuration for yasnippet
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Yasnippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'yasnippet
 (add-to-list 'yas-snippet-dirs (nby/build-relative-path "snippets"))
 (yas-global-mode t))
(nby/with-feature 'auto-complete-yasnippet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auto-complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'auto-complete
 (custom-set-variables
  '(ac-comphist-file (nby/build-relative-path "db/ac-comphist.el")))
 (when (require 'auto-complete-config nil t)
   (ac-config-default)
   (add-to-list 'ac-sources 'ac-source-filename)
   (global-auto-complete-mode t)
;   (define-key ac-complete-mode-map "\t" 'ac-expand)
   (define-key ac-complete-mode-map "\C-n" 'ac-next)
   (define-key ac-complete-mode-map "\C-p" 'ac-previous)
   (define-key ac-complete-mode-map "\t" 'ac-complete)
   (define-key ac-complete-mode-map "\r" nil)))





;;; autocomplete ends here
