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

;; (nby/with-feature
;;  'nxhtml
;;  (add-hook
;;   'nxml-mode-hook
;;   #'(lambda ()
;;       (local-set-key "\r" 'newline-and-indent)
;;       (nby/local-set-variables
;;        '(nxml-child-indent 4)
;;        '(tab-width         4)
;;        '(standard-indent   4)
;;        '(indent-tabs-mode nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Web Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'web-mode
 (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
 (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
 (nby/with-feature 'company-web)
 (add-hook
  'web-mode-hook
  #'(lambda ()
      (setq web-mode-markup-indent-offset 2
            web-mode-css-indent-offset 2
            web-mode-code-indent-offset 2
            web-mode-style-padding 2
            web-mode-script-padding 2
            web-mode-block-padding 0
            web-mode-comment-style 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSS Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables '(css-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

(nby/with-feature
'rainbow-mode
(add-hook 'css-mode-hook 'rainbow-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; javascript mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(nby/with-feature
 'company-tern
 '(add-to-list 'company-backends 'company-tern))

(nby/with-feature
 'tern
 (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)

(nby/with-feature
 'skewer-mode
 (add-hook 'js2-mode-hook 'skewer-mode))

 (nby/with-feature
  'js2-mode
  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;;; w3c.el ends here
