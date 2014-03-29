;;; coding -- Configuration for coding and languages
;;; Commentary:
;;;
;;;   Configuration for coding and languages that are not big enough to
;;;   be hosted in an individual file.
;;;
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compilation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables '(compilation-scroll-output t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code Tidy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete trailing whitespace, hope it's safe
(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))

;; indent & unindent
(global-set-key "\C-c>" 'nby/indent-region)
(global-set-key "\C-c<" 'nby/unindent-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YAML
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'yaml-mode
 (autoload 'yaml-mode "yaml-mode" nil t)

 (if (require 'nby-coding nil t)
   (nby/whitespace-detection-mode 'yaml-mode :tab t)
   (nby/log-warn "whitespace detection failed to start in yaml-mode"))

 ;; saltstack configurations
 (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arduino
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'arduino-mode
(add-hook
 'arduino-mode-hook
 #'(lambda ()
     (local-set-key "\C-c\C-c" #'(lambda () (compile "scons upload"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Haskell mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature 'haskell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VCS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'magit
 (global-set-key "\C-cg" 'magit-status))

(nby/with-feature
 'git-gutter
 (global-git-gutter-mode +1)
 (setq git-gutter:modified-sign "  " ;; two space
       git-gutter:added-sign "++"    ;; multiple character is OK
       git-gutter:deleted-sign "--")
 (color-theme-tomorrow--with-colors
  'night
  (set-face-foreground 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:added    "green")
  (set-face-foreground 'git-gutter:deleted  "red")))

(nby/with-feature
 'git-gutter-fringe
 (color-theme-tomorrow--with-colors
  'night
  (set-face-foreground 'git-gutter-fr:modified yellow)
  (set-face-foreground 'git-gutter-fr:added    green)
  (set-face-foreground 'git-gutter-fr:deleted  red)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XCScope mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature 'xcscope)


;;; coding.el ends here
