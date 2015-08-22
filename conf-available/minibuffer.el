;;; minibuffer --- Configuration of common editing behaviors
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ido:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido is a builtin feature. no need to use nby/require
(nby/with-feature
 'ido
 (setq ido-save-directory-list-file
       (nby/build-relative-path "db/ido-last.el"))
 (custom-set-variables
  '(ido-max-directory-size 100000))
 (ido-mode t))

(nby/with-feature
 'ido-vertical-mode
 (ido-vertical-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tramp:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tramp is a builtin feature. no need to use nby/require
(require 'tramp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Session: Remember things like previous opened files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(nby/with-feature
; 'session
; (setq
;  ;; resolv conflicts with helm
;  ;; https://github.com/emacs-helm/helm/issues/94
;  session-save-print-spec '(t nil 40000)
;  session-save-file (nby/build-relative-path "db" "session.el"))
; (add-hook 'after-init-hook 'session-initialize)
; (nby/with-feature
;  'org
;  (add-to-list 'session-globals-exclude 'org-mark-ring)))

;;; minibuffer.el ends here
