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
 (ido-mode t)
 (custom-set-variables
  '(ido-save-directory-list-file (nby/build-relative-path "db/ido-last.el"))
  '(ido-max-directory-size 100000)))


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

(nby/with-feature
 'session
 (custom-set-variables
  '(session-save-file (nby/build-relative-path "db" "session.el")))
 (add-hook 'after-init-hook 'session-initialize)
 (nby/with-feature
  'org
  (add-to-list 'session-globals-exclude 'org-mark-ring)))

;;; minibuffer.el ends here
