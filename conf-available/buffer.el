;;; buffer --- Configurations about buffer manipulating
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Buffer switch
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-x\C-a" #'(lambda () (interactive)
                               (switch-to-buffer (other-buffer))))

(nby/with-feature 'bs (global-set-key "\C-x\C-b" 'bs-show))

(global-set-key "\C-o" 'nby/occurs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Helm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'helm

 (require 'helm-config)
 (require 'helm-elisp)
 ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
 ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
 ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
 (global-set-key (kbd "C-c h") 'helm-command-prefix)
 (global-unset-key (kbd "C-x c"))

 (when (executable-find "curl")
   (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

 (global-set-key (kbd "C-x C-f") 'helm-find-files)
 (global-set-key (kbd "M-x") 'helm-M-x)
 (global-set-key (kbd "M-y") 'helm-show-kill-ring) ; show kill ring when press alt-y
 (global-set-key (kbd "C-x b") 'helm-mini)
 (global-set-key (kbd "C-o") 'helm-occur)
 (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
 (global-set-key (kbd "C-c h g") 'helm-google-suggest)

 (nby/with-feature 'helm-projectile)

 (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
       helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
       helm-ff-file-name-history-use-recentf t)


 (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
 (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
 (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


 (helm-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Anything
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar nby/anything-c-boring-bookmark-regex "^org-\\(capture\\|refile\\)-")
;; (defvar nby/anything-c-source-bookmarks
;;   `((name . "Bookmarks")
;;     (init . (lambda () (nby/require 'bookmark)))
;;     (candidates . (lambda ()
;;                     (remove-if (lambda (x) (string-match
;;                                             nby/anything-c-boring-bookmark-regex x))
;;                                (bookmark-all-names))))
;;     (type . bookmark))
;;   "See (info \"(emacs)Bookmarks\").")

;; (defun nby/occurs ()
;;   "Run anything-occur with current word."
;;   (interactive)
;;   (let ((anything-compile-source-functions
;;          ;; rule out anything-match-plugin because the input is one regexp.
;;          (delq 'anything-compile-source--match-plugin
;;                (copy-sequence anything-compile-source-functions))))
;;     (anything :sources 'anything-c-source-occur
;;               :buffer "*Anything Occur*"
;;               :input (current-word)
;;               :history 'anything-c-grep-history)))

;; (defun nby/anything-switch-to ()
;;   "My default anything list."
;;   (interactive)
;;   (anything
;;    :prompt "Switch to: "
;;    :candidate-number-limit 9
;;    :sources
;;    '(anything-c-source-buffers+
;;      anything-c-source-bm
;;      anything-c-source-fixme
;;      nby/anything-c-source-bookmarks
;;      anything-c-source-recentf
;;      anything-c-source-buffer-not-found
;;      )))


;; (defun nby/anything-info ()
;;   "An anything list for references."
;;   (interactive)
;;   (anything
;;    :prompt "Switch to: "
;;    :candidate-number-limit 3
;;    :sources
;;    '(anything-c-source-man-pages
;;      anything-c-source-info-emacs
;;      )))

;; (nby/with-feature
;;  'anything
;;  (when (require 'anything-config nil t)
;;    (custom-set-variables
;;     '(anything-c-boring-buffer-regexp
;;       "\\(\\` \\)\\|\\*anything\\|\\*ac-mode\\| \\*Echo Area\\| \\*Minibuf\\|\\*e?shell"))
;;    (global-set-key "\C-xb" 'nby/anything-switch-to)
;;    (global-set-key "\C-ci" 'nby/anything-info)))



;;; buffer.el ends here
