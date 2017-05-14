;;; editing --- Configuration of common editing behaviors
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Editor settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/emacs-temporary-file-directory
  (nby/path-join temporary-file-directory "emacs")
  "Temporary directory for emacs files")

(unless (file-exists-p nby/emacs-temporary-file-directory)
  (make-directory nby/emacs-temporary-file-directory))

(nby/log-info "backup/autosave directory is " nby/emacs-temporary-file-directory)

(custom-set-variables
 '(backup-directory-alist `((".*" . ,nby/emacs-temporary-file-directory)))
 '(recentf-save-file (nby/build-relative-path "db/recentf.el"))
 '(tab-width           8)               ;; default tab width
 '(fill-column        78)               ;; default column width
 '(mouse-yank-at-point t)               ;; dont insert at mouse point
 '(kill-ring-max     512)               ;; size of killing ring
 '(enable-recursive-minibuffers t)
 '(frame-title-format  "emacs@%b"))     ;; display buffer name at title bar

;; move mouse pointer away while cursor is near
(mouse-avoidance-mode 'animate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing shortcuts bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-g"            'goto-line)
(global-set-key "\C-cf"           'grep-find)
(global-set-key (kbd "C-c SPC")   'set-mark-command)
(global-set-key "\C-c\C-x\C-c"    'comment-region)
(global-set-key "\C-c\C-x\C-d"    'uncomment-region)
(global-set-key (kbd "C-x F")     'nby/find-file-as-root)
(global-set-key "\M-p"            'backward-sexp)
(global-set-key "\M-n"            'forward-sexp)
;(global-set-key "\C-cs"           'nby/insert-separator)
(nby/with-feature
 'message
 (global-set-key "\C-c\M-m"        'message-mark-inserted-region))
(nby/with-feature
 'boxquote
 (global-set-key "\C-c\M-b"        'boxquote-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; anwser y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; window numbering mode enables switch window using shortcuts: meta + num
(nby/with-feature
 'window-numbering
 (window-numbering-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auto wrap
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(backup-by-copying t)                     ;; don't clobber symlinks
 '(delete-old-versions t)
 '(kept-new-versions 10)
 '(kept-old-versions 5)
 '(version-control t))                      ;; use versioned backups


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enable prohibitive functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'narrow-to-region 'disabled nil)  ;; narrow-mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Auto wrap
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'dash
 (nby/with-feature
  'wrap-region
  (wrap-region-global-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Editing Server
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use edit-server from el-get, builtin edit-server cannot raise when called
(nby/with-feature
 'edit-server
 (edit-server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nby/find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      ;; use "/sudo::" to prevent asking ssh passphrase
      (setq file (concat "/sudo::" file)))
    (find-file file)))


(defun nby/insert-separator ()
  "Insert a line seperator."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (open-line 1)
    (let ((cur (point)))
      (insert "--8<-----------------------separator------------------>8---")
      (comment-region cur (point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple Cursors Mode: Edit multiple line at once
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'multiple-cursors
 (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
 (global-set-key (kbd "C->") 'mc/mark-next-like-this)
 (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
 (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Override default buffer-name unique rules
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YASnippet
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nby/with-feature 'yasnippet (yas-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; expand-region
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'expand-region
 (global-set-key (kbd "C--") 'er/contract-region)
 (global-set-key (kbd "C-=") 'er/expand-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; avy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nby/with-feature
 'avy
 (avy-setup-default)
 (global-set-key (kbd "C-;") 'avy-goto-char-timer)
 (global-set-key (kbd "C-'") 'avy-goto-char))


;;; editing.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; enable symbol overlay
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'symbol-overlay
 (global-set-key (kbd "M-i") 'symbol-overlay-put))
