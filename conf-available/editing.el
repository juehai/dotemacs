;;; editing --- Configuration of common editing behaviors
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Editor settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(backup-directory-alist `((".*" . ,temporary-file-directory)))
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
(global-set-key "\C-cg"           'grep-find)
(global-set-key "\C-c\C-x\C-c"    'comment-region)
(global-set-key "\C-c\C-x\C-d"    'uncomment-region)
(global-set-key (kbd "C-x F")     'djcb-find-file-as-root)
(global-set-key "\C-c\C-\\"       'winner-undo)
(global-set-key "\C-cs"           'nby/insert-separator)
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

(nby/log-info "backup/autosave directory is " temporary-file-directory)


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
;; Helper Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
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

;;; editing.el ends here
