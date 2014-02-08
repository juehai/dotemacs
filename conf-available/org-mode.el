;;; org-mode --- Configuration for org-mode
;;; Commentary:
;;; Code:


;; require features
(nby/with-feature
 'org
 (dolist (feature '(org-habit ox-man ox-taskjuggler ox-beamer org-mime))
   (unless (require feature nil t)
     (nby/log-warn "%s cannot be loaded" (symbol-name feature)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org-mode Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-<escape>") 'org-clock-goto)
(global-set-key (kbd "C-M-r") 'org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generic settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto mode
(nby/with-feature
 'org
 ;; enable grammar check when using org-mode
 (add-hook 'org-mode-hook
           #'(lambda ()
	       (flyspell-mode t)))
 (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)))

(custom-set-variables
 '(org-babel-temporary-directory
   (nby/path-join temporary-file-directory "babel"))
 '(org-use-fast-todo-selection t)
 '(org-hide-leading-stars t)

 ;; publish timestamp persistent directory
 '(org-publish-timestamp-directory
   (nby/build-relative-path "db/org-timestamp"))

 ;; Targets complete directly with IDO
 '(org-completion-use-ido t)
 '(org-outline-path-complete-in-steps nil)

 ;; just do babel evaluation, do ask anything
 '(org-confirm-babel-evaluate nil)

 ;; use css for style
 '(org-export-htmlize-output-type 'css)

 ;; enable identation
 '(org-startup-indented t)

 ;; dont mark done when subtrees have undone task
 '(org-enforce-todo-dependencies t)

 '(org-clone-delete-id t)

  ;; candidates that can be refiled
 '(org-refile-targets '((nil :maxlevel . 2)
                        (org-agenda-files :maxlevel . 2)))

  ;; do not add a blank line before new entry
 '(org-blank-before-new-entry nil)

  ;; Press return to follow link
 '(org-return-follows-link t)

 ;; Targets start with the file name - allows creating level 1 tasks
 '(org-refile-use-outline-path 'file)

 ;; Allow refile to create parent tasks with confirmation
 '(org-refile-allow-creating-parent-nodes 'confirm)

 ;; Allow setting single tags without the menu
 '(org-fast-tag-selection-single-key 'expert)

 ;; For tag searches ignore tasks with scheduled and deadline dates
 '(org-agenda-tags-todo-honor-ignore-options t)

 '(org-log-done 'time)) ;; add timestamp to done task

(defun bh/clock-in-to-started (kw)
  "Switch task from TODO or NEXT to STARTED when clocking in.
Skips capture tasks."
  (if (and (member (org-get-todo-state) (list "TODO" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      "STARTED"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TeX exporting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use xelatex instead of latex
(custom-set-variables
 '(org-latex-to-pdf-process
   '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Babel
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'org
 (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((dot . t)
    (ditaa . t)
    (plantuml . t)
    (R . t)
    (latex . t)
    (gnuplot . t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mime
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'org
 (add-hook 'message-mode-hook
           (lambda ()
             (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

 (add-hook 'org-mode-hook
           (lambda ()
             (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Project
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/org-project-file
  (nby/build-relative-path "org-projects.el"))

(when (file-exists-p nby/org-project-file)
    (nby/load nby/org-project-file))

(defun org-reload-project ()
  "Reload 'org-mode' project."
  (interactive)
  (nby/load nby/org-project-file))

;;; org-mode ends here
