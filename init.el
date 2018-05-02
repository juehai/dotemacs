;;; init --- emacs configuration start file
;;; Commentary:
;;; Code:

(require 'cl)

(setq nby/startup-timestamp (float-time))
;; enable debugging when env variable set
(when (getenv "EMACS_DEBUG")
  (setq debug-on-error t
        el-get-verbose t))

(defvar user-home-dir "~")

;;; Path Variables
(defun nby/first-exist (c)
  "Find first exist directory in list C."
  (car (remove-if-not'file-exists-p c)))

(defun nby/prepend-user-home-dir (c)
  "Prepend user home directory at each elem of C."
  (mapcar (lambda (x) (concat user-home-dir "/" x)) c))

(defvar user-conf-dir
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".emacs.d" "_emacs.d"))))
(defvar user-info-file
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".userinfo.el" "_userinfo.el"))))
(defvar user-custom-file
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".usercustom.el" "_usercustom.el"))))
(setq custom-file user-custom-file)
(defvar user-local-file
  (nby/first-exist
   (nby/prepend-user-home-dir
    '(".userlocal.el" "_userlocal.el"))))

;;; Set ELPA sources before loading packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'nby (concat user-conf-dir "/lisp/nby.el"))
(require 'nby-tools (concat user-conf-dir "/lisp/nby-tools.el"))

;; load user info before everything
(nby/load user-info-file)

;; initialize packaging system
(if (eq nby/packaging-system 'elpa)
  (progn
    (package-initialize)
    (unless (file-exists-p (nby/path-join user-conf-dir "elpa"))
      (package-refresh-contents))))

;; Add common search path
(nby/add-to-load-path "lisp/site-lisp")

;; Load all configurations in enabled directory
(let ((dir (format "%s/conf-enabled" user-conf-dir)))
  (dolist (el (directory-files dir))
    (when (nby/string-endswith el ".el")
      (load (nby/path-join dir el) nil nil t))))

;; load user custom settings after everything
(nby/load user-custom-file)
(nby/load user-local-file)
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(backup-by-copying t)
 '(backup-directory-alist (\` ((".*" \, nby/emacs-temporary-file-directory))))
 '(blink-cursor-mode nil)
 '(bookmark-default-file (nby/build-relative-path "db/bookmarks.el"))
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(compilation-scroll-output t)
 '(css-indent-offset 2)
 '(current-language-environment "UTF-8")
 '(delete-old-versions t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(enable-recursive-minibuffers t)
 '(fill-column 78)
 '(frame-title-format "emacs@%b" t)
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point (quote symbol))
 '(ido-max-directory-size 100000)
 '(inhibit-startup-screen t)
 '(kept-new-versions 10)
 '(kept-old-versions 5)
 '(kill-ring-max 512)
 '(lambda-symbol "λ" t)
 '(left-fringe-width 11 t)
 '(line-spacing 2)
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(mail-user-agent (quote mu4e-user-agent))
 '(message-cite-function (quote mu-cite-original))
 '(message-kill-buffer-on-exit t)
 '(mouse-wheel-follow-mouse t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1))))
 '(mouse-yank-at-point t)
 '(mu-cite-prefix-format (quote (" > ")))
 '(mu-cite-top-format (quote ("On " date ", " from " wrote:

")))
 '(mu4e-confirm-quit nil)
 '(mu4e-drafts-folder "/local/drafts")
 '(mu4e-html2text-command "html2text -utf8 -width 72")
 '(mu4e-maildir "~/Mails")
 '(mu4e-sent-folder "/local/sent")
 '(mu4e-sent-messages-behavior (quote delete))
 '(mu4e-trash-folder "/local/trash")
 '(mu4e-view-show-images t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-babel-temporary-directory (nby/path-join temporary-file-directory "babel") t)
 '(org-blank-before-new-entry nil)
 '(org-clone-delete-id t)
 '(org-completion-use-ido t)
 '(org-confirm-babel-evaluate nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-export-htmlize-output-type (quote css))
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-hide-leading-stars t)
 '(org-latex-to-pdf-process
   (quote
    ("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f")))
 '(org-log-done (quote time))
 '(org-outline-path-complete-in-steps nil)
 '(org-publish-timestamp-directory (nby/build-relative-path "db/org-timestamp"))
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 2)
     (org-agenda-files :maxlevel . 2))))
 '(org-refile-use-outline-path (quote file))
 '(org-return-follows-link t)
 '(org-startup-indented t)
 '(org-use-fast-todo-selection t)
 '(package-selected-packages
   (quote
    (jdee zoom tern flycheck-pyflakes auto-virtualenvwrapper flycheck-pycheckers tide yasnippet yaml-mode xwidgete xcscope wrap-region window-numbering web-mode w3m vline virtualenvwrapper utop typescript-mode tuareg toml-mode symbol-overlay sublime-themes subatomic256-theme subatomic-theme smart-mode-line skewer-mode shm rustfmt ruby-compilation rotate rainbow-mode racer powerline popwin plantuml-mode php+-mode nlinum nginx-mode multiple-cursors mu-cite markdown-mode+ magit lua-mode latex-preview-pane json-mode jinja2-mode ido-vertical-mode hindent highlight-parentheses helm-projectile helm-bm helm-ag helm-ack haskell-mode google-c-style go-mode git-gutter-fringe+ flycheck-rust flycheck-pos-tip fish-mode expand-region edit-server dockerfile-mode dired+ diffview dashboard company-web company-racer company-jedi column-enforce-mode color-theme coffee-mode cider boxquote avy atom-dark-theme arduino-mode all-the-icons)))
 '(py-start-run-py-shell nil)
 '(pylookup-db-file "/home/jianingy/.emacs.d/db/pylookup.db" t)
 '(pylookup-program "/home/jianingy/.emacs.d/lisp/vendor/pylookup/pylookup.py" t)
 '(recentf-save-file (nby/build-relative-path "db/recentf.el"))
 '(right-fringe-width 11 t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-down-aggressively 0.01)
 '(scroll-margin 3)
 '(scroll-step 1)
 '(scroll-up-aggressively 0.01)
 '(show-paren-mode t)
 '(smtpmail-queue-dir "/home/jianingy/Maildir/local/queue")
 '(sql-postgres-options (quote ("-P" "pager=off" "-w")))
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(version-control t)
 '(visible-bell t)
 '(zoom-ignore-predicates
   (quote
    ((lambda nil
       (>
        (count-lines
         (point-min)
         (point-max))
        20)))))
 '(zoom-ignored-buffer-name-regexps (quote ("^*")))
 '(zoom-ignored-buffer-names (quote ("test.el")))
 '(zoom-ignored-major-modes (quote (dired-mode markdown-mode)))
 '(zoom-mode t nil (zoom))
 '(zoom-size (quote size-callback)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
