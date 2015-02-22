;;; lang-python -- python-mode configuration
;;; Commentary:
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pylookup: online documentation searching
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/python-indentation-size 4
  "Number of spaces for indentation in 'python-mode'.")

(nby/with-feature
 'pylookup
 (custom-set-variables
  `(pylookup-db-file ,(nby/build-relative-path "/db/pylookup.db"))
  `(pylookup-program ,(nby/build-relative-path "/lisp/vendor/pylookup/pylookup.py")))
 (global-set-key "\C-chp" 'pylookup-lookup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jedi-Helpers: Enable virtualenv based on project root directory
;; Copied from: https://github.com/tkf/emacs-jedi/issues/123
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nby/python-project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun nby/python-project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (if buffer-name
      (let ((root-dir (nby/python-project-directory buffer-name)))
        (if root-dir
            (file-name-nondirectory
             (directory-file-name root-dir))
          nil))
    nil))

(defun nby/jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (let ((project-name (nby/python-project-name buffer-file-name)))
    (when project-name
      (progn
        (message (concat
                  "use virtualenv: "
                  project-name))
        (venv-workon project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python-mode: main python mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(nby/with-feature 'pymacs)
(nby/with-feature 'virtualenvwrapper)

(nby/with-feature
 'python-mode

 ;; unload python as it conflicts with python-mode
 (when (featurep 'python) (unload-feature 'python t))

 (if (require 'nby-coding nil t)
   (nby/whitespace-detection-mode 'python-mode :tab t)
   (nby/log-warn "whitespace detection failed to start in python-mode"))

 ;; (nby/with-feature
 ;;  'auto-complete
 ;;  (unless (require 'auto-complete-pycomplete nil t)
 ;;    (nby/log-warn "auto-complete-pycomplete failed to load")))

 ;; do not start python shell at start
 (custom-set-variables
  '(py-start-run-py-shell nil))

 ;; auto mode list
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'auto-mode-alist '("\\.rpy\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode))

 ;; python-mode hook
 (add-hook
  'python-mode-hook
  #'(lambda ()
      (nby/local-set-variables
       '(tab-width            nby/python-indentation-size)
       '(python-indent        nby/python-indentation-size)
       '(py-indent-offset     nby/python-indentation-size)
       '(indent-tabs-mode     nil))
      ;; FIXME: smart indentation may cause python-mode hang
      (py-smart-indentation-on)
      (add-to-list 'ac-sources 'ac-source-yasnippet)
;;      (nby/with-feature
;;       'auto-complete
;;       (add-to-list 'ac-sources 'ac-source-pycomplete))
      (local-set-key (kbd "C-c C-c") 'eval-buffer-as-python))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jedi: Introspection and auto complete
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'python-mode
 (nby/with-feature
  'jedi
  (add-hook 'python-mode-hook 'nby/jedi-setup-venv)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys nil
	jedi:complete-on-dot t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Other functions for coding python
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-buffer-as-python ()
  "Run buffer content as python program."
  (interactive)
  (save-buffer t)
  (venv-with-virtualenv-shell-command
   venv-current-name
   (concat "python" " " (buffer-file-name))))

;;; lang-python.el ends here
