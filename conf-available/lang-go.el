;;; lang-go --- Functions for coding golang
;;; Commentary:
;;; Code:

(defvar nby/golang-directory
  nil
  "User specified golang directory.")

(defun nby/find-golang ()
  "Find proper golang directory."
  (if (and nby/golang-directory
             (file-exists-p nby/golang-directory))
    nby/golang-directory
    (progn
      (dolist (dir '("/opt/go" "/usr/local/go" "/usr"))
        (let ((go-executable (nby/path-join dir "bin" "go")))
          (when (file-exists-p go-executable)
                (return (nby/path-join dir "bin"))))))))

(defun nby/eval-buffer-as-golang ()
  "Run buffer content as python program."
  (interactive)
  (save-buffer)
  (compilation-start
   (concat (nby/path-join (nby/find-golang) "go") " run " (buffer-file-name))))

(nby/with-feature 'go-autocomplete)

(nby/with-feature
 'go-mode
 (add-hook
  'go-mode-hook
  #'(lambda ()
      (nby/local-set-variables
       '(indent-tabs-mode nil)
       '(tab-width          4))
      (flycheck-mode)
      (nby/with-feature
       'flycheck
       (flycheck-set-checker-executable
        'go-gofmt
        (nby/path-join (nby/find-golang) "gofmt"))
       (flycheck-select-checker 'go-gofmt))
      (local-set-key (kbd "C-c C-c") 'nby/eval-buffer-as-golang))))

;;; lang-go.el ends here
