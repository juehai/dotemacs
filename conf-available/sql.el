;;; sql.el --- easy accessing sql database by anything
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Select Database by Anything
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(nby/with-feature
 'sql

 (defvar sql-connection-alist "" nil)

 (defun nby/sql-databases () (mapcar #'car sql-connection-alist))

 (defun nby/sql-complete ()
   (interactive)
   (let ((completions '("select" "insert" "update" "delete")))
     (comint-dynamic-simple-complete
      (comint-word "A-Za-z_")
      completions)))

 (add-hook 'sql-interactive-mode-hook
           (lambda ()
             (local-set-key (kbd "TAB") 'nby/sql-complete)
             (toggle-truncate-lines t)))

 (nby/with-feature
  'helm

  (defun nby/sql-connect ()
    "Connect to the input server using my-sql-servers-list"
    (interactive)
    (let* ((connection (helm-comp-read "Select SQL server: " (nby/sql-databases)))
           (connection-set (assoc connection sql-connection-alist)))
      ;; patch sql.el error
      (setq sql-product (cadadr (assoc 'sql-product (cdr connection-set))))
      (sql-connect connection)))))


;;; sql.el ends here
