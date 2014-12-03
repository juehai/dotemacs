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
 (add-hook 'sql-interactive-mode-hook
           (lambda ()
             (toggle-truncate-lines t)))

 (defvar sql-connection-alist "" nil)

 (defun nby/sql-databases ()
   (mapcar #'(lambda (x) (car x)) sql-connection-alist))

 (nby/with-feature
  'helm
  (defun nby/sql-connect ()
    "Connect to the input server using my-sql-servers-list"
    (interactive)
    (let ((dbname (helm-comp-read "Select SQL server: " (nby/sql-databases))))
      (sql-connect dbname dbname)))))


;;; sql.el ends here
