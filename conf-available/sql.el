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

 (defvar anything-c-source-database
       `((name . "Available databases")
         (candidates . nby/sql-databases)
         (volatile)
         (action . (lambda (name) (sql-connect name name)))
         (type . string)))

 (defun nby/sql-databases ()
   (mapcar #'(lambda (x) (car x)) sql-connection-alist))

 (defun nby/sql-connect ()
   (interactive)
   (anything
    :prompt "Connect to database: "
    :candidate-number-limit 100
    :sources '(anything-c-source-database))))

;;; sql.el ends here
