;;; nby --- Functions help write my emacs configuration
;;; Commentary:
;;; Code:

(setq debug-on-error)

(defvar user-home-dir (getenv "HOME"))
(defvar user-conf-dir (concat user-home-dir "/.emacs.d"))

(defmacro nby/make-log (tag)
  "Make a log function outputs log with TAG as prefix."
  (let ((funcname (intern (concat "nby/log-" tag))))
    `(defun ,funcname (fmt &rest args)
       (message (apply #'format (format "[%s] %s" ,tag fmt)  args)))))


(defun nby/path-join (&rest element)
  "Join multiple path ELEMENT togehter with slash handle properly."
  (substring (apply #'concat (mapcar #'file-name-as-directory element)) 0 -1))


(defun nby/string-endswith (s ending)
  "Return True if string S ends with ENDING."
  (let ((elength (length ending))
        (slength (length s)))
    (and (<= elength slength)
         (string= (substring s (- 0 elength)) ending))))


(defun nby/add-to-load-path (path &optional noerror)
  "Add a PATH which is relative to user config directory to 'load-path'.
If NOERROR is not nil, a warning will be output indicate the path doesn't
exist."
  (let ((new-load-path (nby/path-join user-conf-dir path)))
    (if (file-exists-p new-load-path)
      (progn
        (add-to-list 'load-path new-load-path)
        (nby/log-info "Adding new load path: %s" new-load-path))
      (unless noerror
        (nby/log-warn "Path %s doesn't exist" new-load-path)))))


(defun nby/build-relative-path (&rest args)
  "Build a path by ARGS."
  (apply #'nby/path-join user-conf-dir args))


(defun nby/load (filename)
  "Load FILENAME and return nil if error occurred."
  (condition-case nil
      (load filename)
    (error (progn
             (nby/log-warn "File %s cannot be loaded" filename)
             nil))))

(defun nby/el-get-install (feature)
  "Install FEATURE by el-get and return nil if installation failed."
  (condition-case nil
      (el-get 'sync `(,feature))
    (error (progn
             (nby/log-warn "Feature %s cannot be installed" feature)
             nil))))

(defun nby/require (feature)
  "Require a FEATURE.  If not exist install it by el-get."
  (nby/add-to-load-path (nby/path-join "lisp/vendor" (symbol-name feature)) t)
  (unless (require feature nil t)
    (if (el-get-recipe-filename feature)
        (nby/el-get-install feature)
      (progn
        (nby/log-warn "No recipe for feature %s"
                      feature)
        nil))))

(defmacro nby/with-feature (feature &rest body)
  "When FEATURE is provided, execute BODY."
  (let ((name (gensym)))
  `(let ((,name ,feature))
     (nby/require ,name)
     (if (require ,name nil t)
         (progn ,@body)
       (nby/log-warn ,(concat "Feature %s cannot be found. "
                              "Some settings will be disabled")
                     ,name)))))

(defmacro nby/local-set-variables (&rest pairs)
  "Make variable buffer-local and set according to variable/value PAIRS."
  `(progn
     ,@(mapcar #'(lambda (x) `(set (make-local-variable (quote ,(car (cadr x)))) ,(cadr (cadr x)))) pairs)))



;;; build some log functions
(nby/make-log "info")
(nby/make-log "warn")
(nby/make-log "debug")

(provide 'nby)
;;; nby.el ends here
