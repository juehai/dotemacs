;;; nby --- Functions help write my emacs configuration
;;; Commentary:
;;; Code:

(require 'cl)

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
             (nby/log-warn "Feature %s cannot be installed using el-get"
                           feature)
             nil))))


(defun nby/package-install (feature)
  "Install FEATURE by el-get and return nil if installation failed."
  (condition-case nil
      (package-install feature)
    (error (progn
             (nby/log-warn "Feature %s cannot be installed using elpa"
                           feature)
             nil))))

(defun* nby/require (feature &key (package-name nil))
  "Require a FEATURE.  If not exist install it by el-get.
If PACKAGE-NAME specified, install PACKAGE-NAME and require FEATURE."
  (let ((package (if package-name package-name feature)))
    (if (eq nby/packaging-system 'el-get)
        (progn
          (nby/log-info "finding %s from %s (el-get)" feature package)
          (nby/add-to-load-path (nby/path-join "lisp/vendor" (symbol-name feature)) t)
          (unless (require feature nil t)
            (if (el-get-recipe-filename package)
                (nby/el-get-install package)
              (progn
                (nby/log-warn "No recipe for feature %s"
                              feature)
                nil))))
      (progn
        (nby/log-info "finding %s from %s (elpa)" feature package)
        (unless (require feature nil t)
          (if (assq package package-archive-contents)
              (nby/package-install package)
            (nby/log-warn "%s not available in ELPA repos" package))))
        )))

(defmacro nby/with-feature (feature &rest body)
  "When FEATURE is provided, execute BODY."
  (let ((name (gensym))
	(package (gensym)))
    `(let ((,name ,feature)
	   (,package ,feature))
       (nby/require ,name :package-name ,package)
       (if (require ,name nil t)
	   (progn ,@body)
	 (nby/log-warn ,(concat "Feature %s cannot be found. "
				"Some settings will be disabled")
		       ,name)))))

(defmacro nby/local-set-variables (&rest pairs)
  "Make variable buffer-local and set according to variable/value PAIRS."
  `(progn
     ,@(mapcar #'(lambda (x) `(set (make-local-variable (quote ,(car (cadr x)))) ,(cadr (cadr x)))) pairs)))


(defmacro nby/with-tomorrow-theme-colors (&rest body)
  "Execute BODY With current theme colors in context."
  `(nby/with-feature 'tomorrow-night-theme
   (color-theme-tomorrow--with-colors 'night ,@body)))


(defmacro nby/with-current-theme-colors (&rest body)
  "Execute BODY With current theme colors in context."
  `(let* (
          ;; Primary colors
          (yellow                   "#E6DB74")
          (orange                   "#FD971F")
          (red                      "#F92672")
          (magenta                  "#FD5FF0")
          (violet                   "#AE81FF")
          (blue                     "#66D9EF")
          (cyan                     "#A1EFE4")
          (green                    "#A6E22E")
          (gray                     "#474747")
          ;; Darker and lighter accented colors
          (yellow-d                 "#968B26")
          (yellow-l                 "#F3EA98")
          (orange-d                 "#A45E0A")
          (orange-l                 "#FEB257")
          (red-d                    "#A20C41")
          (red-l                    "#FC5C94")
          (magenta-d                "#A41F99")
          (magenta-l                "#FE87F4")
          (violet-d                 "#562AA6")
          (violet-l                 "#C2A1FF")
          (blue-d                   "#21889B")
          (blue-l                   "#8DE6F7")
          (cyan-d                   "#349B8D")
          (cyan-l                   "#BBF7EF")
          (green-d                  "#67930F")
          (green-l                  "#C1F161")
          (gray-d                   "#333333")
          (gray-l                   "#6b6b6b")
          (background               "#F8F8F2")
          (foreground               "#272822"))
     ,@body))




;;; build some log functions
(nby/make-log "info")
(nby/make-log "warn")
(nby/make-log "debug")

(provide 'nby)
;;; nby.el ends here
