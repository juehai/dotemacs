;;; powerline --- Configuration for status line
;;; Commentary:
;;; Code:

(nby/with-feature
 'powerline

 (defface powerline-modified-active
   '((t (:underline t :slant italic :inherit mode-line)))
   "Powerline file modified face."
   :group 'powerline)

 (defface powerline-modified-inactive
   '((t (:underline t :slant italic :inherit mode-line-inactive)))
   "Powerline file modified face of inactive frame."
   :group 'powerline)

 (defface powerline-flag-on
   '((t (:inherit mode-line)))
   "Powerline flag face."
   :group 'powerline)

 (defface powerline-modified-bound-warning
   '((t (:inherit powerline-modified)))
   "Powerline warning face."
   :group 'powerline)

 (defface powerline-inactive '((t (:inherit mode-line-inactive)))
   "Powerline default inactive face."
   :group 'powerline)

 (defface powerline-clock-active `((t (:inherit mode-line)))
   "Powerline clock active face."
   :group 'powerline)

 (defface powerline-clock-inactive `((t (:inherit mode-line-inactive)))
   "Powerline clock inactive face."
   :group 'powerline)

 (defpowerline powerline-org-task
   (if (and (boundp 'org-clock-current-task)
	    org-clock-current-task)
       (replace-regexp-in-string
	"\\[\\|\\]" ""
	(org-clock-get-clock-string))
     "Clock: off"))

 (setq-default
  mode-line-format
  '("%e"
    (:eval
     (let* ((active (powerline-selected-window-active))
	    (modified (if active
			  (if (< (current-column) 80)
			      (if (buffer-modified-p)
				  'powerline-modified-active nil)
			    'powerline-modified-bound-warning)
			'powerline-modified-inactive))
	    (mode-line-face (if active 'mode-line 'mode-line-inactive))
	    (face1 (if active 'powerline-active1 'powerline-inactive1))
	    (face2 (if active 'powerline-active2 'powerline-inactive2))
	    (flag-face (if active 'powerline-flag-on 'powerline-inactive))
	    (clock-face (if active
			    (if (and (boundp 'org-clock-current-task)
				     org-clock-current-task)
				'powerline-clock-active 'powerline-clock-inactive)
			  'powerline-inactive))
	    (separator-left (intern (format "powerline-%s-%s"
					    powerline-default-separator
					    (car powerline-default-separator-dir))))
	    (separator-right (intern (format "powerline-%s-%s"
					     powerline-default-separator
					     (cdr powerline-default-separator-dir))))
	    (narrow (if (powerline-narrow) " NR " "    "))
	    (read-only (if buffer-read-only
			   (powerline-raw " RO " flag-face)
			 (powerline-raw "    " nil)))
	    (sticky (if (window-dedicated-p)
			(powerline-raw " ST " flag-face)
		      (powerline-raw "    " nil)))
	    (minor-modes (powerline-minor-modes face2))

	    (lhs (list
		  (propertize (concat " " (powerline-major-mode face1) " ") 'face face1)
		  (funcall separator-left face1 face2)
		  (propertize (concat " " minor-modes " ") 'face face2)
		  (funcall separator-left face2 mode-line-face)))

	    (rhs  (if active
		      (list
		       (funcall separator-right mode-line-face clock-face)
		       (powerline-raw (propertize (powerline-org-task) 'face clock-face) clock-face)
		       (funcall separator-right clock-face face1)
		       (powerline-raw (format-time-string " % %Y-%m-%d %a %H:%M ") face1))
		    '("")))
	    (center (list
		     (powerline-raw "%b @ %l:%c" modified)
		     " "
		     (powerline-vc)
		     " "
		     sticky
		     read-only
		     (propertize narrow 'face (when (powerline-narrow) flag-face)))))
       (concat (powerline-render lhs)
	       (powerline-fill-center nil (/ (powerline-width center) 2.0))
	       (powerline-render center)
	       (powerline-fill nil (powerline-width rhs))
	       (powerline-render rhs))))))

 (nby/with-feature
  'tomorrow-night-theme
  (color-theme-tomorrow--with-colors
   'night
   (custom-set-faces
    `(mode-line
      ((t (:background "grey15" :inherit mode-line))))
    `(mode-line-inactive
      ((t (:foreground "grey44" :inherit mode-line))))
    `(powerline-inactive
      ((t (:background "grey22" :inherit mode-line))))
    `(powerline-modified-bound-warning
      ((t(:background ,yellow :foreground ,background :inherit mode-line))))
    `(powerline-clock-inactive
      ((t (:background ,orange :foreground ,background :inherit mode-line))))
    `(powerline-clock-active ((t (:background "grey19" :foreground ,yellow :inherit mode-line))))
    `(powerline-flag-on
      ((t (:background ,green :foreground ,background :inherit mode-line))))
    `(powerline-active1 ((t (:background "grey23" :foreground ,blue :inherit mode-line))))
    `(powerline-inactive1 ((t (:foreground "grey44" :background "grey19"))))
    `(powerline-active2 ((t (:background "grey19" :foreground ,green :inherit mode-line))))
    `(powerline-inactive2 ((t (:foreground "grey44" :background "grey15"))))))))


;;; powerline.el ends here
