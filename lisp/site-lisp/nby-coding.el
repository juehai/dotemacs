;;; nby-coding -- Utlities for coding
;;; Commentary:
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Whitespace Detection Minor Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface nby/font-lock-leading-whitespace-face
  '((t (:underline (:color "deep sky blue" :style wave))))
  "Face for leading whitespaces"
  :group 'whitespace-detection-faces)

(defface nby/font-lock-trailing-whitespace-face
  '((t (:underline (:color "deep sky blue" :style wave) )))
  "Face for leading whitespaces"
  :group 'whitespace-detection-faces)

(defun nby/whitespace-detection-mode (mode &key tab)
  "MODE which detect improper whitespaces and long lines.
If TAB is not nil, hightlight tab characters"
  (when tab
    (font-lock-add-keywords
     mode
     '(("\t+" (0 'nby/font-lock-leading-whitespace-face t)))))
  (font-lock-add-keywords
   mode
   '(("^\s+$" (0 'nby/font-lock-leading-whitespace-face t))
     ("\s+$" (0 'nby/font-lock-leading-whitespace-face t))
     ("^.\\{79\\}\\(.+\\)$" (1 'nby/font-lock-trailing-whitespace-face t)))))


(provide 'nby-coding)
;;; nby-coding.el ends here
