;;; nby-theme-colors --- provides helper functions for theme other module
;;; Commentary:
;;; Code:


(defmacro nby/with-tomorrow-theme-colors (&rest body)
  "Execute BODY With current theme colors in context."
  `(nby/with-feature 'tomorrow-night-theme
                     (color-theme-tomorrow--with-colors 'night ,@body)))


(defmacro nby/with-solarized-light-theme-colors (&rest body)
  `(which-flet ((find-color (name)
                            (let ((index (if window-system
                                             (if solarized-degrade
                                                 3
                                               (if solarized-broken-srgb 2 1))
                                           (case (display-color-cells)
                                             (16 4)
                                             (8  5)
                                             (otherwise 3)))))
                              (nth index (assoc name solarized-colors)))))
     (let ((yellow      (find-color 'yellow))
           (orange      (find-color 'orange))
           (red         (find-color 'red))
           (magenta     (find-color 'magenta))
           (violet      (find-color 'violet))
           (blue        (find-color 'blue))
           (cyan        (find-color 'cyan))
           (green       (find-color 'green))
          ;; Alternatives
          (background2              "#474747"))
       ,@body)))

(defmacro nby/with-monokai-theme-colors (&rest body)
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

          ;; Alternatives
          (background2              "#474747"))
     ,@body))

(defmacro nby/with-spolsky-theme-colors (&rest body)
  "Execute BODY With current theme colors in context."
  `(let* (
          ;; Primary colors
          (yellow                   "#EEDC82")
          (orange                   "#FC580C")
          (red                      "#F92672")
          (magenta                  "#FF80F4")
          (violet                   "#AE81FF")
          (blue                     "#66D9EF")
          (cyan                     "#A1EFE4")
          (green                    "#A6E22E")
          (gray                     "#555555")

          ;; Alternatives
          (background2              "#555555"))
     ,@body))

(provide 'nby-theme-colors)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; fill-column: 95
;; End:

;;; nby-theme ends here
