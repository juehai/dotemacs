;;; nby-theme-colors --- provides helper functions for theme other module
;;; Commentary:
;;; Code:


(defmacro nby/with-tomorrow-theme-colors (&rest body)
  "Execute BODY With current theme colors in context."
  `(nby/with-feature 'tomorrow-night-theme
                     (color-theme-tomorrow--with-colors 'night ,@body)))


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

          ;; compatible with base16 definitions
          (current-line             "#3E3D31")
          (selection                "#49483E")
          (comment                  "#75715E")
          (foreground               "#F8F8F2")
          (background               "#272822"))
     ,@body))

(provide 'nby-theme-colors)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; fill-column: 95
;; End:

;;; nby-theme ends here
