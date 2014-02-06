;;; ui --- Configurations about emacs generic UI
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Behaviors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(current-language-environment "UTF-8")

 '(display-time-day-and-date t)            ;; Show date
 '(display-time-24hr-format  t)            ;; Show time in 24-hour format
 '(show-paren-mode     t)
 '(visible-bell        t)                  ;; turn on visible bell, it looks ugly on my mac
 '(scroll-bar-mode     nil)                ;; disable scrollbar as it looks ugly
 '(tool-bar-mode       nil)                ;; disable toolbar
 '(column-number-mode  t)                  ;; display column number
 '(display-time-mode   t)                  ;; Show time on status bar
 '(blink-cursor-mode   nil)                ;; Stop cursor blinking
 '(inhibit-startup-message t))             ;; disable splash screen


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fonts
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/x-font-latin
  "monofur-12:Bold"
  "Font for english characters.")

(defvar nby/x-font-cjk
  "monofur-12:Bold"
  "Font for CJK characters.")

;; set xft font when we are using window system
(when window-system
  (nby/log-info "set latin font to '%s', set cjk font to '%s'"
                nby/x-font-latin
                nby/x-font-cjk)
  (set-face-attribute 'default nil :font nby/x-font-latin)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      nby/x-font-cjk)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Locale
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment 'UTF-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Color Theme
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/disable-color-theme
  nil
  "Don't apply color theme.")


(defun tomorrow-night-theme-patched ()
  "Apply tommorrow night theme with custom fixes."
  (nby/with-feature
   'tomorrow-night-theme
   (nby/log-info "using color theme: tomorrow-night with patch")
   ;; patch some colors for other modes
   (color-theme-tomorrow--with-colors
    'night
    (setq org-priority-faces
	  `((?A . (:foreground ,background :background ,red :weight "bold"))))
    (custom-set-variables
     `(term-default-bg-color ,background)
     `(term-default-fg-color ,foreground)
     `(ansi-color-names-vector [,background ,red ,green ,yellow
					    ,blue ,purple ,aqua ,foreground])
     `(ansi-term-color-vector [,background ,red ,green ,yellow
					   ,blue ,purple ,aqua ,foreground]))
    (custom-set-faces
     ;; FIXME: The following color settings for terminal seems not work
     `(newsticker-treeview-selection-face (( t (:foreground ,blue :background "grey20"))))
     `(ecb-default-highlight-face (( t (:foreground ,background :background ,orange))))
     `(ecb-tag-header-face (( t (:foreground ,background :background ,blue))))
     `(speedbar-tag-face (( t (:foreground ,yellow))))
     `(bm-face (( t (:foreground ,background :background ,orange))))
     `(highlight (( t (:foreground ,background))))
     `(org-hide (( t (:background ,background :foreground ,background))))))))


(if nby/disable-color-theme
    (nby/log-info "color-theme has been disabled by configuration")
  (progn
    ;; install required package first
    (nby/with-feature
     'color-theme
     (nby/require 'tomorrow-theme)
      ;; enable color-theme
      (if window-system
          (tomorrow-night-theme-patched)
        (progn
          (nby/log-info "using color theme: tty-dark")
          (color-theme-tty-dark))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Parentheses Pairing: Display parentheses in different colors
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))

(nby/with-feature
 'highlight-parentheses
 (global-highlight-parentheses-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lambda: Display "lambda" keywords using lambda latin character.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'lambda-mode
 (custom-set-variables
  `(lambda-symbol ,(string (make-char 'greek-iso8859-7 107)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scrolling: scroll one line a time (less "jumpy" than defaults)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; scroll one line at a time
 '(mouse-wheel-progressive-speed nil)            ;; don't acceleratescrolling
 '(mouse-wheel-follow-mouse t)                   ;; scroll window under mouse
 '(scroll-step 1)                                ;; scroll one line at a time
 '(scroll-margin 3)
 '(scroll-up-aggressively 0.01)
 '(scroll-down-aggressively 0.01)
 '(scroll-conservatively 10000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Winner mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(nby/with-feature
 'winner
 (winner-mode t)
 (global-set-key "\C-c\C-\\" 'winner-undo))

;;; ui.el ends here
