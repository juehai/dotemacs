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

(nby/add-to-load-path "themes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fonts: The following is for chinese-english width testing
;;
;; | 中文        | English     | 中文English | English中文 |
;; |-------------+-------------+-------------+-------------|
;; | English     | 中文English | English中文 | 中文        |
;; | 中文English | English中文 | 中文        | English     |
;; | English中文 | 中文        | English     | 中文English |
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/x-font-latin "Sans"
  "Font for english characters.")

(defvar nby/x-font-cjk "Sans"
  "Font for CJK characters.")

(defvar nby/chinese-font-scale 1.2
  "Rescale value for chinese to match latin fonts")

;; set xft font when we are using window system
(when window-system
  (nby/log-info "set latin font to '%s', set cjk font to '%s'"
                nby/x-font-latin
                nby/x-font-cjk)
  (if nby/x-font-latin
      (set-face-attribute 'default nil :font nby/x-font-latin))

  ;; refer to http://baohaojun.github.io/perfect-emacs-chinese-font.html
  (setq face-font-rescale-alist `((nby/x-font-cjk . ,nby/chinese-font-scale)))

  (if nby/x-font-cjk
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          nby/x-font-cjk))))
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

(defun load-monokai-theme ()
  "Apply monokai theme with custom fixes"
  (nby/with-feature
   'monokai-theme
   (setq nby/current-theme-colors 'nby/with-monokai-theme-colors)))

(defun load-solarized-light-theme ()
  "Apply monokai theme with custom fixes"
  (nby/with-feature
   'solarized-light-theme
   (color-theme-solarized-light)
   (setq nby/current-theme-colors 'nby/with-solarized-light-theme-colors)))

(defun load-solarized-dark-theme ()
  "Apply monokai theme with custom fixes"
  (nby/with-feature
   'solarized-dark-theme
   (color-theme-solarized-dark)
   (setq nby/current-theme-colors 'nby/with-solarized-light-theme-colors)))

(if nby/disable-color-theme
    (nby/log-info "color-theme has been disabled by configuration")
  (progn
    ;; install required package first
    (nby/with-feature
     'color-theme
     ;(nby/require 'tomorrow-theme)
      ;; enable color-theme
      (if window-system
          (load-monokai-theme)
          ;; (load-solarized-light-theme)
          ;; (load-solarized-dark-theme)
          (load-monokai-theme)))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fringe Width
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(left-fringe-width 11)
 '(right-fringe-width 11))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sticky Window
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this feature makes C-x 1 behave weird
;(nby/with-feature
; 'sticky-windows-plus
; (global-set-key "\C-x0" 'sticky-window-delete-window)
; (global-set-key "\C-x1" 'sticky-window-delete-other-windows)
; (global-set-key "\C-x9" 'sticky-windows-plus-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dired+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature 'dired+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tabbar-ruler
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'tabbar-ruler

 (setq tabbar-ruler-global-tabbar t
       tabbar-ruler-global-ruler nil
       tabbar-ruler-popup-menu nil
       tabbar-ruler-popup-toolbar nil
       tabbar-ruler-popup-scrollbar nil)

 ;; disable default tabbar-install-faces
 (defadvice tabbar-install-faces (around nby/tabbar-install-faces-advice activate)
   "Avoids tabbar-ruler install its own faces."
   (nby/tabbar-install-faces))

 (defun nby/tabbar-install-faces ()
   "custom tabbar theme"
   (nby/with-current-theme-colors
    (dolist (face '(tabbar-default
                    tabbar-highlight
                    tabbar-selected
                    tabbar-unselected
                    tabbar-selected-highlight
                    tabbar-unselected-highlight))
      (face-spec-reset-face face nil))
    (setq tabbar-background-color mode-line)
    (set-face-attribute 'tabbar-default nil :box nil :family "Hannotate SC" :background mode-line :foreground comment :height 0.9 :box mode-line)
    (set-face-attribute 'tabbar-highlight nil :inherit nil :box nil)
    (set-face-attribute 'tabbar-separator nil :inherit 'tabbar-default)
    (set-face-attribute 'tabbar-selected nil :inherit 'tabbar-default :foreground yellow :background background :box background)
    (set-face-attribute 'tabbar-selected-modified nil :inherit 'tabbar-selected :weight 'ultra-bold :foreground yellow :background background)
    (set-face-attribute 'tabbar-unselected nil :inherit 'tabbar-default :foreground comment :background mode-line)
    (set-face-attribute 'tabbar-unselected-modified nil :inherit 'tabbar-unselected :weight 'ultra-bold :foreground comment :background mode-line)))

 (nby/tabbar-install-faces)
 ;; group by projectile if we have projectile loadded
 (nby/with-feature
  'projectile
  (tabbar-ruler-group-by-projectile-project))

 (global-set-key (kbd "M-n") 'tabbar-forward-tab)
 (global-set-key (kbd "M-p") 'tabbar-backward-tab))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; smart-mode-line
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'smart-mode-line
 (setq sml/no-confirm-load-theme t
       sml/theme 'respectful)
 (sml/setup))



;;; ui.el ends here
