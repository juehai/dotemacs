;;; coding -- Configuration for coding and languages
;;; Commentary:
;;;
;;;   Configuration for coding and languages that are not big enough to
;;;   be hosted in an individual file.
;;;
;;;   created at : 2013-01-24
;;;   author     : Jianing Yang
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Compilation
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables '(compilation-scroll-output t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code Tidy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete trailing whitespace, hope it's safe
(add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace)))

;; dont use tabe in most cases
(setq-default indent-tabs-mode nil)

;; indent & unindent
(global-set-key "\C-c." 'nby/indent-region)
(global-set-key "\C-c," 'nby/unindent-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YAML
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'yaml-mode
 (autoload 'yaml-mode "yaml-mode" nil t)

 (if (require 'nby-coding nil t)
   (nby/whitespace-detection-mode 'yaml-mode :tab t)
   (nby/log-warn "whitespace detection failed to start in yaml-mode"))

 ;; saltstack configurations
 (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
 (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Arduino
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'arduino-mode
(add-hook
 'arduino-mode-hook
 #'(lambda ()
     (local-set-key "\C-c\C-c" #'(lambda () (compile "scons upload"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PHP mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'php-mode
 (add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
 (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
 (add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
 (add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))
 (add-hook 'php-mode-hook
           #'(lambda ()
               (setq case-fold-search t
                     indent-tabs-mode nil
                     fill-column 78
                     c-basic-offset 4)
               (c-set-offset 'arglist-cont 0)
               (c-set-offset 'arglist-intro '+)
               (c-set-offset 'case-label 2)
               (c-set-offset 'arglist-close 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Haskell mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'haskell-mode
 (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
 (eval-after-load "haskell-mode"
   '(progn
      (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
      (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clojure mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature 'cider)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; tuareg mode for ocaml
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'tuareg
 (nby/with-feature
  'utop
  (when (executable-find "opam")
    ;; Setup environment variables using opam
    (dolist
        (var (car (read-from-string
                   (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))
    ;; Update the emacs path
    (setq exec-path (split-string (getenv "PATH") path-separator))
    ;; Update the emacs load path
    (push (concat (getenv "OCAML_TOPLEVEL_PATH")
                  "/../../share/emacs/site-lisp") load-path)
    ;; Automatically load utop.el
    (setq auto-mode-alist
          (append '(("\\.ml[ily]?$" . tuareg-mode)
                    ("\\.topml$" . tuareg-mode))
                  auto-mode-alist))
    (autoload 'utop "utop" "Toplevel for OCaml" t)
    (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
    (add-hook 'tuareg-mode-hook
              #'(lambda ()
                  (local-set-key (kbd "C-c C-c") 'utop-eval-phrase)
                  (local-set-key (kbd "C-c C-b") 'utop-eval-buffer)))
    (nby/with-feature
     'merlin
     (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
     (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
     ;; Start merlin on ocaml files
     (add-hook 'tuareg-mode-hook 'merlin-mode t)
     (add-hook 'caml-mode-hook 'merlin-mode t)
     ;; Enable auto-complete
     (setq merlin-use-auto-complete-mode 'easy)
     ;; Use opam switch to lookup ocamlmerlin binary
     (setq merlin-command 'opam)
     (setq merlin-error-after-save nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; VCS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'magit
 (global-set-key "\C-cg" 'magit-status))

(nby/with-feature 'fringe-helper)

(nby/with-feature
 'git-gutter+
 (setq git-gutter+-modified-sign "  " ;; two space
       git-gutter+-added-sign "++"    ;; multiple character is OK
       git-gutter+-deleted-sign "--")
 (nby/with-current-theme-colors
  (set-face-foreground 'git-gutter+-modified yellow)
  (set-face-foreground 'git-gutter+-added    green)
  (set-face-foreground 'git-gutter+-deleted  red))
 (global-git-gutter+-mode t))

(nby/with-feature
 'git-gutter-fringe+
 (nby/with-current-theme-colors
  (set-face-foreground 'git-gutter-fr+-modified yellow)
  (set-face-foreground 'git-gutter-fr+-added    green)
  (set-face-foreground 'git-gutter-fr+-deleted  red)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Any INI Mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature 'any-ini-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XCScope mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature 'xcscope)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (setq
;  projectile-cache-file (nby/build-relative-path "db/projectile.cache")
;  projectile-known-projects-file (nby/build-relative-path "db/projectile-bookmarks.eld"))
(nby/with-feature
 'projectile
 (nby/with-feature 'helm (helm-projectile-on))
 (projectile-global-mode t))

;;; coding.el ends here
