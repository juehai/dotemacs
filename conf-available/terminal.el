;;; terminal --- Configuraton for emacs terminal
;;; Commentary:
;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Shell-mode / Term-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(custom-set-variables '(comint-prompt-read-only t))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (nby/local-set-variables '(scroll-margin 0))
              (local-set-key "\C-l"
                             #'(lambda ()
                                 (interactive)
                                 (eshell/clear)
                                 (eshell-send-input)))))

(add-hook 'term-mode-hook
          #'(lambda()
              (nby/local-set-variables '(scroll-margin 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ANSI Colors for SHell
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ansi-color-names-vector
      ["black" "red4" "green4" "yellow4"
       "DeepSkyBlue3" "magenta4" "cyan4" "white"])
(setq ansi-term-color-vector
      [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65"
       "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])

(custom-set-variables
 '(ansi-color-for-comint-mode t)
 '(term-default-bg-color "#111")
 '(term-default-fg-color "grey80"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Terminal using Anything
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'anything

 (defvar anything-c-source-terminal
   '((name . "Terminals")
     (candidates . nby/shell-buffers)
     (volatile)
     (action . (lambda (name) (switch-to-buffer name)))
     (type . string)))


 (defun nby/new-shell ()
   "Create a new shell buffer."
   (interactive)
   (shell (concat "*shell: " (read-from-minibuffer "Shell name: ") "*")))

 (defun nby/shell-buffers ()
   "Get the names of all shell buffers."
   (remove-if-not (lambda (x) (string-match "^\\*shell:" x))
                  (mapcar 'buffer-name (buffer-list))))

 (defun nby/anything-terminals (arg)
   (interactive "p")
   (cond
    ((> arg 1) (nby/new-shell))
    ((= (length (nby/shell-buffers)) 0) (nby/new-shell))
    ((= (length (nby/shell-buffers)) 1)
     (switch-to-buffer (car (nby/shell-buffers))))
    (t (anything
        :prompt "Terminals: "
        :candidate-number-limit 100
        :sources '(anything-c-source-terminal)))))


 (global-set-key (kbd "C-z") 'nby/anything-terminals))

;;; terminal.el ends here
