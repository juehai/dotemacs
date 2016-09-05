;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rust mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'rust-mode
 ; (add-hook 'rust-mode-hook 'cargo-minor-mode)

 (defun rust-save-compile-and-run ()
   "Compile buffer content and run."
   (interactive)
   (save-buffer)
   (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
       (compile "cargo run")
     (compile (format "rustc %s && %s"
                      (buffer-file-name)
                      (file-name-sans-extension (buffer-file-name))))))
 (define-key rust-mode-map (kbd "C-c C-c") 'rust-save-compile-and-run)

 (nby/with-feature
  'flycheck-rust
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

 (nby/with-feature
  'racer
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)

  (nby/with-feature
   'company-racer
   (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
   (setq company-tooltip-align-annotations t))))

