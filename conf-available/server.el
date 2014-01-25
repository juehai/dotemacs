;;; server --- Configuration for server/client emacs
;;; Commentary:
;;; Code:

(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (local-set-key (kbd "C-c C-c") 'server-edit)))

(provide 'server)
;;; server.el ends here
