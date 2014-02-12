;;; mail --- Configuration for mailing using emacs
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic Behavior
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smtpmail is a builtin feature
(require 'smtpmail)

(custom-set-variables
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-stream-type 'starttls)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))

;; starttls support
(require 'starttls)
(setq starttls-use-gnutls t
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(nby/with-feature
 'mu4e
 (custom-set-variables
  '(mu4e-sent-folder "/local/sent")
  '(mu4e-drafts-folder "/local/drafts")
  '(mu4e-trash-folder "/local/trash")

  '(mail-user-agent 'mu4e-user-agent)     ;; set mu4e default emacs mail client
  `(mu4e-maildir ,(nby/path-join user-home-dir "Mails"))
  '(mu4e-sent-messages-behavior 'delete)  ;; don't save message to Sent Messages,
                                          ;; Gmail/IMAP takes care of this
  '(mu4e-view-show-images t)              ;; display inline image
  '(mu4e-confirm-quit nil)                ;; dont ask when quit
  '(message-kill-buffer-on-exit t)        ;; don't keep message buffers around
  '(org-mu4e-conver-to-html t)
  `(smtpmail-queue-dir ,(nby/path-join mu4e-maildir "local" "queue"))
  '(mu4e-html2text-command "html2text -utf8 -width 72"))
 (when (fboundp 'imagemagick-register-types) (imagemagick-register-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Main Menu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'mu4e
 (setq mu4e-bm-ignore "")
 (setq mu4e-bookmarks
       `( ("flag:unread AND NOT flag:trashed"                               "Unread messages"      ?u)
          (,(concat "to:" user-mail-address mu4e-bm-ignore)                 "Messages for me"      ?m)
          (,(concat "date:today..now" mu4e-bm-ignore)                       "Today's messages"     ?t)
          (,(concat "date:7d..now" mu4e-bm-ignore)                          "Last 7 days"          ?w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Message Cite
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mu-cite
(nby/with-feature
 'mu-cite
 (custom-set-variables
  '(message-cite-function 'mu-cite-original)
  '(mu-cite-top-format '("On " date ", " from " wrote:\n\n"))
  '(mu-cite-prefix-format '(" > "))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Account Settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; choose account when composing
(nby/with-feature
 'mu4e
 (defvar nby/mu4e-account-alist
   '()
   "Mail Account Lists")
 (defun nby/mu4e-set-account ()
   "Set the account for composing a message."
   (let* ((account
           (if mu4e-compose-parent-message
               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                 (string-match "/\\(.*?\\)/" maildir)
                 (match-string 1 maildir))
             (completing-read (format "Compose with account: (%s) "
                                      (mapconcat #'(lambda (var) (car var))
                                                 nby/mu4e-account-alist "/"))
                              (mapcar #'(lambda (var) (car var))
                                      nby/mu4e-account-alist)
                              nil t nil nil (caar nby/mu4e-account-alist))))
          (account-vars (cdr (assoc account nby/mu4e-account-alist))))
     (if account-vars
         (mapc #'(lambda (var)
                   (set (car var) (cadr var)))
               account-vars)
       (error "No email account found"))))
 (add-hook 'mu4e-compose-pre-hook 'nby/mu4e-set-account))

;;; mails.el ends here
