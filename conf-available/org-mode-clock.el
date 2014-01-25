;;; org-mode-clock --- Configuration for clock related functions
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keybindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-cI" 'bh/punch-in)
(global-set-key "\C-cO" 'bh/punch-out)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hooking clock
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nby/with-feature
 'org
 (add-hook 'after-init-hook #'(lambda () (bh/punch-in t)))
 (add-hook
  'kill-emacs-hook
  #'(lambda ()
      (condition-case nil
          (progn
            (bh/punch-out)
            (org-save-all-org-buffers))
        (error (progn
                 (nby/log-warn "cannot punch out clock") nil))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clock settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

(custom-set-variables

 ;; clock data
 '(org-clock-persist-file (convert-standard-filename
                           (nby/build-relative-path "db/org-clock-save.el")))

 ;; Yes it's long... but more is better ;)
 '(org-clock-history-length 28)

 ;; Resume clocking task on clock-in if the clock is open
 '(org-clock-in-resume t)

 ;; Separate drawers for clocking and logs
 '(org-drawers (quote ("PROPERTIES" "LOGBOOK")))

 ;; Save clock data and state changes and notes in the LOGBOOK drawer
 '(org-clock-into-drawer t)

 ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
 '(org-clock-out-remove-zero-time-clocks t)

 ;; Clock out when moving task to a done state
 '(org-clock-out-when-done t)

 ;; Set state to "STARTED" when clocking in
 '(org-clock-in-switch-to-state 'bh/clock-in-to-started)

 ;; Save the running clock and all clock history when exiting Emacs, load it on startup
 '(org-clock-persist 'history)

 ;; Enable auto clock resolution for finding open clocks
 '(org-clock-auto-clock-resolution 'when-no-clock-is-running)

 ;; Include current clocking task in clock reports
 '(org-clock-report-include-clocking-task t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ticking
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bh/keep-clock-running nil)

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16)))))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at (or parent-task)
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(add-hook 'org-agenda-mode-hook '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock) 'append))

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (org-get-at-bol 'org-hd-marker))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (cond
       ((equal major-mode 'org-agenda-mode)
        (org-with-point-at pom
          (org-agenda-set-restriction-lock restriction-type)))
       ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
        (org-agenda-set-restriction-lock 'file))
       (t
        (org-with-point-at pom
          (org-agenda-set-restriction-lock restriction-type)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interact with screensaver
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/org-clock-last-task nil)

(nby/with-feature
 'dbus
 (defun nby/org-check-in-out-on-screensaver (p-screen-locked)
   (if p-screen-locked
       (if org-clock-current-task
           (progn
             (setq nby/org-clock-last-task org-clock-current-task)
             (bh/punch-out)
             (message "punch-out because screen is locked"))
         (setq nby/org-clock-last-task nil))
     (if nby/org-clock-last-task
         (progn
           (bh/punch-in 0)
           (message "punch-in because screen is unlocked")))))

 (if (eq system-type 'gnu/linux)
     (condition-case nil
         (dbus-register-signal :session "org.gnome.ScreenSaver"
                               "/org/gnome/ScreenSaver"
                               "org.gnome.ScreenSaver"
                               "ActiveChanged"
                               'nby/org-check-in-out-on-screensaver)
       (error nil))))

;;; org-mode-clock.el ends here
