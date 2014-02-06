;;; org-mode-gtd --- Configuration for GTD org-mode
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Basic settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables

 ;; Remind things those deadline are in 5 days
 '(org-deadline-warning-days 5)

 ;; Set default column view headings: Task Effort Clock_Summary
 '(org-columns-default-format "%65ITEM(Task) %20TAGS %Effort(Effort){:} %CLOCKSUM")

 ;; global Effort estimate values
 '(org-global-properties '(("Effort_ALL" . "0:10 0:30 1:00 1:30 2:00 4:00 8:00"))))

;; save org schedule every 30 seconds
;; this impl omits annoying message from 'org-save-all-org-buffers'
(run-with-timer 0 30
		#'(lambda ()
		    (save-some-buffers t #'(lambda ()
					     (derived-mode-p 'org-mode)))
		    (when (featurep 'org-id) (org-id-locations-save))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Workflow
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ,---- Self Task Workflow
;; | TODO: A thing need to be done
;; | NEXT: A thing need to be done ASAP
;; | STARTED: Task on the go
;; | DONE: Task finished
;; | WAITING: Pending due to some reason
;; | CANCELLED: Cancelled due to some reason
;; | MEETING: Interrupted Meetings
;; `----

;; ,---- Task Checking Workflow
;; | CHECK: Check the progress of someone else
;; | VERIFIED: Well done
;; `----

(custom-set-variables

 ;; define keywords
 '(org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!/!)")
     (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
     (sequence "CHECK(k)" "|" "VERIFIED(v!)")))

 ;; workflow trigger
 '(org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     (done ("WAITING"))
     ("TODO" ("WAITING") ("CANCELLED"))
     ("NEXT" ("WAITING") ("CANCELLED"))
     ("STARTED" ("WAITING") ("CANCELLED"))
     ("DONE" ("WAITING") ("CANCELLED")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tags
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(org-tag-alist
   '((:startgroup)
     ("@HOME"    . ?h)
     ("@OFFICE"  . ?o)
     ("@STADIUM" . ?s)
     (:endgroup)
     (:startgroup)
     ("ADTIME"   . ?a)
     ("PERSONAL" . ?p)
     ("QUNAR"    . ?q)
     (:endgroup)
     ("MAYBE"    . ?m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Task template
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nby/org-base-directory "~/notes/")
(defvar nby/org-archive-pattern "~/notes/archive/%s_archive::")
(defvar nby/org-default-schedule-file "schedule/default.org")
(defvar nby/org-default-note-file "note/misc.org")
(defvar nby/org-default-solution-file "kb/solution.org")

(custom-set-variables
 '(org-archive-location nby/org-archive-pattern)
 '(org-id-locations-file (convert-standard-filename
                          (nby/build-relative-path "db/org-id-locations.el")))
 '(org-reverse-note-order t)
 '(org-capture-templates
   '(("t" "TODO" entry (file (nby/path-join nby/org-base-directory nby/org-default-schedule-file))
      "* TODO %?\n%U\n%a\n  %i" :clock-in nil :clock-resume t :prepend t)
     ("c" "CHECK" entry (file (nby/path-join nby/org-base-directory nby/org-default-schedule-file))
      "* CHECK %? \n%U\n%a\n  %i" :clock-in nil :clock-resume t :prepend t)
     ("m" "MEETING" entry (file (nby/path-join nby/org-base-directory nby/org-default-schedule-file))
      "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t :prepend t)
     ("n" "NOTE" entry (file (nby/path-join nby/org-base-directory nby/org-default-note-file))
      "* %?\n%U\n%i" :clock-in nil :clock-resume t :prepend t)
     ("h" "HABIT" entry (file (nby/path-join nby/org-base-directory nby/org-default-schedule-file))
      "* NEXT %?\n%a\nSCHEDULED: %t\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i\n%U"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Agenda
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 `(org-agenda-files '(,(nby/path-join nby/org-base-directory "schedule")))
 '(org-agenda-custom-commands
   '(("a" "Default Agenda"
      ((agenda ""
               ((org-agenda-ndays 1)
                (org-agenda-priority '(priority-up effort-down))))
       (tags-todo "-WAITING-CANCELLED-NOTRACK/!STARTED"
                  ((org-agenda-overriding-header "Started")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
       (tags-todo "-WAITING-CANCELLED-NOTRACK/!NEXT"
                  ((org-agenda-overriding-header "Next")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
       (tags-todo "-WAITING-CANCELLED-NOTRACK/!Check"
                  ((org-agenda-overriding-header "Check")
                   (org-tags-match-list-sublevels t)
                   (org-agenda-sorting-strategy
                    '(todo-state-down effort-up category-keep))))
       (tags-todo "+MAYBE-REFILE-CANCELLED-NOTRACK/!-STARTED-HOLD-NEXT"
                  ((org-agenda-overriding-header "Time Insensitive")
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-todo-ignore-scheduled t)
                   (org-agenda-todo-ignore-deadlines t)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags "LEVEL=1+REFILE-NOTRACK"
	     ((org-agenda-overriding-header "Dangling")))
       (tags-todo "+STYLE<>\"habit\"-MAYBE-REFILE-CANCELLED-NOTRACK/!-STARTED-HOLD-NEXT"
                  ((org-agenda-overriding-header "Scheduled TODO")
		   (org-agenda-todo-ignore-scheduled 'past)
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy
                    '(category-keep))))
       (tags "-REFILE-PROJECT-NOTRACK/"
             ((org-agenda-overriding-header "Tasks to Archive")
              (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))
      nil))))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving."
  (let ((next-headline (save-excursion (outline-next-heading))))
    ;; Consider only tasks with done todo headings as archivable candidates
    (if (member (org-get-todo-state) org-done-keywords)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (daynr (string-to-int (format-time-string "%d" (current-time))))
               (a-month-ago (* 60 60 24 (+ daynr 1)))
               (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
               (this-month (format-time-string "%Y-%m-" (current-time)))
               (subtree-is-current (save-excursion
                                     (forward-line 1)
                                     (and (< (point) subtree-end)
                                          (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
          (if subtree-is-current
              subtree-end ; Has a date in this month or last month, skip it
            nil))  ; available to archive
      (or next-headline (point-max)))))

;;; org-mode-gtd.el ends here
