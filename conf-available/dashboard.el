(nby/with-feature
 'dashboard
 (setq dashboard-items '((recents  . 8)
                         (projects . 8)))
 (dashboard-setup-startup-hook))
