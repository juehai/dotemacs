(nby/with-feature
 'dashboard
 (setq dashboard-banner-logo-title
       (format "Emacs starts in %.2f seconds. "
               (- (float-time) nby/startup-timestamp)))
 (setq dashboard-items '((recents  . 8)
                         (projects . 8)))
 (dashboard-setup-startup-hook))
