;;; ecb --- configuration for ecb
;;; Commentary:
;;; Code:

(nby/with-feature
 'ecb
 (setq semanticdb-default-save-directory
       (nby/build-relative-path "db/semantic.cache")))

;;; ecb.el ends here
