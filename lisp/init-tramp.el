;;; TRAMP for remote editing
;;; On Windows, use PuTTY/plink to open files, using the following
;;; syntax. Be sure that PuTTY's folder is in the Windows path.
;;; /plink:user@remotehost:/remotepath
;;; /plink:user@remotehost|sudo:remotehost:/remotepath

(require 'tramp)
(when *is-my-desktop*
  (setq tramp-default-method "plink")
  )

(provide 'init-tramp)
