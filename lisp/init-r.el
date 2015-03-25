;;; Packages to install when not Windows
(when (not *is-windows*) (require-package 'ess))

(provide 'init-r)

