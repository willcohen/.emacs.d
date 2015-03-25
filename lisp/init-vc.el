(require-package 'magit)

;;; Magit
;; On Windows, Git needs to ask for a password.
;; Ensure that Git on Windows is in the path.
(when *is-windows*
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(provide 'init-vc)
