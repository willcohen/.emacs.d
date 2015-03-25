(require-package 'zenburn-theme)

;;; Set default theme to zenburn
(defun zenburn-init ()
  (load-theme 'zenburn)
  )

(add-hook 'after-init-hook 'zenburn-init)


(provide 'init-themes)
