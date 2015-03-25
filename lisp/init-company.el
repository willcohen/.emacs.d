;;; Enable company mode
(require-package 'company)

(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
