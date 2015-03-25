;;; On Windows, fix errors with unsafe directories in Emacs server
(require 'server)
(when *is-windows* (defun server-ensure-safe-dir (dir) "Noop" t))

(provide 'init-win32)
