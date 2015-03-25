(require-package 'js3-mode)
(require-package 'nodejs-repl)
(require-package 'tern)
(require-package 'tern-auto-complete)

;;; JavaScript
;;; Use js3-mode for indentation and javascript
(require 'js3-mode)

;;; Use node-repl to use Node.js locally as well
(require 'nodejs-repl)

;;; Add command to send comint-region to nodejs-repl
(defun send-region-to-nodejs-repl-process (start end)
  "Send region to `nodejs-repl' process."
  (interactive "r")
  (save-selected-window
    (save-excursion (nodejs-repl)))
  (comint-send-region (get-process nodejs-repl-process-name)
                      start end))

(define-key js3-mode-map (kbd "C-c n")
  'send-region-to-nodejs-repl-process)

;;; Set js3-mode settings which are turned off
'(js3-auto-indent-p t)         ; right-comma
'(js3-enter-indents-newline t) ; Electric indent
'(js3-indent-on-enter-key t)   ; Fix indenting before continuing

;;; Enable use of Tern in JavaScript
(when (require 'js2-mode nil 'noerror)
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))
(when (require 'js3-mode nil 'noerror)
  (add-hook 'js3-mode-hook (lambda () (tern-mode t))))
(setq tern-command (cons (executable-find "tern") '()))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(provide 'init-js)
