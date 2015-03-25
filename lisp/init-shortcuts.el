(require-package 'guru-mode)
(require-package 'deft)

;;; Turn on time-stamp updating. Timestamp must be in first 8 lines of
;;;   file and look like:
;;;   Time-stamp: <2015-03-25 13:28:59 wcohen>
(add-hook 'write-file-hooks 'time-stamp)

;;; Get current system's name
(defun insert-system-name() (interactive)
       "Get current system's name"
       (insert (format "%s" system-name))
       )

;;; Get current system type
(defun insert-system-type() (interactive)
       "Get current system type"
       (insert (format "%s" system-type))
       )

;; Insertion of Dates.
(defun insert-date-string () (interactive)
       "Insert a nicely formated date string"
       (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

;;; Open init.el in the .emacs.d folder.
(defun find-dot-emacs () (interactive)
       "Try to find and open the dot emacs file"
       (let ((my-init-file (if (not user-init-file)
                               "~/.emacs.d/init.el"
                             user-init-file)))
         (find-file my-init-file)))


;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)

;; C-c r reloads ~/.emacs.d/init.el
(global-set-key (kbd "C-c r")
		'(lambda ()
		   (interactive)
		   (load-file "~/.emacs.d/init.el")))


;;; Guru

;;; Enable guru-mode to stop using bad keybindings
(require 'guru-mode)
(guru-global-mode +1)

;; To enable it only for modes like prog-mode
;; (add-hook 'prog-mode-hook 'guru-mode)

;; To get warnings only for arrow keys
(setq guru-warn-only t)

;;; Deft (for notes)
(require 'deft)
(setq deft-use-filename-as-title t)

(provide 'init-shortcuts)

