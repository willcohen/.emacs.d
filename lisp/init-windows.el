(require-package 'golden-ratio)
(require-package 'winner)
(require-package 'switch-window)


;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
;; (winner-mode 1)


;;; Golden Ratio

(require 'golden-ratio)

(setq golden-ratio-exclude-modes '("nh-map-mode" "nh-message-mode"
                      "nh-status-mode" "rmail-mode"
                      "rmail-summary-mode" 
                      ;; fundamental-mode is added here because the
                      ;; temp buffers used by switch-window is
                      ;; fundamental-mode "fundamental-mode"
                      ))

(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)

;;; Helm Autoresize and Golden Ratio can coexist

(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

;; Make "C-x o" prompt for a target window when there are more than 2

(require 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(defadvice switch-window
    (around golden-ratio-resize-window activate)
  (if (<= (length (window-list)) 3)
      (call-interactively 'other-window)
      ad-do-it)
  (golden-ratio)
  nil)
(global-set-key (kbd "C-x o") 'switch-window)



;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key "\C-x1" 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer nil)))



(provide 'init-windows)
