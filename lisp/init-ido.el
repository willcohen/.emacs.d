;;; Set up ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; Make 'R' and 'C' in dired use ido.
(with-eval-after-load 'ido
  (put 'dired-do-rename 'ido nil)
  (put 'dired-do-copy 'ido nil))

(provide 'init-ido)
