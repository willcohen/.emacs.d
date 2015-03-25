(require-package 'geiser)

;;; Scheme / Racket

(when *is-my-laptop*
  (setq geiser-racket-binary
        "/Applications/Racket v6.1.1/bin/racket"))

(provide 'init-lisp)
