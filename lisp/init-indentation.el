;;; Indent code automatically when yanking
;;; Do it for ELisp, Lisp, Scheme, Python, R, and JS

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
		(member major-mode
			'(emacs-lisp-mode lisp-mode scheme-mode
					  python-mode r-mode js3-mode
					  ))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))


;; Replace tabs with spaces
(setq-default indent-tabs-mode nil)

(provide 'init-indentation)
