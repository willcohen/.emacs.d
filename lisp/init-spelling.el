(require 'ispell)

(when *is-my-desktop* (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))

(when (executable-find ispell-program-name)
  (require 'init-flyspell))

(provide 'init-spelling)
