;;; Font setup

;;; Note that the OTF of Inconsolata on Windows looks blurry, so use
;;; OTF on Mac (and Linux?) and TTF on Windows

;;; On my laptop
(when *is-my-laptop*
  (set-face-attribute 'default nil :font "Inconsolata-14")
  )

;;; On my desktop
(when *is-my-desktop*
  (set-face-attribute 'default nil :font "Source Code Pro-10")
  )

(provide 'init-fonts)
