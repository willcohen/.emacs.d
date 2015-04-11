
(setq org-structure-template-alist
      '(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
        ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
        ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
        ("v" "#+BEGIN_VERSE\n?\n#+END_VERSE" "<verse>\n?\n</verse>")
        ("c" "#+BEGIN_COMMENT\n?\n#+END_COMMENT")
        ("p" "#+BEGIN_PRACTICE\n?\n#+END_PRACTICE")
        ("l" "#+begin_src emacs-lisp\n?\n#+end_src" "<src lang=\"emacs-lisp\">\n?\n</src>")
        ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
        ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
        ("H" "#+html: " "<literal style=\"html\">?</literal>")
        ("a" "#+begin_ascii\n?\n#+end_ascii")
        ("A" "#+ascii: ")
        ("i" "#+index: ?" "#+index: ?")
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")))

;;; Install a nightly snapshot of Emacs using Homebrew on Mac:
;;; brew update
;;; brew unlink emacs
;;; brew uninstall emacs
;;; brew install emacs --HEAD --use-git-head --with-cocoa
;;; --with-gnutls --with-rsvg --with-imagemagick
;;; brew linkapps

;;; ESS on Windows needs to be installed without MELPA:
;;; http://vgoulet.act.ulaval.ca/en/emacs/

;;; On Windows 7, after running emacs:
;;; Pin it to taskbar, modify the shortcut to runemacs.exe instead of emacs.exe
;;; And set it to run in XP SP3 compatibility mode (so the shell can
;;; be switched to cygwin bash if desired)

;;; This file bootstraps the configuration and all its related files.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-my-laptop* (eq system-name 'Will-MacBookPro))
(defconst *is-my-desktop* (eq system-name 'UTILE-T1700-08))

;;; Bootstrapping
(require 'init-utils)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)

(require 'init-gui-frames)              ; GUI features
(require 'init-win32)                   ; Windows-specific issues
(require 'init-mac)                     ; Mac-specific issues
(require 'init-shortcuts)               ; Miscellaneous
(require 'init-text)                    ; Fill mode
(require 'init-indentation)             ; Indent after yanking
(require 'init-themes)
;; (require 'init-ido) ; disabled and replaced by helm

(require 'init-editing-utils)
(require 'init-paredit)             ; Paredit, etc.
(require 'init-r)
(require 'init-postgres)
(require 'init-tramp)
(require 'init-fonts)
(require 'init-helm)
(require 'init-windows)
(require 'init-discover)
(require 'init-company)
(require 'init-vc)
(require 'init-git)
(require 'init-github)
;; (require 'init-js)
(require 'init-flycheck)
(require 'init-spelling)
(require 'init-flyspell)
(require 'init-javascript)
(require 'init-python)
(require 'init-lisp)
(require 'init-web)
(require 'init-company)


;; Extra packages which don't require any configuration

(require-package 'htmlize)
(require-package 'regex-tool)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit-gh-pulls github-clone bug-reference-github github-browse-file yagist magit-svn zenburn-theme whole-line-or-region whitespace-cleanup-mode wgrep web-mode unfill undo-tree tern-auto-complete switch-window scratch regex-tool rainbow-delimiters project-local-variables paredit-menu paredit-everywhere page-break-lines nodejs-repl mwe-log-commands multiple-cursors move-dup magit json-mode js3-mode js-comint htmlize highlight-symbol highlight-escape-sequences helm-projectile guru-mode guide-key golden-ratio gitignore-mode gitconfig-mode git-timemachine git-messenger git-blame geiser fullframe flycheck fill-column-indicator expand-region exec-path-from-shell ess elpy discover diminish diff-hl deft coffee-mode browse-kill-ring ace-jump-mode ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
