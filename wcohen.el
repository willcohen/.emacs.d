(load "~/.emacs.d/personal.el" t)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (eq system-type 'windows-nt))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
             '(("melpa" . "https://melpa.org/packages/")
              ("org" . "http://orgmode.org/elpa/")
              ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(defun wc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar wc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
    (around build-require-times (feature &optional filename noerror) activate)
  "Note in `wc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'wc/require-times
                     (cons feature
                           (wc/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun wc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun wc/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]+$" "" str))

;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun wc/directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (tramp-tramp-file-p file-name)
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(require 'package)

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install package `%s': %S" package err)
     nil)))

;;; Start package.el

;(setq package-enable-at-startup nil)
;(package-initialize)

(use-package cl-lib
  :ensure t
  :config
  (require 'cl-lib))

(defun wc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (cl-loop for column across tabulated-list-format
           when (string= col-name (car column))
           do (setf (elt column 1) width)))

(defun wc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (wc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (wc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'wc/maybe-widen-package-menu-columns)

(use-package bind-key
  :ensure t)

(when *is-mac*
  (use-package exec-path-from-shell
    :ensure t
    :config
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
  "LANG" "LC_CTYPE"))
      (add-to-list 'exec-path-from-shell-variables var))))


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(use-package wgrep
   :ensure t)

 (use-package
project-local-variables
   :ensure t)

 (use-package diminish
   :ensure t)

 (use-package scratch
   :ensure t)

 (use-package mwe-log-commands
   :ensure t)

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun wc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'wc/maybe-suspend-frame)


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

; Make scratch empty
(setq initial-scratch-message nil)

;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun wc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(setq ns-use-native-fullscreen nil)
(when (and *is-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-s-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (wc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (wc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-0") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(require 'server)
(when *is-windows* (defun server-ensure-safe-dir (dir) "Noop" t))

;;; Fix hard-links on Mac
(when *is-mac*
  (setq backup-by-copying-when-linked t))

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(defun swap-meta-and-super ()
  "Swap the mapping of meta and super. Very useful for people using their Mac
with a Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

(global-set-key (kbd "C-c w") 'swap-meta-and-super)

(use-package guru-mode
  :ensure t)

(use-package deft
  :ensure t)

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

;; C-c e reloads ~/.emacs.d/init.el
(global-set-key (kbd "C-c e")
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

(setq-default auto-fill-function 'do-auto-fill)

(setq sentence-end-double-space nil)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode
                        '(emacs-lisp-mode lisp-mode scheme-mode
                                          python-mode r-mode
                                          ))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))


;; Replace tabs with spaces
(setq-default indent-tabs-mode nil)

(use-package solarized-theme
   :ensure t)

 ;; make the fringe stand out from the background
 (setq solarized-distinct-fringe-background t)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
;(setq solarized-high-contrast-mode-line t)

;; Use less bolding
(setq solarized-use-less-bold t)

;; Use more italics
;(setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
(setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
;(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

(defun solarized-init ()
  (load-theme 'solarized-light)
  )

(add-hook 'after-init-hook 'solarized-init)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme))

(when (not window-system)
  (define-key key-translation-map [?\C-h] [?\C-']))

(let ((translations '(     229 [?\M-a] nil [?\M-b] 231 [?\M-c]
                           8706 [?\M-d]  nil [?\M-e]   402 [?\M-f]
                           169 [?\M-g]   729 [?\M-h]   nil [?\M-i]
                           8710 [?\M-j]  730 [?\M-k]   172 [?\M-l]
                           181 [?\M-m]   nil [?\M-n]   248 [?\M-o]
                           960 [?\M-p]   339 [?\M-q]   174 [?\M-r]
                           223 [?\M-s]   8224 [?\M-t]  nil [?\M-u]
                           8730 [?\M-v]  8721 [?\M-w]  8776 [?\M-x]
                           165 [?\M-y]   937 [?\M-z]   ;96 [?\M-~]
                           161 [?\M-1]   162 [?\M-4]   163 [?\M-3]
                           167 [?\M-6]   170 [?\M-9]   171 [?\M-\\]
                           175 [?\M-<]   176 [?\M-*]   177 [?\M-+]
                           182 [?\M-7]   183 [?\M-\(]  186 [?\M-0]
                           187 [?\M-|]   191 [?\M-\?]  198 [?\M-\"]
                           230 [?\M-']   247 [?\M-/]   728 [?\M->]
                           8211 [?\M-\-] 8212 [?\M-_]  8216 [?\M-\]]
                           8217 [?\M-}]  8218 [?\M-\)] 8220 [?\M-\[]
                           8221 [?\M-{]  8225 [?\M-&]  8226 [\?M-8]
                           8249 [?\M-#]  8250 [?\M-$]  8260 [?\M-!]
                           8364 [\?M-@]  8482 [?\M-2]  8734 [\?M-5]
                           8800 [?\M-=]  8804 [?\M-,]  8805 [?\M-.]
                           64257 [?\M-%] 64258 [?\M-^])))

  (while translations
    (let ((key (car translations)) (def (cadr translations)))
      (if key
          (define-key key-translation-map (make-string 1 key) def)))
    (setq translations (cddr translations))))

(use-package unfill
  :ensure t)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 ;blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 ;; show-trailing-whitespace t
 show-trailing-whitespace nil
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)

;;; Whitespace

(defun wc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'wc/no-trailing-whitespace))


(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode t))

;; To enable for a mode instead of using the global mode
;; (add-hook 'ruby-mode-hook 'whitespace-cleanup-mode)


(global-set-key [remap just-one-space] 'cycle-spacing)

;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun wc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'wc/newline-at-end-of-line)


(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))


(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))

(use-package highlight-symbol
  :ensure t
  :config
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode))
  (add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
  (diminish 'highlight-symbol-mode)
  (defadvice highlight-symbol-temp-highlight (around wc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless isearch-mode ad-do-it))
  )

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)


(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode)
  (diminish 'page-break-lines-mode)
  )


(use-package browse-kill-ring
  :ensure t
  :bind ("M-y" . browse-kill-ring)
  :config
  (setq browse-kill-ring-separator "\f")
  (push 'browse-kill-ring-mode page-break-lines-modes)
  )


;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-;" . ace-jump-mode)
         ("C-:" . ace-jump-word-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-+" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ;; From active region to multiple cursors
         ("C-c c r" . set-rectangular-region-anchor)
         ("C-c c c" . mc/edit-lines)
         ("C-c c e" . mc/edit-ends-of-lines)
         ("C-c c a" . mc/edit-beginnings-of-lines)
         ))

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])



(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(use-package move-dup
  :ensure t
  :bind (
         ([M-up] . md/move-lines-up)
         ([M-down] . md/move-lines-down)
         ([M-S-up] . md-move-lines-up)
         ([M-S-down] . md/move-lines-down)
         ("C-c p" . md/duplicate-down)
         ("C-c P" . md/duplicate-up)))

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-mode t)
  (diminish 'whole-line-or-region-mode)
  (make-variable-buffer-local 'whole-line-or-region-mode)
  )

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

(defun wc/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'wc/open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))









(use-package highlight-escape-sequences
  :ensure t
  :config
  (hes-mode))



(use-package guide-key
  :ensure t
  :config
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r"))
  (guide-key-mode 1)
  (diminish 'guide-key-mode)
  )

(blink-cursor-mode 0)

(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit")

  (defun maybe-map-paredit-newline ()
    (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
                (minibufferp))
      (local-set-key (kbd "RET") 'paredit-newline)))

  (add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)
  (diminish 'paredit-mode " Par")
  (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
                         (kbd "C-M-<left>") (kbd "C-M-<right>")))
    (define-key paredit-mode-map binding nil))

  ;; Disable kill-sentence, which is easily confused with the kill-sexp
  ;; binding, but doesn't preserve sexp structure
  (define-key paredit-mode-map [remap kill-sentence] nil)
  (define-key paredit-mode-map [remap backward-kill-sentence] nil)

  ;; Allow my global binding of M-? to work when paredit is active
  (define-key paredit-mode-map (kbd "M-?") nil)

  (suspend-mode-during-cua-rect-selection 'paredit-mode)

  ;; Use paredit in the minibuffer
  ;; TODO: break out into separate package
  ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

  (defvar paredit-minibuffer-commands '(eval-expression
                                        pp-eval-expression
                                        eval-expression-with-eldoc
                                        ibuffer-do-eval
                                        ibuffer-do-view-and-eval)
    "Interactive commands for which paredit should be enabled in the minibuffer.")

  (defun conditionally-enable-paredit-mode ()
    "Enable paredit during lisp-related minibuffer commands."
    (if (memq this-command paredit-minibuffer-commands)
        (enable-paredit-mode)))
  )

(use-package paredit-everywhere
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
  (add-hook 'css-mode-hook 'paredit-everywhere-mode)
  )

(when (not *is-windows*)
  (use-package ess
    :ensure t)
  )

(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432)))

  ;;; On Windows, use the Cygwin psql client instead (and be sure it is
  ;;; installed).

(when *configured-windows*
  (setq sql-postgres-program "C:/cygwin64/bin/psql.exe"))

(when *configured-mac*
  (setq sql-postgres-program "/Applications/Postgres.app/Contents/Versions/latest/bin/psql"))

(use-package sql-indent
  :ensure t)

'(load-library "sql-indent")

(use-package cider
  :ensure t)

(require 'tramp)
(when *configured-windows*
  (setq tramp-default-method "plink")
  )

(defun wc/first-available-font (&rest stack)
  (loop for f in stack
        if (member f (font-family-list))
        do (return f)))

(setq wc/font-fixed
      (wc/first-available-font
       "Source Code Pro"
       "Inconsolata"
       "Droid Sans Mono"
       "Ubuntu Mono"
       "Menlo"
       "Monaco"
       "fixed"
       )
      )

(when *configured-mac*
  (set-face-attribute 'default nil
                      :family wc/font-fixed
                      ; :height 140
                      ; :weight 'light
                      ))

(when *configured-windows*
  (set-face-attribute 'default nil
                      :family wc/font-fixed
                      :height 100
                      ; :weight 'light
                      ))

(use-package ivy
  :ensure t
  :pin melpa)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(use-package find-file-in-project
  :ensure
  :init
  (autoload 'find-file-in-project "find-file-in-project" nil t)
  (autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
  (autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
  (autoload 'ffip-show-diff "find-file-in-project" nil t)
  (autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
  (autoload 'ffip-ivy-resume "find-file-in-project" nil t)
  )

(use-package projectile
  :ensure t)

(projectile-global-mode)

(use-package counsel-projectile
  :ensure t)

(counsel-projectile-on)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package ob-ipython
  :ensure t
  :config
  (require 'ob-ipython)
  )

(setq python-shell-completion-native-enable nil)

(use-package geiser
  :ensure t
  :config
  (when *configured-mac*
    (setq geiser-racket-binary
          "/Applications/Racket v6.1.1/bin/racket"))
  )

(use-package sicp
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (require 'web-mode))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

;(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun syntax-colorize-foreground (color)
  "Colorize foreground based on background luminance."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (car (cdr (cdr values)))))
    (if (> 128.0 (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256))
        "white" "black")))

(defun add-syntax-color-hex ()
  "Syntax color hex color spec such as 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{3,6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)
                      :foreground (syntax-colorize-foreground
                                   (match-string-no-properties
                                    0))))))))
  (font-lock-fontify-buffer)
  )

;;; (add-hook 'css-mode-hook 'add-syntax-color-hex)

(defun web-mode-hook-settings ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  ;;    (idle-highlight-mode 0)
  ;;    (font-lock-mode 0)
  )

(add-hook 'web-mode-hook 'web-mode-hook-settings)

(use-package json-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
  )

(defvar preferred-javascript-indent-level 2)

(setq-default js2-basic-offset preferred-javascript-indent-level
              js2-bounce-indent-p nil)

(use-package js-comint
  :ensure t)

(use-package nvm
  :ensure t)

(setq inferior-js-program-command "node")
(setq inferior-js-program-arguments '("--interactive"))

(js-do-use-nvm)

;; Keybindings to send from js2-mode
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                            (local-set-key "\C-cl" 'js-load-file-and-go)
                            ))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'load-path
               "~/.emacs.d/plugins/yasnippet")
  (require 'yasnippet)
  (yas-global-mode 1))

(use-package company
  :ensure t
  :config

  ;; company mode
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; company delay until suggestions are shown
  (setq company-idle-delay 0.5)

  ;; weight by frequency
  (setq company-transformers '(company-sort-by-occurrence))

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )



(use-package eclim
  :ensure t)

(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

(setq eclimd-executable
      "/Applications/Eclipse.app/Contents/Eclipse/eclimd")
(setq eclim-executable
      "/Applications/Eclipse.app/Contents/Eclipse/eclim")

(use-package company-emacs-eclim
  :ensure t)
(company-emacs-eclim-setup)
(global-company-mode t)

(use-package gradle-mode
  :ensure t)

(add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))


; Might work -- haven't needed to try yet.

; Commands to make grade more useful.
;(defun build-and-run ()
;  (interactive)
;  (gradle-run "build run"))

;(define-key gradle-mode-map (kbd "C-c C-r") 'build-and-run)

; Gradle colors
;(custom-set-faces
 ;; ...
 ;'(company-preview ((t (:background "black" :foreground "red"))))
 ;'(company-preview-common ((t (:foreground "red"))))
 ;'(company-preview-search ((t (:inherit company-preview))))
 ;'(company-scrollbar-bg ((t (:background "brightwhite"))))
 ;'(company-scrollbar-fg ((t (:background "red"))))
 ;'(company-template-field ((t (:background "magenta" :foreground "black"))))
 ;'(company-tooltip ((t (:background "brightwhite" :foreground "black"))))
 ;'(company-tooltip-annotation ((t (:background "brightwhite" :foreground "black"))))
 ;'(company-tooltip-annotation-selection ((t (:background "color-253"))))
 ;'(company-tooltip-common ((t (:background "brightwhite" :foreground "red"))))
 ;'(company-tooltip-common-selection ((t (:background "color-253" :foreground "red"))))
 ;'(company-tooltip-mouse ((t (:foreground "black"))))
 ;'(company-tooltip-search ((t (:background "brightwhite" :foreground "black"))))
 ;'(company-tooltip-selection ((t (:background "color-253" :foreground
 ;                                             "black"))))
 ;; ...
 ;)

; When going over error with cursor, this command can select some
;possible corrections
;(define-key eclim-mode-map (kbd "C-c C-c") 'eclim-problems-correct)

; M-x eclim-java-refactor-rename-symbol-at-point can rename symbols.
; M-x eclim-java-refactor-move-class can move classes.

(use-package emmet-mode
  :ensure t
  :config

  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)

  ;; Enable emmet's css abbrevation
  (add-hook 'css-mode-hook  'emmet-mode)

  (setq emmet-move-cursor-between-quotes t) ;; default nil
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "kramdown"))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(defvar wc/refile-map (make-sparse-keymap))

(defmacro wc/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key wc/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))

(wc/defshortcut ?i "~/.emacs.d/wcohen.org")

(bind-key "C-c g" 'jump-to-register)

(use-package org
  :ensure org-plus-contrib
  :defer 7)

(setq org-modules '(org-bbdb
                    org-gnus
                    org-drill
                    org-info
                    org-jsinfo
                    org-habit
                    org-irc
                    org-mouse
                    org-annotate-file
                    org-eval
                    org-expiry
                    org-interactive-query
                    org-man
                    org-collector
                    org-panel
                    org-screen
                    org-toc))
(eval-after-load 'org
  '(org-load-modules-maybe t))
(setq org-expiry-inactive-timestamps t)

(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

(defun wc/org-follow-entry-link ()
  "Follow the defined link for this entry."
  (interactive)
  (if (org-entry-get (point) "LINK")
      (org-open-link-from-string (org-entry-get (point) "LINK"))
    (org-open-at-point)))

(bind-key "C-c o" 'wc/org-follow-entry-link org-mode-map)

(defun wc/org-link-projects (location)
  "Add link properties between the current subtree and the one specified by LOCATION."
  (interactive
   (list (let ((org-refile-use-cache nil))
     (org-refile-get-location "Location"))))
  (let ((link1 (org-store-link nil)) link2)
    (save-window-excursion
      (org-refile 4 nil location)
      (setq link2 (org-store-link nil))
      (org-set-property "LINK" link1))
    (org-set-property "LINK" link2)))

(eval-after-load 'org
  '(progn
     (bind-key "C-c k" 'org-cut-subtree org-mode-map)
     (setq org-yank-adjusted-subtrees t)))

(bind-key "C-c r" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c L" 'org-insert-link-global)
(bind-key "C-c O" 'org-open-at-point-global)
(bind-key "<f9> <f9>" 'org-agenda-list)
(bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
(bind-key "C-TAB" 'org-cycle org-mode-map)
(bind-key "C-c v" 'org-show-todo-tree org-mode-map)
(bind-key "C-c C-r" 'org-refile org-mode-map)
(bind-key "C-c R" 'org-reveal org-mode-map)

(eval-after-load 'org
  '(bind-key "C-M-w" 'append-next-kill org-mode-map))

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/organizer.org")

(defun wc/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'wc/yank-more)

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
        ("I" "#+include %file ?" "<include file=%file markup=\"?\">")
        ("r" "#+begin_src R\n?\n#+end_src" "<src lang=\"R\">\n?\n</src>")
        ("rs" "#+begin_src R :session\n?\n#+end_src" "<src lang=\"R\">\n?\n</src>")
        ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ipython . t)
   (R . t)
   (sql . t)
   (shell . t)
   ))

(when *configured-windows*
  (setq org-babel-R-command "C:/Progra~1/R/R-3.2.2/bin/R.exe
  --slave --no-save --ess"))

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  )

(use-package magit
  :ensure
  :init
  (setq-default
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'ivy-completing-read)
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  (add-hook 'magit-popup-mode-hook 'wc/no-trailing-whitespace)
  (when *is-mac*
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))
  )

(use-package gitignore-mode
  :ensure)

(use-package gitconfig-mode
  :ensure)

(use-package git-timemachine
  :ensure)

;; On Windows, Git needs to ask for a password.
;; Ensure that Git on Windows is in the path.
(when *is-windows*
  (setenv "GIT_ASKPASS" "git-gui--askpass"))

(require-package 'yagist)
(require-package 'github-browse-file)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(maybe-require-package 'github-clone)
(maybe-require-package 'magit-gh-pulls)

(require-package 'discover)
(require 'discover)
(global-discover-mode 1)

(require-package 'winner)
(require-package 'switch-window)


;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)

(require 'switch-window)
(setq switch-window-shortcut-style 'qwerty)

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

(defun wc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key "\C-x1" 'wc/toggle-delete-other-windows)

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


;; Next window and previous window

(defun prev-window ()
  (interactive)
  (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun wc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'wc/split-window)
      (progn
        (jump-to-register :wc/split-window)
        (setq this-command 'wc/unsplit-window))
    (window-configuration-to-register :wc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'wc/split-window)
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer nil)))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)

  ;; Override default flycheck triggers
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)

  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list))

;; From http://pastebin.com/bS8r3Euk
;; Disable jshint on windows
(when *is-windows*
  (add-hook 'js2-mode-hook
            (lambda () (setq flycheck-disabled-checkers
                        '(javascript-jshint)))))

(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))


;; use eslint with web-mode and js2-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)


;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(require 'ispell)

(when *configured-windows* (add-to-list 'exec-path
      "C:/Program Files (x86)/Aspell/bin/"))

(when (executable-find ispell-program-name)
;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------
(if (fboundp 'prog-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook
                  yaml-mode
                  python-mode-hook
                  shell-mode-hook
                  php-mode-hook
                  css-mode-hook
                  haskell-mode-hook
                  caml-mode-hook
                  nxml-mode-hook
                  crontab-mode-hook
                  perl-mode-hook
                  tcl-mode-hook
                  javascript-mode-hook))
    (add-hook hook 'flyspell-prog-mode)))

(after-load 'flyspell
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

(require-package 'htmlize)
(require-package 'regex-tool)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------


;; TODO: Diagnose why I needed to comment these out on windows
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun wc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (wc/utf8-locale-p (and (executable-find "locale") (shell-command-to-string "locale")))
      (wc/utf8-locale-p (getenv "LC_ALL"))
      (wc/utf8-locale-p (getenv "LC_CTYPE"))
      (wc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
  (prefer-coding-system 'utf-8))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (wc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit-gh-pulls github-clone bug-reference-github github-browse-file yagist magit-svn zenburn-theme whole-line-or-region whitespace-cleanup-mode wgrep web-mode unfill undo-tree tern-auto-complete switch-window scratch regex-tool rainbow-delimiters project-local-variables paredit-menu paredit-everywhere page-break-lines nodejs-repl mwe-log-commands multiple-cursors move-dup magit json-mode js3-mode js-comint htmlize highlight-symbol highlight-escape-sequences guru-mode guide-key golden-ratio gitignore-mode gitconfig-mode git-timemachine git-messenger git-blame geiser fullframe flycheck fill-column-indicator expand-region exec-path-from-shell ess elpy discover diminish diff-hl deft coffee-mode browse-kill-ring ace-jump-mode ac-js2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
