;;; Configuration for .emacs;;; Time-stamp: <2014-11-25 22:17:32 wcohen>;;; Install a nightly snapshot of Emacs using Homebrew on Mac:;;; brew install emacs --HEAD --use-git-head --cocoa --with-gnutls;;; --with-rsvg --with-imagemagick;;; ESS on Windows needs to be installed without MELPA:;;; http://vgoulet.act.ulaval.ca/en/emacs/;;; On Windows 7, after running emacs:;;; Pin it to taskbar, modify the shortcut to runemacs.exe instead of emacs.exe;;; And set it to run in XP SP3 compatibility mode (so the shell can;;; be switched to cygwin bash if desired);;;; SYSTEM DETECTION;; Check if system is Darwin/Mac OS X(defun system-type-is-darwin ()  (interactive)  "Return true if system is darwin-based (Mac OS X)"  (string-equal system-type "darwin")  );; Check if system is Linux(defun system-type-is-linux ()  (interactive)  "Return true if system is Linux-based"  (string-equal system-type "gnu/linux")  );; Check if system is Windows(defun system-type-is-windows ()  (interactive)  "Return true if system is Windows-based"  (string-equal system-type "w32")  );; Check if the system is my home laptop(defun system-is-my-laptop ()  (interactive)  "Return true if the system we are running on is my home laptop"  (string-equal system-name "Will-MacBookPro")  );; Check if the system is my work desktop(defun system-is-my-desktop ()  (interactive)  "Return true if the system we are running on is my work desktop"  (string-equal system-name "TKTKTK")  );;;; PACKAGE INSTALLATION;;; Use M-x list-packages to see what is available;;; List the packages that should be automatically installed.(setq package-list '(paredit paredit-menu web-mode zenburn-theme			     discover elpy js3-mode nodejs-repl			     guru-mode tern tern-auto-complete));;; Packages to install if Mac or Linux (unapplicable for Windows)(if (or (system-type-is-darwin) (system-type-is-linux))    (setq package-list (append package-list '(ess exec-path-from-shell)))  );;; List the repositories containing them(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")                         ("gnu" . "http://elpa.gnu.org/packages/")));;; Activate all the packages (in particular autoloads)(package-initialize);;; Fetch the list of packages available (unless package-archive-contents  (package-refresh-contents));;; Install the missing packages(dolist (package package-list)  (unless (package-installed-p package)    (package-install package)));;;; PACKAGE-INDEPENDENT ENVIRONMENT SETUP;;; Turn scrollbar off(scroll-bar-mode -1);;; Turn splash screen off(setq inhibit-startup-message t);;; Turn toolbar off(tool-bar-mode -1);;; Turn menubar off;;; (menu-bar-mode -1);;; Setup my full name(setq user-full-name "");;; Setup my email address(setq user-mail-address "");;; Turn on time-stamp updating. Timestamp must be in first 8 lines of;;;   file and look like:;;;   Time-stamp: <>(add-hook 'write-file-hooks 'time-stamp);;; Turn on Auto-Fill-Mode(setq-default auto-fill-function 'do-auto-fill);;; Show matching parentheses minor mode(show-paren-mode 1);;; Remove delay of show-paren-mode(setq show-paren-delay 0);;; Show matching paren when it is offscreen, in show-paren-mode(defadvice show-paren-function  (after show-matching-paren-offscreen activate)  "If the matching paren is offscreen, show the matching line in the        echo area. Has no effect if the character before point is not of        the syntax class ')'."  (interactive)  (let* ((cb (char-before (point)))	 (matching-text (and cb			     (char-equal (char-syntax cb) ?\) )			     (blink-matching-open))))    (when matching-text (message matching-text))));;; Font setup;;; Note that the OTF of Inconsolata on Windows looks blurry, so use;;; OTF on Mac (and Linux?) and TTF on Windows;;; On my laptop(if (system-is-my-laptop)    (set-face-attribute 'default nil :font "Inconsolata-14")    ;; (set-face-attribute 'default nil :font "Source Code Pro-12")    ;; (set-face-font 'default    ;; 		   '"-apple-inconsolata-medium-r-normal--14-0-72-72-m-0-iso10646-1")  );;;; PACKAGE-DEPENDENT ENVIRONMENT SETUP;;; Enable guru-mode to stop using bad keybindings(require 'guru-mode)(guru-global-mode +1);; To enable it only for modes like prog-mode;; (add-hook 'prog-mode-hook 'guru-mode);; To get warnings only for arrow keys;; (setq guru-warn only t);;; Enable exec-path-from-shell on Mac(if (system-type-is-darwin)    (exec-path-from-shell-initialize));;; Set default theme to zenburn(defun zenburn-init ()  (load-theme 'zenburn))(add-hook 'after-init-hook 'zenburn-init);;; Add discover mode (to learn Dired)(require 'discover)(global-discover-mode 1);;; Enable paredit(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)(add-hook 'ielm-mode-hook             #'enable-paredit-mode)(add-hook 'lisp-mode-hook             #'enable-paredit-mode)(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)(add-hook 'scheme-mode-hook           #'enable-paredit-mode);;; Add menu for paredit, so I can learn the commands better;; (require 'paredit-menu);;;; LANGUAGE SUPPORT;;; JavaScript;;; Use js3-mode for indentation and javascript(require 'js3-mode);;; Use node-repl to use Node.js locally as well(require 'nodejs-repl);;; Add command to send comint-region to nodejs-repl(defun send-region-to-nodejs-repl-process (start end)  "Send region to `nodejs-repl' process."  (interactive "r")  (save-selected-window    (save-excursion (nodejs-repl)))  (comint-send-region (get-process nodejs-repl-process-name)                      start end))(define-key js3-mode-map (kbd "C-c n")  'send-region-to-nodejs-repl-process);;; Enable use of Tern in JavaScript(when (require 'js2-mode nil 'noerror)  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))(when (require 'js3-mode nil 'noerror)  (add-hook 'js3-mode-hook (lambda () (tern-mode t))))(setq tern-command (cons (executable-find "tern") '()))(eval-after-load 'tern  '(progn     (require 'tern-auto-complete)     (tern-ac-setup)));;; Web-Mode (JavaScript/HTML in combined files)(require 'web-mode)(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode));;; Elpy (Python);;; In addition to the emacs package, need to install jedi and flake8;;; using pip.(elpy-enable);;;; MISCELLANEOUS COMMANDS;;; Get current system's name(defun insert-system-name() (interactive)  "Get current system's name"  (insert (format "%s" system-name))  );;; Get current system type(defun insert-system-type() (interactive)  "Get current system type"  (insert (format "%s" system-type))  );; Insertion of Dates.(defun insert-date-string () (interactive)  "Insert a nicely formated date string"  (insert (format-time-string "%a %b %d %H:%M:%S %Y")));;; Open init.el in the .emacs.d folder.(defun find-dot-emacs () (interactive)  "Try to find and open the dot emacs file"  (let ((my-init-file (if (not user-init-file)                                   "~/.emacs.d/init.el"                                 user-init-file)))    (find-file my-init-file)));;;; MISCELLANEOUS KEYBINDINGS;; C-c i calls insert-date-string(global-set-key (kbd "C-c i") 'insert-date-string);; C-c r reloads ~/.emacs.d/init.el(global-set-key (kbd "C-c r")		'(lambda ()		   (interactive)		   (load-file "~/.emacs.d/init.el")));;;; CUSTOM VARIABLES ADDED AUTOMATICALLY(custom-set-variables ;; custom-set-variables was added by Custom. ;; If you edit it by hand, you could mess it up, so be careful. ;; Your init file should contain only one such instance. ;; If there is more than one, they won't work right. '(custom-safe-themes   (quote    ("9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" default))))(custom-set-faces ;; custom-set-faces was added by Custom. ;; If you edit it by hand, you could mess it up, so be careful. ;; Your init file should contain only one such instance. ;; If there is more than one, they won't work right. )