;;; -*- lexical-binding: t; -*-

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Add this line, if init.el is separated into different files
;;(add-to-list 'load-path '"~/.emacs.d/modules")

(setopt auto-save-interval 20)
(setopt auto-save-visited-mode t)
(setopt auto-save-visited-interval 0.1) 

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq org-hide-emphasis-markers t)

(defvar my/is-linux-system (eq system-type 'gnu/linux))
(defvar my/is-windows-system (eq system-type 'windows-nt))
(defvar my/is-macos-system (eq system-type 'darwin))

(when my/is-windows-system
  (setq find-program "C:/cygwin64/bin/find.exe"))

(defun my/protect-init-file ()
  "Make `init.el` read-only to prevent accidental edits."
  (when (my/current-file-init-p)
    (read-only-mode 1)))

(defun my/current-file-init-p ()
  (and buffer-file-name
             (string-equal (file-truename buffer-file-name)
                           (file-truename user-init-file))))

(defun my/make-current-file-read-only ()
  (read-only-mode 1))

(add-hook 'find-file-hook #'my/make-current-file-read-only)

(setq inhibit-startup-message t)

(setq initial-frame-alist
      '((fullscreen . maximized)
	(undecorated . t)))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode '(5 . 5))  ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;;OS specific
;;Change needed on new machine.
(setq delete-by-moving-to-trash t)
(cond
 (my/is-macos-system
  (setq trash-directory "~/.Trash"))
 (my/is-windows-system
  (setq trashcan-dirname (expand-file-name "~/Recycle Bin"))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; Install Symbols Nerd Fonts Mono (Symbols Nerd Font) and
;; Firacode Nerd Font Mono from the website. 
;; You can use M-x nerd-icons-install-fonts to install
;; Symbols Nerd Fonts Mono for you.
;; OS specific: for Windows you’ll need to
;; manually install the font after you used this function.

;;Font
;;Change needed on new machine.
(defconst font-name "FiraCode Nerd Font Mono"
  "Name of the font to use.")

(defconst font-size (if my/is-windows-system 10 13)
  "Font size to use in points.")

(defun font-spec ()
  "Construct the full font specification."
  (format "%s-%d" font-name font-size))

(defun font-available-p (font)
  "Check if FONT exists on the current system."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(let ((font (font-spec)))
  (if (font-available-p font)
      (set-frame-font font nil t)))

;; Also install fonts using M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))


;;Theme

;; (use-package modus-themes)
;; (load-theme 'modus-vivendi-deuteranopia :no-confirm)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-challenger-deep t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;Change needed on new machine.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;;Change Emacs backup file location
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;;Change Emacs auto-save file location
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosave/" t)))

(use-package vlf
  :config
  (require 'vlf-setup)
  (setopt vlf-application 'dont-ask))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(column-number-mode)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
		treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(defun my/set-additional-general-purpose-keybindings ()
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (with-eval-after-load 'ivy
    (keymap-set evil-normal-state-map "C-n" 'ivy-next-line)
    (keymap-set evil-normal-state-map "C-p" 'ivy-previous-line)
    (keymap-set evil-insert-state-map "C-n" 'ivy-next-line)
    (keymap-set evil-insert-state-map "C-p" 'ivy-previous-line))

  (keymap-set evil-normal-state-map "C-w C-w" 'evil-window-next)

  (define-prefix-command 'my/evil-insert-C-w-map)
  (define-key evil-insert-state-map (kbd "C-w") 'my/evil-insert-C-w-map)
  (define-key my/evil-insert-C-w-map (kbd "C-w") #'evil-window-next)

  (advice-add 'counsel-find-file :before #'(lambda ()
					     (setq evil-want-minibuffer nil)))
  (advice-add 'counsel-M-x :before #'(lambda ()
					    (setq evil-want-minibuffer t))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-minibuffer t)
  :config
  (evil-mode 1)
  (keymap-set evil-insert-state-map "C-g" 'evil-normal-state)

  (my/set-additional-general-purpose-keybindings)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package project)

(defcustom project-root-markers
  '("package.lisp" "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (when-let ((root (locate-dominating-file path #'project-root-p))) ; goes up the path directory
    (cons 'transient (expand-file-name root))))

(add-to-list 'project-find-functions #'project-find-root)

(use-package magit
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  (with-eval-after-load 'ediff
    (custom-set-faces
     '(ediff-current-diff-A ((t (:background "#4B1818"))))
     '(ediff-fine-diff-A    ((t (:background "#6F1313" :weight bold))))

     '(ediff-current-diff-B ((t (:background "#267326")))) 
     '(ediff-fine-diff-B    ((t (:background "#2E8B2E" :weight bold))))

     '(ediff-current-diff-C ((t (:background "#264F78"))))
     '(ediff-fine-diff-C    ((t (:background "#3B6EA8" :weight bold))))

     '(ediff-even-diff-A    ((t (:background "#1e1e1e"))))
     '(ediff-even-diff-B    ((t (:background "#1e1e1e"))))
     '(ediff-odd-diff-A     ((t (:background "#252526"))))
     '(ediff-odd-diff-B     ((t (:background "#252526")))))))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (keymap-set company-active-map "<tab>" 'company-complete-selection)
  (keymap-set prog-mode-map "<tab>" 'company-indent-or-complete-common)
  (add-hook 'emacs-lisp-mode-hook #'company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

(defun my/set-slime-repl-mode-keybindings ()
  (evil-define-key 'normal slime-repl-mode-map
    (kbd "C-n") 'slime-repl-forward-input
    (kbd "C-p") 'slime-repl-backward-input)
  (evil-define-key 'insert slime-repl-mode-map
    (kbd "C-n") 'slime-repl-forward-input
    (kbd "C-p") 'slime-repl-backward-input))

;; SLIME config!
;; IMPORTANT NOTE: INSTALL COMMON LISP USING SCOOP ON WINDOWS
(use-package slime
  :after evil
  :config
  (slime-setup '(slime-fancy slime-company))
  (my/set-slime-repl-mode-keybindings))

(use-package slime-company
  :after (slime company)
  :config
  (setq slime-company-display-arglist t)
  (setq inferior-lisp-program "sbcl"))

(use-package clojure-mode)

(use-package cider
  :after clojure-mode
  :hook (clojure-mode . cider-mode))

(defun my/config-clojure-refactor-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))

(use-package clj-refactor
  :after clojure-mode
  :init
  (setq cljr-add-ns-to-blank-clj-files nil)
  :hook
  (clojure-mode . #'my/config-clojure-refactor-mode))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
	    :rev :newest)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  :config
  (dashboard-setup-startup-hook))

;; For treemacs to work, got to the all-the-icons github
;;  page and download the fonts manually.
(use-package treemacs
  :config
  (treemacs-git-mode 'deferred)
  (setopt treemacs-indet-guide-mode 'line)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-all-the-icons)

;; Machine specific: do not forget to install the LSP servers.
(use-package eglot
  :after (clojure-mode)
  :init (setopt eglot-autoshutdown t)
  :hook ((clojure-mode . eglot-ensure)))

(use-package xref
  :demand t)
