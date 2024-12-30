(defun my-protect-init-file ()
  "Make `init.el` read-only to prevent accidental edits."
  (when (and buffer-file-name
             (string-equal (file-truename buffer-file-name)
                           (file-truename user-init-file)))
    (read-only-mode 1)))

(add-hook 'find-file-hook 'my-protect-init-file)

(setq inhibit-startup-message t)

(setq initial-frame-alist
      '((fullscreen . fullboth)
        (undecorated . t)))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode '(5 . 5))  ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;;OS specific
;;Change needed on new machine.
(setq delete-by-moving-to-trash t)
(setq trashcan-dirname "~/Recycle Bin")

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


;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

;;Use this if all-the-icons package does not resolve the problem.
;;There will be icons missing after new installation. Install NerdFontsSymbolsOnly from nerd-fonts.
;;Link: https://github.com/ryanoasis/nerd-fonts/releases
;;
;;Change needed on new machine.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;;Theme

(use-package modus-themes)
(load-theme 'modus-vivendi-tinted :no-confirm)

;;Font
;;Change needed on new machine.
(defvar my-font-name "FiraCode Nerd Font Mono"
  "Text font to use.")
(defvar my-font-size 10 "Font size to use in points (for example, 10).") 
(defvar my-font (format "%s-%f" my-font-name my-font-size))

(defun font-exists-p (font)
  "Check if the FONT exists." (and (display-graphic-p) (not (null (x-list-fonts font)))))

(when (font-exists-p my-font)
  (set-frame-font my-font nil t))

;;Change Emacs backup file location
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;;Change Emacs auto-save file location
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosave/" t)))

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
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'tetris-mode 'insert))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;; Machine specific!
  (let ((projects-path "~/Desktop/Programming"))
    (when (file-directory-p projects-path)
      (setq projectile-project-search-path (directory-files projects-path t))))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; Execution needed on new machine.
;; Install this language server with M-x lsp-install-server RET ts-ls RET.
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

;; Slime config!
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; evil s-expression bindings
;; 
;;common lisp config
;;js, c, clojure config



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("faf642d1511fb0cb9b8634b2070a097656bdb5d88522657370eeeb11baea4a1c" "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" "d41229b2ff1e9929d0ea3b4fde9ed4c1e0775993df9d998a3cdf37f2358d386b" "7b602fe4a324dc18877dde647eb6f2ff9352566ce16d0b888bfcb870d0abfd70" "937401a2e532f2c8c881b6b3f20d9d4b6b9405bccf72ea6289c9d3f4507eb1ab" "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "a75aff58f0d5bbf230e5d1a02169ac2fbf45c930f816f3a21563304d5140d245" default))
 '(package-selected-packages
   '(evil-nerd-commenter lsp-treemacs lsp-ui company-box typescript-mode lsp-mode evil-magit magit counsel-projectile projectile evil-collection evil all-the-icons helpful ivy which-key modus-themes doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
