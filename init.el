;; Add this line, if init.el is separated into different files
;;(add-to-list 'load-path '"~/.emacs.d/modules")
(defvar my-is-linux-system (eq system-type 'gnu/linux))
(defvar my-is-windows-system (eq system-type 'windows-nt))

(when my-is-windows-system (setq find-program "C:/cygwin64/bin/find.exe"))

(defun my-protect-init-file ()
  "Make `init.el` read-only to prevent accidental edits."
  (when (my-current-file-init-p)
    (read-only-mode 1)))

(defun my-current-file-init-p ()
  (and buffer-file-name
             (string-equal (file-truename buffer-file-name)
                           (file-truename user-init-file))))

(defun my-make-current-file-read-only ()
  (read-only-mode 1))

(add-hook 'find-file-hook #'my-make-current-file-read-only)

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

(defconst font-size 10
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

;;Theme

;; (use-package modus-themes)
;; (load-theme 'modus-vivendi-tinted :no-confirm)

(use-package doom-themes
  :ensure t
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

(use-package emacs
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t))

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
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
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
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(require 'project)

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

(use-package magit)

(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (message "Company loaded!!!!!")
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key prog-mode-map (kbd "<tab>") #'company-indent-or-complete-common)
  (add-hook 'emacs-lisp-mode-hook #'company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package evil-nerd-commenter
  :bind ("C-/" . evilnc-comment-or-uncomment-lines))

;; SLIME config!
;; IMPORTANT NOTE: INSTALL COMMON LISP USING SCOOP ON WINDOWS
(use-package slime
  :config
  (message "Slime loaded!!!!")
  (slime-setup '(slime-fancy slime-company)))

(defun my-slime-mode-keybindings ()
  "Used inside slime-repl-mode-hook"
  (evil-define-key 'normal slime-repl-mode-map
    (kbd "C-j") 'slime-repl-forward-input
    (kbd "C-k") 'slime-repl-backward-input))

(add-hook 'slime-repl-mode-hook #'my-slime-mode-keybindings)

(use-package slime-company
  :after (slime company)
  :config
  (message "Slime-Company loaded!!!!!!!!")
  (setq slime-company-display-arglist t)
  (setq inferior-lisp-program "sbcl"))

;; evil s-expression bindings
;; C  config
