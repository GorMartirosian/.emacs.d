(setq inhibit-startup-message t)

(setq initial-frame-alist
      '((fullscreen . fullboth)
        (undecorated . t)))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 7)         ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;;Windows specific
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

;;ivy
;;
;;evil mode
;; vim-paredit
;; 
;;common lisp config
;;js/ts, c, clojure config


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("faf642d1511fb0cb9b8634b2070a097656bdb5d88522657370eeeb11baea4a1c" "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c" "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" "d41229b2ff1e9929d0ea3b4fde9ed4c1e0775993df9d998a3cdf37f2358d386b" "7b602fe4a324dc18877dde647eb6f2ff9352566ce16d0b888bfcb870d0abfd70" "937401a2e532f2c8c881b6b3f20d9d4b6b9405bccf72ea6289c9d3f4507eb1ab" "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "a75aff58f0d5bbf230e5d1a02169ac2fbf45c930f816f3a21563304d5140d245" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
