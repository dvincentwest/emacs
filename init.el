;; package and repo setup
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (github)))
 '(custom-safe-themes
   (quote
    ("3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" default)))
 '(package-selected-packages
   (quote
    (company counsel-projectile swiper-helm counsel ivy projectile evil-org yaml-mode use-package markdown-mode htmlize evil ess color-theme)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Consolas" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))

;; inhibit startup screen
(setq inhibit-startup-screen t
      ;; Show *scratch* on start
      initial-buffer-choice t)
(prefer-coding-system 'utf-8)
(setq column-fill 80)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; scrolling options
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq backup-directory-alist  ;; backup files go in a dedicated directory
    `(("." . ,(concat user-emacs-directory "backups"))))
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(when window-system (set-frame-size (selected-frame) 160 48))
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))

;; I like my background, but this is the only way I can get it without modifying the theme
(defun vince-background () (interactive)  ;; calling this at the end of the init doesn't work, but calling with M-x does
    (add-to-list 'default-frame-alist '(background-color . "#F8F8FF"))
    (set-background-color "#F8F8FF")
    )
;; special function to toggle comments
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
	(if (region-active-p)
	    (setq beg (region-beginning) end (region-end))
	    (setq beg (line-beginning-position) end (line-end-position)))
	(comment-or-uncomment-region beg end)))
	
;; use-package settings
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package evil
    :init
    (evil-mode 1)
    )

(use-package org
    :init
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (setq org-startup-indented 1
	org-src-fontify-natively t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t)
    )

(use-package projectile
    :init
    (projectile-mode)
    (counsel-projectile-mode)
    (setq projectile-completion-system 'ivy)
    )

(use-package company
    :init
    (company-mode)
    (add-hook 'after-init-hook 'global-company-mode)
    )

(use-package ivy
    :init
    (ivy-mode 1)
    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (define-key undo-tree-map (kbd "C-/") nil)  ;; so I can use it later for toggling comments
    )

(use-package ess
    :init
    (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "M-,") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)
    (global-set-key (kbd "<backtab>") 'company-complete-common)

    (setq company-selection-wrap-around t
	company-tooltip-align-annotations t
	company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-tooltip-limit 25)
    )

;; wrap up with changing the default directory
(setq default-directory "~/")
(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(vince-background)
