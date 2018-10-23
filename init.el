(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(setq package-archives
      '(
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	;; ("org" . "http://orgmode.org/elpa/")
      ))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "43c1a8090ed19ab3c0b1490ce412f78f157d69a29828aa977dae941b994b4147" default)))
 '(package-selected-packages
   (quote
    (elpy counsel-projectile powerline ranger markdown-mode projectile org company-quickhelp mmm-mode json-mode counsel company ivy evil-org use-package htmlize evil)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)
(setq default-directory "~/")
(when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))

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
(column-number-mode)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; scrolling options
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(4 ((shift) . 10))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq backup-directory-alist  ;; backup files go in a dedicated directory
    `(("." . ,(concat user-emacs-directory "backups"))))
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "C-S-b") 'ibuffer)
(when window-system (set-frame-size (selected-frame) 160 48))
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))

(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
    (setenv "PATH" (concat "C:\\Users\\A3R7LZZ\\Programs\\Cygwin\\bin;" (getenv "PATH")))
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
    (setq evil-want-C-u-scroll t)
    (evil-mode 1)
    )

(use-package powerline
    :init
    (powerline-default-theme)
    )

(use-package org
    :init
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (setq
	org-startup-indented 1
	org-src-fontify-natively t
	org-src-preserve-indentation t
	org-src-tab-acts-natively t
	org-latex-table-scientific-notation nil
	org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-minted-options
	'(("frame" "leftline")
	  ("linenos" "")
	  ("fontsize" "\\small")
	  )
	org-latex-pdf-process
	'("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    )

(use-package ivy
    :init
    ;; not sure the next two are needed, but whatever
    (use-package counsel :init)
    (use-package swiper :init)
    (use-package counsel-projectile :init)
    (ivy-mode 1)
    (setq ivy-height 15)
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
    (setq company-selection-wrap-around t
	company-tooltip-align-annotations t
	company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-tooltip-limit 25)
    )

(use-package ranger
    :init
    )

(use-package markdown-mode
    :init
    )

(use-package mmm-mode
    :init
    (defun my-mmm-markdown-auto-class (lang &optional submode)
    "Define a mmm-mode class for LANG in `markdown-mode' using SUBMODE.
    If SUBMODE is not provided, use `LANG-mode' by default."
    (let ((class (intern (concat "markdown-" lang)))
	    (submode (or submode (intern (concat lang "-mode"))))
	    (front (concat "^```" lang "[\n\r]+"))
	    (back "^```"))
	(mmm-add-classes (list (list class :submode submode :front front :back back)))
	(mmm-add-mode-ext-class 'markdown-mode nil class)))
    (setq mmm-global-mode 'maybe) ;; Mode names that derive directly from the language name
    (mapc 'my-mmm-markdown-auto-class
	'("awk" "bibtex" "c" "cpp" "css" "html" "latex" "lisp" "makefile"
	    "markdown" "python" "r" "ruby" "sql" "stata" "xml"))
    )

(use-package elpy
  :init
  (elpy-enable)
  )

(cd "C:/Coding")
