;; setup package repositories
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						 ("gnu" . "https://elpa.gnu.org/packages/")
						 ))
(package-initialize)

;; use-package settings
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(org-babel-load-file (expand-file-name "initialize.org" user-emacs-directory))

(if (eq system-type 'windows-nt)
    (org-babel-load-file (expand-file-name "windows.org" user-emacs-directory))
    (org-babel-load-file (expand-file-name "nonwindows.org" user-emacs-directory)))
(if (eq system-type 'darwin)
    (org-babel-load-file (expand-file-name "macosx.org" user-emacs-directory)))
(if (file-exists-p (expand-file-name "local.org" user-emacs-directory))
    (org-babel-load-file (expand-file-name "local.org" user-emacs-directory)))

(put 'narrow-to-region 'disabled nil)

;; place all custom settings at the end here
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; lastly load the theme after marked safe by custom.el
(if (file-exists-p (expand-file-name "theme.org" user-emacs-directory))
    (org-babel-load-file (expand-file-name "theme.org" user-emacs-directory)))
