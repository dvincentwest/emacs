;; Other than loading the theme, I prefer to set font attributes
;; via the menu which then gets saved to custom.el
(use-package zenburn-theme
  :ensure t
  :defer t  ; use if you don't want to load the theme
  )
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t
  )
(use-package color-theme-sanityinc-solarized
  :ensure t
  :defer t
  )
;; (load-theme 'sanityinc-solarized-dark)
;; (load-theme 'zenburn)
(load-theme 'sanityinc-tomorrow-eighties)
;; (load-theme 'adwaita)
;; (load-theme 'immaterial-light)
