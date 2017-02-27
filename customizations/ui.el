(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)  
(global-linum-mode)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow--define-theme night)
(set-face-attribute 'default nil :height 160)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 134) (height . 52)))
(setq 
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)
(setq-default frame-title-format "%b (%f)")
(global-set-key (kbd "s-t") '(lambda () (interactive)))
(setq ring-bell-function 'ignore)
