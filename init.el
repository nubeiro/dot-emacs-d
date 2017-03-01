(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; paredit
(use-package paredit
  :ensure t
  :init
  (dolist (hook '(emacs-lisp-mode-hook 
                  lisp-mode-hook
                  clojure-mode-hook
                  scheme-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'paredit-mode))
  :diminish paredit-mode)


(use-package clojure-mode)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;;LISPy packages

    clojure-mode-extra-font-locking
    cider
    geiser
    rainbow-delimiters
    ;; emacsy packages
    ido-ubiquitous
    smex
    projectile
    tagedit
    flycheck
    company
    ;; git
    magit
    git-gutter
    git-timemachine
    ;;themes
    solarized-theme
    color-theme-sanityinc-tomorrow
    ;; other language
    markdown-mode
    php-mode
    ac-php
    yaml-mode))

(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "shell-integration.el")
(load "setup-flycheck.el")
(load "setup-company.el")
(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "setup-magit.el")
;; Languages
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-md.el")
(load "setup-php.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (use-package git-gutter yaml-mode tagedit solarized-theme smex rainbow-delimiters projectile paredit markdown-mode magit ido-ubiquitous flycheck exec-path-from-shell color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
