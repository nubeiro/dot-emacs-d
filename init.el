(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

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
    paredit
    clojure-mode
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
(load "elisp-editing.el")
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
    (git-gutter yaml-mode tagedit solarized-theme smex rainbow-delimiters projectile paredit markdown-mode magit ido-ubiquitous flycheck exec-path-from-shell color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
