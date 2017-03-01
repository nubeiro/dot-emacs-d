(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'diminish)

(use-package company
  :ensure t
  :defer t
  :init 
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-dabbrev-downcase nil)
  :config 
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map))
  :diminish company-mode)

(use-package paredit
  :ensure t
  :defer t
  :config
  (dolist (hook '(emacs-lisp-mode-hook 
                  lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'paredit-mode))
  :diminish paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  ;; syntax highlighting for midje
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq inferior-lisp-program "lein repl")
              (font-lock-add-keywords
               nil
               '(("(\\(facts?\\)"
                  (1 font-lock-keyword-face))
                 ("(\\(background?\\)"
                  (1 font-lock-keyword-face))))
              (define-clojure-indent (fact 1))
              (define-clojure-indent (facts 1))))
  ;; Use clojure mode for other extensions
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t
  :init
  (setq cider-repl-pop-to-buffer-on-connect t
        cider-show-error-buffer t
        cider-auto-select-error-buffer t
        cider-repl-history-file "~/.emacs.d/cider-history"
        cider-repl-wrap-history t
        cider-repl-use-pretty-printing t)
  :config 
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(use-package geiser
  :ensure t
  :defer t
  :config
  (add-hook 'scheme-mode-hook 'geiser-mode))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package smex
  :ensure t
  :defer t)

(use-package projectile
  :ensure t)

(use-package tagedit
  :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode 1))

(use-package magit
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package php-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t
)

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t
      :config
      ;; Sets up exec-path-from shell
      ;; https://github.com/purcell/exec-path-from-shell
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-envs
         '("PATH")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/customizations")

(load "setup-flycheck.el")

(load "navigation.el")
(load "ui.el")
(load "editing.el")
(load "misc.el")
(load "setup-magit.el")
;; Languages

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
