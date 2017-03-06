(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'diminish)
(require 'bind-key)

(use-package ox-gfm
  :ensure t)

(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :config
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (setq org-export-backends '(html beamer ascii latex md gfm)
        org-log-done 'time
        org-agenda-files (quote ("~/.org-files/npf-daily.org"
                                 "~/.org-files/npf-projects.org"
                                 "~/.org-files/coding-dojos.org"
                                 "~/.org-files/daily.org"
                                 "~/.org-files/journal.org"
                                 "~/.org-files/someday.org"
                                 "~/.org-files/gtd.org"))
        org-todo-keywords '((sequence "TODO(t)"
                                      "WIP(i!)"
                                      "WAIT(w!)" 
                                      "|" 
                                      "DONE(d!)" 
                                      "CANCELED(c!)"))
        org-directory "~/.org-files/"
        org-startup-indented t
        org-hide-emphasis-markers t))

(use-package company
  :ensure t
  :init 
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-dabbrev-downcase nil)
  :config 
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  :diminish company-mode)

(use-package paredit
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook 
                  lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook 'enable-paredit-mode))
  :diminish paredit-mode)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

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
  :config
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (setq geiser-default-implementation "mit"
        geiser-active-implementations '(mit)
        geiser-repl-query-on-kill-p  nil)
  (add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode)))

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :init
  (smex-initialize)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config 
  (setq projectile-completion-system 'helm))

(use-package tagedit
  :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode 1)
  :config 
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :diminish flycheck-mode)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)
  (setq git-gutter:update-interval 2)
  :diminish git-gutter-mode)

(use-package git-timemachine
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :config
  (customize-set-variable 'frame-background-mode 'dark)
  (load-theme 'solarized-dark t))

(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-save-file (concat user-emacs-directory ".recentf")
        recentf-max-menu-items 40))

(use-package saveplace
  :init (save-place-mode 1)
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")
        save-place-limit nil))

(use-package js-mode
  :mode 
  (("\\.json$" . js-mode)
   ("\\.js$" . js-mode))
  :config
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook 'subword-mode))

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

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  :diminish eldoc-mode)

(use-package php-mode
  :ensure t)

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :commands python-mode
  :init 
  (setq-default python-shell-interpreter "ipython"
                python-shell-interpreter-args "--deep-reload"
                python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
                python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                python-shell-completion-setup-code
                "from IPython.core.completerlib import module_completion"
                python-shell-completion-string-code
                "';'.join(get_ipython().Completer.all_completions('''%s'''))\n" )
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package anaconda-mode
  :ensure t
  :after python
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode 'eldoc-mode))

(use-package company-anaconda
  :ensure t
  :after anaconda-mode)

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (helm-mode t))
  :config
  (ido-mode -1)
  (setq helm-candidate-number-limit 100
        helm-idle-delay 0.1 
        helm-input-idle-delay 0.1 
        helm-quick-update t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-ff-auto-update-initial-value t
        helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t)
  :bind (("C-x b" . helm-mini)
         ("C-h a" . helm-apropos)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c SPC" . helm-all-mark-rings)
         ("C-x C-f" . helm-find-files)))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package linum
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'linum-mode))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-t") '(lambda () (interactive)))
(global-set-key (kbd "M-/") 'hippie-expand)
;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)

(global-hl-line-mode 1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-face-attribute 'default nil :height 150)
(setq ring-bell-function 'ignore
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-frame-alist '((top . 0) (left . 0) (width . 146) (height . 52))
      ns-right-alternate-modifier nil
      electric-indent-mode nil
      auto-save-default nil
      create-lockfiles nil)

(setq user-full-name "Raúl Araya"
      user-mail-address "nubeiro@gmail.com")

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(setq-default indent-tabs-mode nil
              frame-title-format "%b (%f)"
              sh-basic-offset 2
              sh-indentation 2
              initial-scratch-message (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
              
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'html-mode-hook 'subword-mode)
(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(max-lisp-eval-depth 1800)
 '(max-specpdl-size 1800)
 '(package-selected-packages
   (quote
    (helm-projectile yaml-mode use-package tagedit solarized-theme smex rainbow-delimiters projectile paredit ox-gfm org markdown-mode magit ido-ubiquitous helm git-timemachine git-gutter geiser flycheck exec-path-from-shell company-jedi company-anaconda color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
