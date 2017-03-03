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

(use-package ox-gfm
  :ensure t)

(use-package org
  :ensure t
  :mode ("\\.org$" . org-mode)
  :config
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-export-backends '(html beamer ascii latex md gfm)))

(use-package company
  :ensure t
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
  :config
  (dolist (hook '(emacs-lisp-mode-hook 
                  lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  lisp-interaction-mode-hook))
    (add-hook hook 'enable-paredit-mode))
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
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1
        ido-use-virtual-buffers t)
  (ido-mode +1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode +1))

(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :init
  (smex-initialize)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package projectile
  :ensure t
  :init (projectile-global-mode))

(use-package tagedit
  :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode 1)
  :diminish flycheck-mode)

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :defer t
  :init
  (global-git-gutter-mode t)
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
  (progn
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "places")
          save-place-limit nil)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html


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
(global-linum-mode)
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

(setq-default indent-tabs-mode nil)
(setq-default frame-title-format "%b (%f)")
;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

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
 '(frame-background-mode (quote dark))
 '(max-lisp-eval-depth 1800)
 '(max-specpdl-size 1800))
(custom-set-faces)
