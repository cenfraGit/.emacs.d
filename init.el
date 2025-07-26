
;; ------------------------------------------------------------
;; package setup
;; ------------------------------------------------------------

(require 'use-package)
(setq use-package-always-ensure t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ------------------------------------------------------------
;; packages
;; ------------------------------------------------------------

(setq package-check-signature nil)

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

;; Load the theme
(load-theme 'spacemacs-dark t) ;; or spacemacs-light




(use-package multiple-cursors)
;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)
;; (use-package eldoc-box)

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package neotree
  :ensure t
  :bind (("C-c v" . neotree-toggle))
  :config
  (setq neo-smart-open t))

;; (use-package company
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

;; (use-package visual-regexp
;;   :bind (("C-c 5" . #'vr/replace)))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((python-mode . lsp-deferred)
;;          (c++-mode . lsp-deferred)
;;          (c-mode . lsp-deferred))
;;   :commands (lsp lsp-deferred)
;;   :config
;;   (setq lsp-prefer-flymake nil)
;;   (setq lsp-clients-clangd-args '("--compile-commands-dir=build"))
;;   (setq lsp-clients-clangd-executable
;;         (if (eq system-type 'windows-nt)
;;             "C:/Program Files/LLVM/bin/clangd.exe"
;;           "/usr/bin/clangd")))

;; (use-package lsp-pyright
;;   :ensure t
;;   :after lsp-mode
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp-deferred))))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; ------------------------------------------------------------
;; theme
;; ------------------------------------------------------------

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
;;(load-theme 'masked t)

(set-face-attribute 'line-number nil
                    :foreground "#6082B6")

(set-face-attribute 'line-number-current-line nil
                    :foreground "#00A36C"
                    :weight 'bold)

;; ------------------------------------------------------------
;; variables
;; ------------------------------------------------------------

(setq
 native-comp-speed 3
 native-comp-async-report-warnings-errors nil
 inhibit-startup-message t
 initial-scratch-message nil
 ring-bell-function 'ignore
 visible-bell nil
 use-dialog-box nil
 use-short-answers t
 case-fold-search nil
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 custom-safe-themes t
 backup-directory-alist '(("." . "~/.emacs.d/.emacs-backups"))
 default-directory "~/"
 display-line-numbers-type 'relative
 indent-tabs-mode nil
 truncate-lines t
 tab-bar-tab-hints t
 neo-show-hidden-files t
 neo-window-fixed-size nil
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 )
(setq-default message-log-max nil)
(setq-default fill-column 100)

;; ------------------------------------------------------------
;; commands
;; ------------------------------------------------------------

(tool-bar-mode -1)
(menu-bar-mode -1)
;; (setq default-frame-alist '((font . "Courier-11")))
(setq default-frame-alist '((font . "Source Code Pro-11")))
(global-display-line-numbers-mode 1)
(load custom-file 'noerror 'nomessage)
(scroll-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode t)
(kill-buffer "*Messages*")
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;(global-tree-sitter-mode)
(load-file "~/.emacs.d/functions.el")
(global-set-key (kbd "C-c c") 'insert-comment-based-on-mode)
(global-set-key (kbd "C-c d") 'insert-text-comment)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ------------------------------------------------------------
;; encoding
;; ------------------------------------------------------------

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(cond
 ((eq system-type 'windows-nt)
  (setq default-directory (concat (getenv "USERPROFILE") "\\Desktop\\")))
 ((eq system-type 'gnu/linux)
  (setq default-directory (concat (getenv "HOME") "/Desktop/"))))

