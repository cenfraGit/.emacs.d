(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

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
 )

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(delete-selection-mode t)

(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(set-frame-font "NanumGothicCoding Bold" nil t)

(load-file "~/.emacs.d/functions.el")
(global-set-key (kbd "C-c c") 'insert-comment-based-on-mode)
(global-set-key (kbd "C-c d") 'insert-text-comment)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------- package setup -------------------- ;;

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

;; ----------------------- packages ----------------------- ;;

;; (use-package doom-themes
;;   :config
;;   (let ((chosen-theme 'doom-material-dark))
;;     (doom-themes-visual-bell-config)
;;     (doom-themes-org-config)
;;     (setq doom-challenger-deep-brighter-comments t
;;           doom-challenger-deep-brighter-modeline t
;;           doom-rouge-brighter-comments t
;;           doom-ir-black-brighter-comments t
;;           modus-themes-org-blocks 'gray-background
;;           doom-dark+-blue-modeline nil)
;;     (load-theme chosen-theme)))

(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin t))


(use-package multiple-cursors)

;; ;; (setq package-check-signature nil)

;; (unless (package-installed-p 'lsp-mode)
;;   (package-refresh-contents)
;;   (package-install 'lsp-mode))

;; (unless (package-installed-p 'lsp-pyright)
;;   (package-refresh-contents)
;;   (package-install 'lsp-pyright))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((python-mode . lsp-deferred))
;;   :commands (lsp lsp-deferred)
;;   :config
;;   (setq lsp-prefer-flymake nil))

;; (use-package lsp-pyright
;;   :ensure t
;;   :after lsp-mode
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp-deferred))))

;; (setq lsp-log-io t)
;; (setq lsp-server-trace t)

;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)
;; (use-package company)
;; (use-package eldoc-box)

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))

;; (let ((installed (package-installed-p 'all-the-icons)))
;;   (use-package all-the-icons)
;;   (unless installed (all-the-icons-install-fonts)))

;; (use-package all-the-icons-dired
;;   :after all-the-icons
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-clients-clangd-args '("--compile-commands-dir=build"))
  )

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))


(setq lsp-clients-clangd-executable "/usr/bin/clangd")

;; ----------------------- commands ----------------------- ;;

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-c v") 'dired-sidebar-toggle-sidebar)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-lsp lsp-ui visual-regexp tree-sitter-langs multiple-cursors mood-line lsp-pyright eldoc-box doom-themes dired-sidebar company catppuccin-theme all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
