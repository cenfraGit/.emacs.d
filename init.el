;; (server-start)
(setq native-comp-speed 3)

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

;; ---------------------- variables ---------------------- ;;

(setq
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
 ;; default-directory "~/OneDrive/Desktop/"
 display-line-numbers-type 'relative
 )

(setq-default indent-tabs-mode nil)
;; (setq-default cursor-type 'bar)
(setq-default truncate-lines t)

;; ---------------------- lsp setup ---------------------- ;;

(setq package-check-signature nil)

(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; ---------------- package configuration ---------------- ;;

(use-package tree-sitter)
(use-package tree-sitter-langs)
(use-package company)
(use-package eldoc-box)
(use-package multiple-cursors)

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))

(let ((installed (package-installed-p 'all-the-icons)))
  (use-package all-the-icons)
  (unless installed (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-material-dark))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t
          doom-rouge-brighter-comments t
          doom-ir-black-brighter-comments t
          modus-themes-org-blocks 'gray-background
          doom-dark+-blue-modeline nil)
    (load-theme chosen-theme)))

(use-package mood-line
  :config
  (defun pt/mood-line-segment-project-advice (oldfun)
    (let
        ((project-relative (ignore-errors (pt/project-relative-file-name nil))))
         (if
             (and (project-current) (not org-src-mode) project-relative)
             (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
           (funcall oldfun))))

  (advice-add 'mood-line-segment-buffer-name :around #'pt/mood-line-segment-project-advice)
  (mood-line-mode))

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Emacs"
        dashboard-items nil
        dashboard-set-init-info nil
        dashboard-set-footer-messages nil
        dashboard-footer-messages '("")))

;; ----------------------- commands ----------------------- ;;

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(delete-selection-mode t)
(global-display-line-numbers-mode 1)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ---------------------- appearance ---------------------- ;;

(set-frame-font "Cascadia Code 11" nil t)
;(add-to-list 'default-frame-alist '(width . 100))
;(add-to-list 'default-frame-alist '(height . 30))

;; ----------------------- encoding ----------------------- ;;

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; -------------- functions and keybindings -------------- ;;

(load-file "~/.emacs.d/functions.el")

(global-set-key (kbd "C-c c") 'insert-comment-based-on-mode)
(global-set-key (kbd "C-c v") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
