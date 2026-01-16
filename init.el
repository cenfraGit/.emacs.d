;; --------------------------------------------------------------------------------
;; package setup
;; --------------------------------------------------------------------------------

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
(setq package-check-signature nil)

(unless (package-installed-p 'gnu-elpa-keyring-update)
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update))

(unless package-archive-contents
  (package-refresh-contents))

;; --------------------------------------------------------------------------------
;; packages
;; --------------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))))

(use-package multiple-cursors)

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :defer t
  :config
  :bind (("C-c v" . treemacs)))
(setq treemacs-width 42)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-Iosvkem")
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((csharp-mode . lsp-deferred))
  :commands lsp lsp-deferred)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  :hook (lsp-mode . lsp-ui-mode))

;; --------------------------------------------------------------------------------
;; variables
;; --------------------------------------------------------------------------------

(setq-default
 native-comp-speed 3
 native-comp-async-report-warnings-errors nil
 inhibit-startup-message t
 initial-scratch-message nil
 ring-bell-function 'ignore
 visible-bell t
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
 org-startup-with-inline-images t
 message-log-max nil
 fill-column 130
 whitespace-line-column 130
 indent-tabs-mode nil
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil
 nxml-child-indent 4
 nxml-attribute-indent 4
 lsp-enable-file-watchers t
 lsp-file-watch-threshold nil
 )

(cond
 ((eq system-type 'windows-nt)
  (setq default-directory (concat (getenv "USERPROFILE") "\\Desktop\\")))
 ((eq system-type 'gnu/linux)
  (setq default-directory (concat (getenv "HOME") "/Desktop/"))))
(put 'downcase-region 'disabled nil)

(setq exec-path (split-string (getenv "PATH") ";" t))

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 30))

(add-to-list 'custom-theme-load-path (expand-file-name
                                      "themes"
                                      user-emacs-directory))

;; --------------------------------------------------------------------------------
;; commands
;; --------------------------------------------------------------------------------

(load custom-file 'noerror 'nomessage)
(let ((buffer-name "*Messages*"))
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name)))

;; modes
;; (global-whitespace-mode 1)
;; (setq whitespace-style (delq 'lines-tail (delq 'lines whitespace-style)))
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)

;; encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; functions and commands
(load-file "~/.emacs.d/functions.el")
(global-set-key (kbd "C-c c") 'my/insert-inline-comment-based-on-mode)
(global-set-key (kbd "C-c d") 'my/insert-block-comment)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c 5") #'search-forward-regexp)

;; csharp
(defun csharp-mode-setup ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))

;; --------------------------------------------------------------------------------
;; hooks
;; --------------------------------------------------------------------------------

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

(add-hook 'csharp-mode-hook 'csharp-mode-setup)
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

(add-hook 'latex-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'latex-insert-block)
            (local-set-key (kbd "C-c p") 'latex-codeblock)))

;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------

(load-theme 'doom-acario-dark t)
;; (load-theme 'doom-Iosvkem t)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'leuven t)

;; (add-to-list 'default-frame-alist '(font . "Courier-10"))
(add-to-list 'default-frame-alist '(font . "Lucida Console-10"))
;; (add-to-list 'default-frame-alist '(font . "Iosevka-10"))

;; --------------------------------------------------------------------------------
;; start in org mode (daemon)
;; --------------------------------------------------------------------------------

(org-mode)
