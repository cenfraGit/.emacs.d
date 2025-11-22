
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
;; (use-package eldoc-box)

;; (use-package visual-regexp
;;   :bind (("C-c 5" . #'vr/replace)))
(global-set-key (kbd "C-c 5") #'search-forward-regexp)

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t))

(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :defer t
  :config
  :bind (("C-c v" . treemacs)))

(package-install 'plantuml-mode)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-treemacs-theme "doom-atom") ; treemacs, use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

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
org-startup-with-inline-images t
 )
(setq-default message-log-max nil)
(setq-default fill-column 60) ; 100

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 30))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(cond
 ((eq system-type 'windows-nt)
  (setq default-directory (concat (getenv "USERPROFILE") "\\Desktop\\")))
 ((eq system-type 'gnu/linux)
  (setq default-directory (concat (getenv "HOME") "/Desktop/"))))
(put 'downcase-region 'disabled nil)

(setq exec-path (split-string (getenv "PATH") ";" t))

;; ------------------------------------------------------------
;; commands
;; ------------------------------------------------------------

(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(load custom-file 'noerror 'nomessage)
(scroll-bar-mode -1)
(tooltip-mode -1)
(delete-selection-mode t)
(kill-buffer "*Messages*")
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; functions and commands
(load-file "~/.emacs.d/functions.el")
(global-set-key (kbd "C-c c") 'my/insert-inline-comment-based-on-mode)
(global-set-key (kbd "C-c d") 'my/insert-block-comment)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; csharp
(defun csharp-mode-setup ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'csharp-mode-hook 'csharp-mode-setup)
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode))
(defun xml-mode-setup ()
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil))
(add-hook 'xml-mode-hook 'xaml-mode-setup)

;; latex
(add-hook 'latex-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c l") 'latex-insert-block)
	    (local-set-key (kbd "C-c p") 'latex-codeblock)))

;; encoding
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;; file updates
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; --------------------------------------------------------------------------------
;; appearance
;; --------------------------------------------------------------------------------

;; (add-to-list 'default-frame-alist '(font . "Courier-11"))
;; (add-to-list 'default-frame-alist '(font . "Cascadia Mono-11"))
(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium-10"))
;; (add-to-list 'default-frame-alist '(font . "Iosevka-11"))
;; (load-theme 'leuven t)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'modus-vivendi-tritanopia t)

(tool-bar-mode -1)
(menu-bar-mode -1)
