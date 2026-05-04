;--------------------------------------------------------------------------------
; init.el
; emacs config
;--------------------------------------------------------------------------------

(global-set-key (kbd "C-;") 'comment-dwim)

;--------------------------------------------------------------------------------
; variables
;--------------------------------------------------------------------------------

(setq-default
 default-directory "~/"
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 backup-directory-alist '(("." . "~/.emacs.d/.emacs-backups"))
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 cursor-type 'bar
 line-spacing 0.2
 display-line-numbers-type 'relative
 truncate-lines t
 tab-bar-tab-hints t
 ring-bell-function 'ignore
 visible-bell t

 indent-tabs-mode nil
 fill-column 130
 tab-width 4
 indent-tabs-mode nil
 whitespace-line-column 130
 case-fold-search nil
 mode-require-final-newline nil
 whitespace-style '(face tabs trailing empty tab-mark)

 use-dialog-box nil
 use-short-answers t
 custom-safe-themes t
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 native-comp-speed 3
 native-comp-async-report-warnings-errors nil
 inhibit-startup-message t
 initial-scratch-message nil
 message-log-max nil

 org-startup-with-inline-images t
 nxml-child-indent 4
 nxml-attribute-indent 4
 c-basic-offset 4
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 )

(cond
 ((eq system-type 'windows-nt)
  (setq default-directory (concat (getenv "USERPROFILE") "\\Desktop\\")))
 ((eq system-type 'gnu/linux)
  (setq default-directory (concat (getenv "HOME") "/Desktop/"))))
(put 'downcase-region 'disabled nil)

;--------------------------------------------------------------------------------
; packages
;--------------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil)
(setq use-package-compute-statistics t)

; :init -> for variables needed before package loads
; :config -> hooks, keybindings, funciton calls AFTER package loads
; :custom -> user options (variables) set AFTER package loads

;------------------------------------------------------------ dired

(use-package dired
  :ensure nil
  :config
  (setq delete-by-moving-to-trash t)
  (eval-after-load "dired"
    #'(lambda ()
        (put 'dired-find-alternate-file 'disabled nil)
        (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)))
)

;------------------------------------------------------------ multiple-cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C-c x" . mc/edit-lines)
         ("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
)

;------------------------------------------------------------ company

(use-package company
  :ensure t
  :defer 2
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  :config
  (global-company-mode 1)
)

;------------------------------------------------------------ treemacs

(use-package treemacs
  :ensure t
  :defer t
  :init (setq treemacs-width 50)
  :bind (("C-c v" . treemacs))
)

(with-eval-after-load 'treemacs
  (add-to-list 'treemacs-ignored-file-predicates 'my-treemacs-ignore-hidden-dirs)
  (defun my-treemacs-ignore-hidden-dirs (filename absolute-path)
    (or (string-match-p "/bin" absolute-path)
        (string-match-p "/obj" absolute-path)
        (string-match-p "/.vs" absolute-path)
        ))
)

;------------------------------------------------------------ doom-themes

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-Iosvkem")
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
)

;------------------------------------------------------------ vertico

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
)

(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (setq vertico-posframe-width 100)
  (setq vertico-posframe-height 12)
  (setq vertico-posframe-border-width 2)
  (setq vertico-posframe-hide-minibuffer t))

;------------------------------------------------------------ savehist

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
)

;------------------------------------------------------------ orderless

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
)

;------------------------------------------------------------ marginalia

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
)

;------------------------------------------------------------ consult

(use-package consult
  :ensure t
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line))
)

;------------------------------------------------------------ olivetti

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100)
)

(global-set-key (kbd "<f9>") 'olivetti-mode)

;------------------------------------------------------------ magit

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
)

;------------------------------------------------------------ treesit

(use-package treesit
  :ensure nil
)

(setq treesit-language-source-alist
      '((c-sharp
         "https://github.com/tree-sitter/tree-sitter-c-sharp"
         "v0.23.1"))) ;; this version compiles to ABI 14

;------------------------------------------------------------ project

(use-package project
  :ensure nil
  ;; :bind-keymap
  ;; ("C-c p" . project-prefix-map)
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-dired "Dired")
     (project-compile "Compile")
     (project-switch-to-buffer "Buffer")))
)

(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p s") #'project-find-regexp)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p d") #'project-dired)
(global-set-key (kbd "C-c p c") #'project-compile)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)

;------------------------------------------------------------ eldoc

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.3)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-echo-area-display-truncation-message nil))

;------------------------------------------------------------ eglot

(use-package eglot
  :ensure nil
  :hook ((csharp-ts-mode . eglot-ensure)
         (csharp-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((csharp-ts-mode csharp-mode) . ("csharp-ls")))
  :hook ((eglot-managed-mode . my/eglot-mode-hook-fn))
  :config
  (defun my/eglot-mode-hook-fn ()
    (eglot-inlay-hints-mode 0))
)

(global-set-key (kbd "C-c l l") #'eglot)
(global-set-key (kbd "C-c l q") #'eglot-shutdown)
(global-set-key (kbd "C-c l r") #'eglot-rename)
(global-set-key (kbd "C-c l a") #'eglot-code-actions)
(global-set-key (kbd "C-c l f") #'eglot-format-buffer)
(global-set-key (kbd "C-c l h") #'eldoc-doc-buffer)
(global-set-key (kbd "C-c l d") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c l D") #'flymake-show-project-diagnostics)

;------------------------------------------------------------ corfu

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  :init
  (global-corfu-mode 1)
  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;------------------------------------------------------------ indent-bars

(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode)
)

;------------------------------------------------------------ yasnippet

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
)

;------------------------------------------------------------ nxml-mode

(use-package nxml-mode
  :ensure nil
  :hook ((nxml-mode . hs-minor-mode)
         (nxml-mode . indent-bars-mode))
  :config
  (require 'hideshow)
  (require 'sgml-mode)
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil))
)

;--------------------------------------------------------------------------------
; minor modes
;--------------------------------------------------------------------------------

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-whitespace-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode)
(scroll-bar-mode -1)
(delete-selection-mode t)
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;--------------------------------------------------------------------------------
; general
;--------------------------------------------------------------------------------

;------------------------------------------------------------ encoding

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

;------------------------------------------------------------ user functions

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'comment-functions)
(require 'csharp-functions)
(require 'dotnet-functions)
(require 'notes-functions)

;------------------------------------------------------------ appearance

;; (add-to-list 'default-frame-alist '(font . "Lucida Console-10"))
(add-to-list 'default-frame-alist '(font . "Cascadia Code-10"))
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 33))

(load-theme 'doom-dark+ t)
;; (load-theme 'leuven t)

;------------------------------------------------------------ misc

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

(add-to-list 'major-mode-remap-alist '(csharp-mode . csharp-ts-mode))

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.\\(xaml\\|axaml\\)\\'" . nxml-mode))

(global-set-key (kbd "C-c j") 'hs-toggle-hiding)

(load custom-file 'noerror 'nomessage)
(let ((buffer-name "*Messages*"))
 (when (get-buffer buffer-name)
   (kill-buffer buffer-name)))

;------------------------------------------------------------ cleanup

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq gc-cons-percentage 0.1)
            (message "Emacs loaded in %s with %d garbage collections."
                     (emacs-init-time)
                     gcs-done))
)