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

 line-spacing 0.2
 display-line-numbers-type 'relative
 truncate-lines t
 tab-bar-tab-hints t
 ring-bell-function 'ignore
 visible-bell t

 indent-tabs-mode nil
 fill-column 70
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

;------------------------------------------------------------ doom-themes

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
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
  (olivetti-body-width 150)
)

(global-set-key (kbd "<f9>") 'olivetti-mode)

;------------------------------------------------------------ magit

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
)

;------------------------------------------------------------ project

(use-package project
  :ensure nil
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

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "vmd"))

;--------------------------------------------------------------------------------
; minor modes
;--------------------------------------------------------------------------------

;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
(global-whitespace-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(global-visual-line-mode 1)
(delete-selection-mode t)
(tooltip-mode -1)

;; (setq-default font-lock-mode nil)
;; (advice-add 'font-lock-mode :before-until (lambda (&rest _) t))

;--------------------------------------------------------------------------------
; general
;--------------------------------------------------------------------------------

;------------------------------------------------------------ encoding

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

;------------------------------------------------------------ user functions

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'comment-functions)
(require 'csharp-functions)
(require 'dotnet-functions)
(require 'notes-functions)

;------------------------------------------------------------ appearance

(cond
 ((eq system-type 'windows-nt)
  (when-let ((user-profile (getenv "USERPROFILE")))
    (setq default-directory (expand-file-name "Desktop/" user-profile)))
  )
 ((eq system-type 'gnu/linux)
  (when-let ((home (getenv "HOME")))
    (setq default-directory (expand-file-name "Desktop/" home))))
)

(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 33))

(defun my/toggle-dark-mode ()
  (interactive)
  (let ((local-file (expand-file-name "local-config.el" user-emacs-directory)))
    (if (memq 'doom-moonlight custom-enabled-themes)
        (progn
          (disable-theme 'doom-moonlight)
          (tool-bar-mode 1)
          (menu-bar-mode 1)
          (when (file-exists-p local-file) (delete-file local-file)))
      (load-theme 'doom-moonlight t)
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (with-temp-file local-file
        (insert "(load-theme 'doom-moonlight t)\n(tool-bar-mode -1)\n(menu-bar-mode -1)\n")))))

(let ((local-config (expand-file-name "local-config.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config)))

;------------------------------------------------------------ misc

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.\\(xaml\\|axaml\\)\\'" . nxml-mode))

(global-set-key (kbd "C-c j") 'hs-toggle-hiding)
