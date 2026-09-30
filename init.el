;--------------------------------------------------------------------------------
; init.el
; emacs config
;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
; variables
;--------------------------------------------------------------------------------

(setq-default
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 global-auto-revert-non-file-buffers t
 auto-revert-verbose nil

 line-spacing 0.2
 display-line-numbers-type 'relative
 truncate-lines t
 tab-bar-tab-hints t

 indent-tabs-mode nil
 fill-column 70
 tab-width 4
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
 message-log-max 1000

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

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu"  . "https://elpa.gnu.org/packages/")))

;; init.el is byte-compiled, which expands every use-package form ahead of
;; time. only bind-key is needed at runtime, for :bind.
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure nil)

;; :ensure t loads all of package.el just to confirm a package is installed.
;; only fall through to it when the package really is missing.
(setq use-package-ensure-function
      (lambda (name &rest args)
        (unless (memq name package-activated-list)
          (apply #'use-package-ensure-elpa name args))))

;------------------------------------------------------------ dired

(use-package dired
  :ensure nil
  :defer t
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
  (company-minimum-prefix-length 2)
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
  :bind ("<f9>" . olivetti-mode)
  :custom
  (olivetti-body-width 150)
)

;------------------------------------------------------------ magit

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
)

;------------------------------------------------------------ project

(use-package project
  :ensure nil
  :defer t
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-find-regexp "Find regexp")
     (project-dired "Dired")
     (project-compile "Compile")
     (project-switch-to-buffer "Buffer")))
)

;; windows resolves a bare "find" to the System32 find.exe, which is a text
;; search tool, not gnu find - project.el shells out to it and fails with
;; "FIND: Parameter format not correct".  the path must not contain spaces,
;; because project.el interpolates it into a shell command unquoted.
(when (eq system-type 'windows-nt)
  (when-let* ((gnu-find (seq-find #'file-exists-p
                                  '("C:/msys64/usr/bin/find.exe"
                                    "C:/PROGRA~1/Git/usr/bin/find.exe"))))
    (setq find-program gnu-find)))

(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p s") #'project-find-regexp)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p d") #'project-dired)
(global-set-key (kbd "C-c p c") #'project-compile)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)

;------------------------------------------------------------ cslite

(use-package eglot
  :ensure nil
  :defer t
  :hook ((csharp-mode csharp-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 2000 :format short))
  (eldoc-echo-area-use-multiline-p t)
  :config
  (require 'cslite)
  (cslite-setup)
)

;; cslite finds c# project roots outside git, so hook it in without loading eglot
(with-eval-after-load 'project
  (autoload 'cslite-project-root "cslite")
  (add-hook 'project-find-functions #'cslite-project-root t))

;------------------------------------------------------------ flymake

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("C-c e n" . flymake-goto-next-error)
              ("C-c e p" . flymake-goto-prev-error)
              ("C-c e l" . flymake-show-buffer-diagnostics))
)

(global-set-key (kbd "C-c l r") #'cslite-restart)
(global-set-key (kbd "C-c l e") #'eglot-events-buffer)
;; Windows will not let you overwrite dist/cslite.dll while the server is
;; running, so stop it before republishing a new build.
(global-set-key (kbd "C-c l s") #'eglot-shutdown)

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
(global-whitespace-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(delete-selection-mode t)

;; pause after a prefix like C-c d to see every key under it
(which-key-mode 1)
(which-key-add-key-based-replacements
  "C-c c" "comment"
  "C-c d" "dotnet"
  "C-c e" "errors"
  "C-c h" "notes"
  "C-c l" "lsp"
  "C-c p" "project")
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

(defun my/toggle-dark-mode ()
  (interactive)
  (let ((local-file (expand-file-name "local-config.el" user-emacs-directory)))
    (if (memq 'doom-Iosvkem custom-enabled-themes)
        (progn
          (disable-theme 'doom-Iosvkem)
          (tool-bar-mode 1)
          (menu-bar-mode 1)
          (when (file-exists-p local-file) (delete-file local-file)))
      (load-theme 'doom-Iosvkem t)
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (with-temp-file local-file
        (insert "(load-theme 'doom-Iosvkem t)\n(tool-bar-mode -1)\n(menu-bar-mode -1)\n")))))

(let ((local-config (expand-file-name "local-config.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config)))

;------------------------------------------------------------ misc

(add-hook 'prog-mode-hook 'hs-minor-mode)

;; wrap prose, truncate code. nxml and html derive from text-mode but are code.
(add-hook 'text-mode-hook
          (lambda ()
            (unless (derived-mode-p 'nxml-mode 'sgml-mode)
              (visual-line-mode 1))))

(add-hook 'eshell-mode-hook (lambda () (company-mode -1)))

;; turn color escape codes in compilation output into actual colors
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(add-to-list 'auto-mode-alist '("\\.\\(xaml\\|axaml\\)\\'" . nxml-mode))

(global-set-key (kbd "C-c j") 'hs-toggle-hiding)

;; keep init.el and lisp/ compiled: recompile any .el that already has an .elc
(add-hook 'after-save-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-suffix-p ".el" buffer-file-name)
                       (file-exists-p (concat buffer-file-name "c")))
              (byte-compile-file buffer-file-name))))
