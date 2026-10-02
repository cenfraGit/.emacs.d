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
 ;; after C-u C-SPC jumps to the last mark, each C-SPC keeps going back
 set-mark-command-repeat-pop t
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
  ;; loaded while idle, see "idle loading" below
  :defer t
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

;------------------------------------------------------------ recentf

;; recent files also show up in C-x b (f SPC narrows to them)
(use-package recentf
  :ensure nil
  :defer t
  :init
  (setq recentf-max-saved-items 100
        ;; the default checks every file at startup, which is slow on this
        ;; machine. drop deleted files after 5 minutes idle instead.
        recentf-auto-cleanup 300)
  ;; loading it costs ~100ms here, so wait until emacs is idle
  (run-with-idle-timer 1 nil #'recentf-mode 1)
)

;------------------------------------------------------------ org

;; the default modules add links to gnus, irc, docview and others. gnus alone
;; took 7s to load on the first .org file.
(setq org-modules nil)

;------------------------------------------------------------ idle loading

;; company (~0.9s), the c# stack (~1.9s) and org (~2-3s) are slow to load here.
;; load them a piece at a time while idle, so the first use is fast and no
;; single pause is noticeable. an item is a feature to require, or a function
;; to call. typing just delays the next piece until the next pause.
;; not named `features': init.el binds dynamically, and that would hide the
;; global list `require' checks, so nothing would load
(defun my/idle-load (pending)
  (when pending
    (let ((item (car pending)))
      (if (symbolp item) (require item nil t) (funcall item)))
    ;; an idle timer set while already idle needs the current idle time added,
    ;; or it waits for the next idle period
    (run-with-idle-timer (time-add (current-idle-time) 0.1) nil
                         #'my/idle-load (cdr pending))))

(run-with-idle-timer
 2 nil #'my/idle-load
 `(;; company turns on global-company-mode in its :config once loaded
   bytecomp xref etags company
   compile cc-mode treesit eieio auth-source flymake jsonrpc pp ewoc ert diff-mode
   track-changes eglot cslite csharp-mode
   calendar org-macs org-compat org-keys org-fold ol org-table ob-core org-list
   org-src ob org
   ;; first org-mode setup loads even more, do it once in a hidden buffer
   ,(lambda () (with-temp-buffer (org-mode)))))

;------------------------------------------------------------ ibuffer

(global-set-key (kbd "C-x C-b") #'ibuffer)

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
         ("M-g g" . consult-goto-line)
         ;; every mark in this buffer / in all buffers, with live preview
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark))
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
(global-set-key (kbd "C-c l n") #'eglot-rename)
(global-set-key (kbd "C-c l a") #'eglot-code-actions)
(global-set-key (kbd "C-c l f") #'eglot-format-buffer)

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
  ;; gfm input so tables and task lists render. markdown-mode adds the html
  ;; header around pandoc's output itself.
  :init (setq markdown-command "pandoc -f gfm"
              markdown-fontify-code-blocks-natively t
              ;; github-like look for the browser preview
              markdown-xhtml-header-content "<style>
body { max-width: 860px; margin: 2em auto; padding: 0 1em;
       font: 16px/1.6 -apple-system, 'Segoe UI', Helvetica, Arial, sans-serif; color: #1f2328; }
h1, h2 { border-bottom: 1px solid #d1d9e0; padding-bottom: .3em; }
a { color: #0969da; }
code { font-family: Consolas, monospace; background: #eff1f3; padding: .2em .4em; border-radius: 6px; }
pre { background: #f6f8fa; padding: 1em; border-radius: 6px; overflow: auto; }
pre code { background: none; padding: 0; }
table { border-collapse: collapse; }
th, td { border: 1px solid #d1d9e0; padding: 6px 13px; }
tr:nth-child(even) { background: #f6f8fa; }
blockquote { margin: 0; padding: 0 1em; color: #59636e; border-left: .25em solid #d1d9e0; }
ul.task-list, ul:has(> li > input) { list-style: none; padding-left: 1.2em; }
img { max-width: 100%; }
</style>")
  :config
  ;; both leave an .html next to the file. C-c C-c p uses a temp file instead.
  (keymap-unset markdown-mode-command-map "l" t)
  (keymap-unset markdown-mode-command-map "v" t))

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
(require 'home-functions)

;; start on the home buffer (projects and recent files), C-c h h reopens it
(setq initial-buffer-choice #'my/home-startup)

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

;; vc runs git on every file open (~0.5s here) to show the branch in the mode
;; line. magit covers git, and project.el still finds repos without it.
(remove-hook 'find-file-hook #'vc-refresh-state)

(add-to-list 'auto-mode-alist '("\\.\\(xaml\\|axaml\\|csproj\\)\\'" . nxml-mode))

;; folds #region blocks too, see csharp-functions.el
(global-set-key (kbd "C-c j") #'my/toggle-fold)

;; ascii drawing, C-c C-c leaves it
(global-set-key (kbd "C-c a") #'artist-mode)

;; keep init.el and lisp/ compiled: recompile any .el that already has an .elc
(add-hook 'after-save-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-suffix-p ".el" buffer-file-name)
                       (file-exists-p (concat buffer-file-name "c")))
              (byte-compile-file buffer-file-name))))
