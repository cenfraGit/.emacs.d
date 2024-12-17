
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

;; ----------------------- commands ----------------------- ;;

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
