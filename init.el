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

(require 'use-package)
(setq use-package-always-ensure t)

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
(use-package multiple-cursors)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)
(delete-selection-mode t)

(set-frame-font "Cascadia Code 11" nil t)
(add-to-list 'default-frame-alist '(width . 70))
(add-to-list 'default-frame-alist '(height . 25))

(load-file "~/.emacs.d/functions.el")
(global-set-key (kbd "C-c c") 'insert-comment-based-on-mode)
(global-set-key (kbd "C-;") 'comment-dwim)
(global-set-key (kbd "C-c x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun load-setup()
  (interactive)
  (load-file "~/.emacs.d/setup.el"))

