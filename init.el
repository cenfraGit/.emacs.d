(setq native-comp-speed 3)

(setq 
 default-directory "~/OneDrive/Desktop/"
 backup-directory-alist '(("." . "~/.emacs.d/.emacs-backups"))
 display-line-numbers-type 'relative
 inhibit-startup-message t
 initial-scratch-message nil
 use-short-answers t
 case-fold-search nil
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 ring-bell-function 'ignore
 visible-bell nil
 )

(when (window-system)
  (scroll-bar-mode -1)
  )

(load-file "~/.emacs.d/functions.el")
(global-set-key (kbd "C-c c") 'insert-comment-based-on-mode)
(global-set-key (kbd "C-;") 'comment-dwim)

(delete-selection-mode t)
(global-display-line-numbers-mode 1)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
