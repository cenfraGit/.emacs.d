;;; -*- lexical-binding: t -*-
;--------------------------------------------------------------------------------
; early-init.el
; runs before the first frame exists, so frame settings here avoid a redraw
;--------------------------------------------------------------------------------

;; skip gc during startup, restored once init is done
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; file lookups under the user profile are slow on this machine (~0.5ms each)
;; and every require tries 6 suffixes in every load-path dir. only look for
;; .elc/.el. windows keeps it that way: built-in sources here aren't .gz and
;; no .dll modules are used. elsewhere restore after startup, since distros
;; often ship .el.gz sources.
(let ((suffixes load-suffixes)
      (reps load-file-rep-suffixes))
  (setq load-suffixes '(".elc" ".el")
        load-file-rep-suffixes '(""))
  (unless (eq system-type 'windows-nt)
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq load-suffixes suffixes
                      load-file-rep-suffixes reps)))))

(setq frame-inhibit-implied-resize t)

(push '(width . 120) default-frame-alist)
(push '(height . 33) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (eq system-type 'windows-nt)
  (push '(font . "Consolas-10") default-frame-alist))

;; local-config.el only exists while dark mode is on (see my/toggle-dark-mode).
;; colors match doom-Iosvkem so the first frame doesn't flash white.
(when (file-exists-p (expand-file-name "local-config.el" user-emacs-directory))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(background-color . "#1b1d1e") initial-frame-alist)
  (push '(foreground-color . "#dddddd") initial-frame-alist))

;; loads one pre-built autoloads file instead of package.el and every
;; package's own autoloads. package.el regenerates it on install/delete,
;; M-x package-quickstart-refresh if it ever gets stale.
(setq package-quickstart t)

;; init.el and lisp/ are byte-compiled. if a .el was edited outside emacs and
;; its .elc is stale, load the .el instead of the old .elc.
(setq load-prefer-newer t)
