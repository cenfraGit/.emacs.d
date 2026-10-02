;;; -*- lexical-binding: t -*-
;--------------------------------------------------------------------------------
; home-functions.el
; startup buffer with known projects and recent files
;--------------------------------------------------------------------------------

(require 'notes-functions)

(defvar my/home-buffer-name "*home*")
(defvar my/home-recent-count 10)

;------------------------------------------------------------ internals

(defun my/home--read-file (file)
  "return the first lisp form in FILE, nil if it's missing or unreadable"
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (ignore-errors (read (current-buffer)))))
)

;; at startup these read the save files directly. loading project.el and
;; recentf just for the lists costs ~100ms each here.

(defun my/home--projects ()
  (if (featurep 'project)
      (project-known-project-roots)
    (mapcar #'car (my/home--read-file (locate-user-emacs-file "projects"))))
)

(defun my/home--recent-files ()
  (if (bound-and-true-p recentf-mode)
      recentf-list
    ;; the file is (setq recentf-list '(...))
    (let* ((form (my/home--read-file (locate-user-emacs-file "recentf")))
           (value (nth 2 form)))
      (and (eq (car-safe form) 'setq)
           (eq (nth 1 form) 'recentf-list)
           (eq (car-safe value) 'quote)
           (cadr value))))
)

(defun my/home--section (title key items action)
  "insert TITLE and one button per item in ITEMS, calling ACTION with it"
  (insert (propertize title 'face 'bold)
          (propertize (format "  [%s]" key) 'face 'shadow)
          "\n")
  (if (null items)
      (insert "  none yet\n")
    (dolist (item items)
      (insert "  ")
      (insert-text-button (abbreviate-file-name item)
                          'action (lambda (_) (funcall action item))
                          'follow-link t)
      (insert "\n")))
  (insert "\n")
)

(defun my/home--render (&rest _)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; the same logo as the default emacs startup screen
    (when (display-images-p)
      ;; full size takes half the window and pushes the lists off screen
      (insert-image (create-image (fancy-splash-image-file) nil nil :scale 0.6))
      (insert "\n\n"))
    (insert (propertize (format "started in %s" (emacs-init-time "%.2fs")) 'face 'shadow)
            "\n\n")
    ;; shown even before the file exists, my/open-todo starts it
    (my/home--section "Todo" "t" (list (expand-file-name "todo.org" user-emacs-directory))
                      (lambda (_) (my/open-todo)))
    (my/home--section "Projects" "p" (my/home--projects) #'dired)
    (my/home--section "Recent files" "r"
                      (seq-take (my/home--recent-files) my/home-recent-count)
                      #'find-file)
    (insert (propertize "RET open  TAB next  s scratch  t todo  p projects  r recent  g refresh  q close"
                        'face 'shadow))
    (goto-char (point-min))
    (forward-button 1 nil nil t))
)

(defun my/home--jump (title)
  (goto-char (point-min))
  (when (search-forward title nil t)
    (forward-button 1 nil nil t))
)

;------------------------------------------------------------ mode

(defvar-keymap my/home-mode-map
  :parent special-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  ;; built in, recreates *scratch* if it was killed
  "s" #'scratch-buffer
  "t" (lambda () (interactive) (my/home--jump "Todo"))
  "p" (lambda () (interactive) (my/home--jump "Projects"))
  "r" (lambda () (interactive) (my/home--jump "Recent files")))

(define-derived-mode my/home-mode special-mode "Home"
  "startup buffer with known projects and recent files"
  (setq-local revert-buffer-function #'my/home--render)
)

(defun my/home ()
  "show the home buffer, refreshed"
  (interactive)
  (let ((buffer (get-buffer-create my/home-buffer-name)))
    (with-current-buffer buffer
      (my/home-mode)
      ;; global-display-line-numbers-mode turns them on as the mode starts
      (display-line-numbers-mode -1)
      (my/home--render))
    (when (called-interactively-p 'any)
      (switch-to-buffer buffer))
    buffer)
)

(defun my/home-startup ()
  "for `initial-buffer-choice': the home buffer, unless emacs was started with a file"
  ;; files on the command line are already open by the time this runs
  (or (seq-find #'buffer-file-name (buffer-list))
      (my/home))
)

;------------------------------------------------------------ keybindings

;; C-c h h shows it in the current window
(global-set-key (kbd "C-c h h") #'my/home)

;; new tabs (C-x t 2) open on it too
(setq tab-bar-new-tab-choice #'my/home)

(provide 'home-functions)
