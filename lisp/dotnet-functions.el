;--------------------------------------------------------------------------------
; dotnet-functions.el
; functions related with dotnet cli
;--------------------------------------------------------------------------------

;------------------------------------------------------------ internals

(defun my/dotnet--project-root ()
  "return current project root"
  (project-root (project-current t))
)

(defun my/dotnet--nearest-csproj ()
  "return nearest .csproj above current file"
  (let* ((start (or (and buffer-file-name
                         (file-name-directory buffer-file-name))
                    default-directory))
         (dir (locate-dominating-file
               start
               (lambda (d)
                 (directory-files d nil "\\.csproj\\'" t)))))
    (when dir
      (car (directory-files dir t "\\.csproj\\'" t))))
)

(defun my/dotnet--args (command)
  "return the arguments picked in `my/dotnet-menu', if called from it"
  (let ((args (and (eq (bound-and-true-p transient-current-command) 'my/dotnet-menu)
                   (transient-args 'my/dotnet-menu))))
    ;; dotnet clean rejects --no-restore
    (if (string-prefix-p "dotnet clean" command)
        (remove "--no-restore" args)
      args))
)

(defun my/dotnet--compile (command buffer-name &optional directory)
  ;; loaded here, not at startup. it must load before the let below binds one
  ;; of its variables, or its defcustom is ignored.
  (require 'compile)
  (setq command (string-join (cons command (my/dotnet--args command)) " "))
  (let ((default-directory (or directory default-directory))
        (compilation-buffer-name-function
         (lambda (_) buffer-name)))
    (compile command))
)

;------------------------------------------------------------ build

(defun my/dotnet-build-root ()
  (interactive)
  (my/dotnet--compile "dotnet build"
                      "*Dotnet Build Root*"
                      (my/dotnet--project-root)))

(defun my/dotnet-build-project ()
  (interactive)
  (if-let ((csproj (my/dotnet--nearest-csproj)))
      (my/dotnet--compile
       (format "dotnet build %s" (shell-quote-argument csproj))
       "*Dotnet Build Project*"
       (file-name-directory csproj))
    (user-error "No .csproj found."))
)

;------------------------------------------------------------ clean

(defun my/dotnet-clean-root ()
  (interactive)
  (my/dotnet--compile "dotnet clean"
                      "*Dotnet Clean Root*"
                      (my/dotnet--project-root))
)

(defun my/dotnet-clean-project ()
  (interactive)
  (if-let ((csproj (my/dotnet--nearest-csproj)))
      (my/dotnet--compile
       (format "dotnet clean %s" (shell-quote-argument csproj))
       "*Dotnet Clean Project*"
       (file-name-directory csproj))
    (user-error "No .csproj found."))
)

;------------------------------------------------------------ test

(defun my/dotnet-test-root ()
  (interactive)
  (my/dotnet--compile "dotnet test"
                      "*Dotnet Test Root*"
                      (my/dotnet--project-root))
)

(defun my/dotnet-test-project ()
  (interactive)
  (if-let ((csproj (my/dotnet--nearest-csproj)))
      (my/dotnet--compile
       (format "dotnet test %s" (shell-quote-argument csproj))
       "*Dotnet Test Project*"
       (file-name-directory csproj))
    (user-error "No .csproj found."))
)

;------------------------------------------------------------ run

(defun my/dotnet-run-project ()
  (interactive)
  (if-let ((csproj (my/dotnet--nearest-csproj)))
      (my/dotnet--compile
       (format "dotnet run --project %s" (shell-quote-argument csproj))
       "*Dotnet Run Project*"
       (file-name-directory csproj))
    (user-error "No .csproj found."))
)

;------------------------------------------------------------ keybindings

;; the menu keeps the old key sequences, so C-c d b r still builds the root.
;; it lives in its own file so transient only loads on first use.
(autoload 'my/dotnet-menu "dotnet-menu" nil t)
(global-set-key (kbd "C-c d") #'my/dotnet-menu)

(provide 'dotnet-functions)