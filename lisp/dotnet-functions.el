;--------------------------------------------------------------------------------
; dotnet-functions.el
; functions related with dotnet cli
;--------------------------------------------------------------------------------

(require 'project)
(require 'compile)

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

(defun my/dotnet--compile (command buffer-name &optional directory)
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

(global-set-key (kbd "C-c d b r") #'my/dotnet-build-root)
(global-set-key (kbd "C-c d b p") #'my/dotnet-build-project)
(global-set-key (kbd "C-c d c r") #'my/dotnet-clean-root)
(global-set-key (kbd "C-c d c p") #'my/dotnet-clean-project)
(global-set-key (kbd "C-c d t r") #'my/dotnet-test-root)
(global-set-key (kbd "C-c d t p") #'my/dotnet-test-project)
(global-set-key (kbd "C-c d r")   #'my/dotnet-run-project)

(provide 'dotnet-functions)