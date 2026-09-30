;--------------------------------------------------------------------------------
; dotnet-functions.el
; functions related with dotnet cli
;--------------------------------------------------------------------------------

;------------------------------------------------------------ internals

(defun my/dotnet--project-root ()
  "return current project root"
  ;; project-current first: it autoloads project.el, which defines project-root
  (let ((project (project-current t)))
    (project-root project))
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
    (cond
     ;; dotnet new takes neither option
     ((string-prefix-p "dotnet new" command) nil)
     ;; dotnet clean rejects --no-restore
     ((string-prefix-p "dotnet clean" command) (remove "--no-restore" args))
     (t args)))
)

(defun my/dotnet--root-or-here ()
  "return current project root, or default-directory outside a project"
  (if-let ((project (project-current)))
      (project-root project)
    default-directory)
)

(defvar my/dotnet--run-history nil)

(defun my/dotnet--pick-project (prompt)
  "pick a .csproj in the current project, defaulting to the last one picked"
  (let* ((project (project-current t))
         (root (project-root project))
         (names (mapcar (lambda (f) (file-relative-name f root))
                        (seq-filter (lambda (f) (string-suffix-p ".csproj" f))
                                    (project-files project))))
         (last (car (member (car my/dotnet--run-history) names))))
    (unless names (user-error "No .csproj found in %s" root))
    (expand-file-name
     (completing-read prompt names nil t nil 'my/dotnet--run-history last)
     root))
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

;------------------------------------------------------------ run from solution

(defun my/dotnet-run-solution-project (csproj)
  "pick any project in the solution and run it"
  (interactive (list (my/dotnet--pick-project "Run project: ")))
  (my/dotnet--compile
   (format "dotnet run --project %s" (shell-quote-argument csproj))
   ;; one buffer per project, so several can run at once
   (format "*Dotnet Run %s*" (file-name-base csproj))
   (file-name-directory csproj))
)

;------------------------------------------------------------ new

(defvar my/dotnet-templates
  '("console" "classlib" "wpf" "wpflib" "winforms" "avalonia.app" "avalonia.mvvm"
    "xunit" "nunit" "mstest" "webapi" "web" "mvc" "blazor" "worker" "sln")
  "suggestions for `my/dotnet-new-project', any other template name also works")

(defun my/dotnet-new-project (template name directory)
  "create NAME from TEMPLATE inside DIRECTORY and add it to the nearest solution"
  (interactive
   (list (completing-read "Template: " my/dotnet-templates)
         (read-string "Name: ")
         (read-directory-name "In directory: " (my/dotnet--root-or-here))))
  (when (string-empty-p name) (user-error "Name is empty"))
  (let* ((sln-dir (locate-dominating-file
                   directory
                   (lambda (d) (directory-files d nil "\\.slnx?\\'" t))))
         (sln (and sln-dir (car (directory-files sln-dir t "\\.slnx?\\'" t))))
         (command (format "dotnet new %s -o %s" template (shell-quote-argument name))))
    (when (and sln (not (equal template "sln")))
      (setq command (format "%s && dotnet sln %s add %s"
                            command
                            (shell-quote-argument sln)
                            (shell-quote-argument (expand-file-name name directory)))))
    (my/dotnet--compile command (format "*Dotnet New %s*" name) directory))
)

;------------------------------------------------------------ keybindings

;; the menu keeps the old key sequences, so C-c d b r still builds the root.
;; it lives in its own file so transient only loads on first use.
(autoload 'my/dotnet-menu "dotnet-menu" nil t)
(global-set-key (kbd "C-c d") #'my/dotnet-menu)

(provide 'dotnet-functions)