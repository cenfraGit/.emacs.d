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

(defun my/dotnet--msbuild-p (command)
  "non-nil if COMMAND builds, so it takes msbuild options"
  (string-match-p "\\`dotnet \\(build\\|clean\\|test\\|run\\)\\_>" command)
)

(defun my/dotnet--args (command)
  "return the arguments picked in `my/dotnet-menu', if called from it"
  (let ((args (and (eq (bound-and-true-p transient-current-command) 'my/dotnet-menu)
                   (transient-args 'my/dotnet-menu))))
    (cond
     ;; dotnet new, add, sln take neither option
     ((not (my/dotnet--msbuild-p command)) nil)
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

(defvar my/dotnet--project-history nil)

(defun my/dotnet--pick-project (prompt &optional multiple exclude)
  "pick a .csproj in the current project, defaulting to the last one picked.
with MULTIPLE, pick several (comma separated) and return a list.
EXCLUDE is a .csproj left out of the choices."
  (let* ((project (project-current t))
         (root (project-root project))
         (names (mapcar (lambda (f) (file-relative-name f root))
                        (seq-filter (lambda (f) (and (string-suffix-p ".csproj" f)
                                                     (not (equal f exclude))))
                                    (project-files project))))
         (last (car (member (car my/dotnet--project-history) names))))
    (unless names (user-error "No .csproj found in %s" root))
    (if multiple
        (or (mapcar (lambda (name) (expand-file-name name root))
                    (completing-read-multiple prompt names nil t))
            (user-error "No project picked"))
      (expand-file-name
       (completing-read prompt names nil t nil 'my/dotnet--project-history last)
       root)))
)

(defun my/dotnet--compile (command buffer-name &optional directory)
  ;; loaded here, not at startup. it must load before the let below binds one
  ;; of its variables, or its defcustom is ignored.
  (require 'compile)
  (setq command (string-join (cons command (my/dotnet--args command)) " "))
  ;; msbuild drops colors when output is piped, as it is in a compilation buffer
  (when (my/dotnet--msbuild-p command)
    (setq command (concat command " -clp:ForceConsoleColor")))
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

(defun my/dotnet-build-project (csproj)
  "pick a project and build it"
  (interactive (list (my/dotnet--pick-project "Build project: ")))
  (my/dotnet--compile
   (format "dotnet build %s" (shell-quote-argument csproj))
   (format "*Dotnet Build %s*" (file-name-base csproj))
   (file-name-directory csproj))
)

;------------------------------------------------------------ clean

(defun my/dotnet-clean-root ()
  (interactive)
  (my/dotnet--compile "dotnet clean"
                      "*Dotnet Clean Root*"
                      (my/dotnet--project-root))
)

(defun my/dotnet-clean-project (csproj)
  "pick a project and clean it"
  (interactive (list (my/dotnet--pick-project "Clean project: ")))
  (my/dotnet--compile
   (format "dotnet clean %s" (shell-quote-argument csproj))
   (format "*Dotnet Clean %s*" (file-name-base csproj))
   (file-name-directory csproj))
)

;------------------------------------------------------------ test

(defun my/dotnet-test-root ()
  (interactive)
  (my/dotnet--compile "dotnet test"
                      "*Dotnet Test Root*"
                      (my/dotnet--project-root))
)

(defun my/dotnet-test-project (csproj)
  "pick a project and test it"
  (interactive (list (my/dotnet--pick-project "Test project: ")))
  (my/dotnet--compile
   (format "dotnet test %s" (shell-quote-argument csproj))
   (format "*Dotnet Test %s*" (file-name-base csproj))
   (file-name-directory csproj))
)

;------------------------------------------------------------ run

(defun my/dotnet-run-project (csproj)
  "pick a project and run it"
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

;------------------------------------------------------------ references

(defun my/dotnet-add-reference (csproj references)
  "make CSPROJ reference each project in REFERENCES"
  (interactive
   (let ((csproj (my/dotnet--pick-project "Add reference to project: ")))
     (list csproj
           (my/dotnet--pick-project (format "%s references (comma separated): "
                                            (file-name-base csproj))
                                    t csproj))))
  (my/dotnet--compile
   (format "dotnet add %s reference %s"
           (shell-quote-argument csproj)
           (mapconcat #'shell-quote-argument references " "))
   (format "*Dotnet Reference %s*" (file-name-base csproj))
   (file-name-directory csproj))
)

(defun my/dotnet--references (csproj)
  "return the project references in CSPROJ, as written in the file"
  (with-temp-buffer
    (insert-file-contents csproj)
    (let (references)
      (while (re-search-forward "<ProjectReference[^>]*Include=\"\\([^\"]+\\)\"" nil t)
        (push (match-string 1) references))
      (nreverse references)))
)

(defun my/dotnet-remove-reference (csproj references)
  "remove each of REFERENCES, as written in CSPROJ, from CSPROJ"
  (interactive
   (let* ((csproj (my/dotnet--pick-project "Remove reference from project: "))
          (name (file-name-base csproj))
          (existing (or (my/dotnet--references csproj)
                        (user-error "%s has no project references" name))))
     (list csproj
           (or (completing-read-multiple
                (format "Remove from %s (comma separated): " name) existing nil t)
               (user-error "No reference picked")))))
  ;; the paths are relative to the .csproj, and dotnet compile runs from there
  (my/dotnet--compile
   (format "dotnet remove %s reference %s"
           (shell-quote-argument csproj)
           (mapconcat #'shell-quote-argument references " "))
   (format "*Dotnet Reference %s*" (file-name-base csproj))
   (file-name-directory csproj))
)

;------------------------------------------------------------ keybindings

;; the menu lives in its own file so transient only loads on first use
(autoload 'my/dotnet-menu "dotnet-menu" nil t)
(global-set-key (kbd "C-c d") #'my/dotnet-menu)

(provide 'dotnet-functions)