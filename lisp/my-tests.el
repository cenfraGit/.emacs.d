;--------------------------------------------------------------------------------
; my-tests.el
; tests for lisp/, run with:
; emacs --batch --eval "(setq load-prefer-newer t)" -L lisp -l lisp/my-tests.el -f ert-run-tests-batch-and-exit
;--------------------------------------------------------------------------------

(require 'ert)
(require 'cl-lib)
;; loaded up front so it can't replace the mocked `compile' mid-test
(require 'compile)
;; loaded up front so the let below can rebind its variables
(require 'project)
(require 'comment-functions)
(require 'dotnet-functions)

;------------------------------------------------------------ comments

(defmacro my/test--with-comment-buffer (&rest body)
  `(with-temp-buffer
     (setq-local comment-start "//")
     ,@body))

(ert-deftest my/header-comment-without-file-uses-buffer-name ()
  (my/test--with-comment-buffer
   (rename-buffer "sample" t)
   (my/insert-header-comment)
   (should (equal (nth 1 (split-string (buffer-string) "\n")) "//sample"))))

(ert-deftest my/header-comment-with-file-uses-file-name ()
  (my/test--with-comment-buffer
   (setq buffer-file-name (expand-file-name "foo.cs" temporary-file-directory))
   (rename-buffer "foo.cs<other>" t)
   (unwind-protect
       (progn
         (my/insert-header-comment)
         (should (equal (nth 1 (split-string (buffer-string) "\n")) "//foo.cs")))
     (setq buffer-file-name nil))))

(ert-deftest my/inline-comment-is-ascii-and-full-length ()
  (my/test--with-comment-buffer
   ;; an even-length word splits the dashes evenly
   (my/insert-inline-comment "ab")
   (should (= (length (buffer-string)) my/inline-comment-length))
   (should (string-match-p "\\`// -+ ab -+ //\\'" (buffer-string)))
   (should-not (string-match-p "[^[:ascii:]]" (buffer-string)))))

(ert-deftest my/line-comment-length ()
  (my/test--with-comment-buffer
   (my/insert-line-comment "word")
   (should (equal (buffer-string)
                  (concat "//" (make-string my/line-comment-length ?-) " word")))))

;------------------------------------------------------------ dotnet

(defmacro my/test--dotnet-command (menu-args &rest body)
  "run BODY as if called from the menu with MENU-ARGS, return the command"
  `(let ((transient-current-command (and ,menu-args 'my/dotnet-menu))
         (ran nil))
     (cl-letf (((symbol-function 'transient-args) (lambda (_) ,menu-args))
               ((symbol-function 'compile) (lambda (command) (setq ran command))))
       ,@body)
     ran))

(ert-deftest my/dotnet-menu-args-are-appended ()
  (should (equal (my/test--dotnet-command
                  '("--configuration=Release" "--no-restore")
                  (my/dotnet--compile "dotnet build" "*b*"))
                 "dotnet build --configuration=Release --no-restore -clp:ForceConsoleColor")))

(ert-deftest my/dotnet-clean-drops-no-restore ()
  (should (equal (my/test--dotnet-command
                  '("--configuration=Debug" "--no-restore")
                  (my/dotnet--compile "dotnet clean" "*c*"))
                 "dotnet clean --configuration=Debug -clp:ForceConsoleColor")))

(ert-deftest my/dotnet-outside-menu-adds-only-color ()
  (should (equal (my/test--dotnet-command
                  nil
                  (my/dotnet--compile "dotnet test" "*t*"))
                 "dotnet test -clp:ForceConsoleColor")))

(ert-deftest my/dotnet-new-ignores-menu-args ()
  (should (equal (my/test--dotnet-command
                  '("--configuration=Release" "--no-restore")
                  (my/dotnet--compile "dotnet new console -o App" "*n*"))
                 "dotnet new console -o App")))

(defmacro my/test--with-temp-dir (var &rest body)
  "run BODY with VAR bound to a fresh directory, deleted afterwards"
  `(let* ((,var (file-name-as-directory (make-temp-file "my-test" t)))
          ;; project.el remembers every project it sees, keep that out of the real list
          (project-list-file (expand-file-name "projects" ,var))
          (project--list 'unset))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(ert-deftest my/dotnet-new-project-adds-to-nearest-solution ()
  (my/test--with-temp-dir
   root
   (write-region "" nil (expand-file-name "Demo.slnx" root))
   (make-directory (expand-file-name "src" root))
   (let ((src (expand-file-name "src/" root)))
     (should (equal (my/test--dotnet-command nil (my/dotnet-new-project "classlib" "Lib" src))
                    (format "dotnet new classlib -o Lib && dotnet sln %s add %s"
                            (shell-quote-argument (expand-file-name "Demo.slnx" root))
                            (shell-quote-argument (expand-file-name "Lib" src))))))))

(ert-deftest my/dotnet-new-project-without-solution ()
  (my/test--with-temp-dir
   root
   (should (equal (my/test--dotnet-command nil (my/dotnet-new-project "console" "App" root))
                  "dotnet new console -o App"))))

(ert-deftest my/dotnet-new-solution-does-not-add-itself ()
  (my/test--with-temp-dir
   root
   (write-region "" nil (expand-file-name "Old.sln" root))
   (should (equal (my/test--dotnet-command nil (my/dotnet-new-project "sln" "New" root))
                  "dotnet new sln -o New"))))

(ert-deftest my/dotnet-new-project-rejects-empty-name ()
  (should-error (my/dotnet-new-project "console" "" temporary-file-directory)
                :type 'user-error))

(ert-deftest my/dotnet-pick-project-lists-csproj-and-remembers-last ()
  (my/test--with-temp-dir
   root
   (let ((default-directory root)
         (offered nil)
         (default nil))
     (call-process "git" nil nil nil "init" "-q")
     (dolist (f '("a/A.csproj" "b/B.csproj" "b/readme.md"))
       (make-directory (file-name-directory (expand-file-name f root)) t)
       (write-region "" nil (expand-file-name f root)))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt names &rest args)
                  (setq offered (sort (copy-sequence names) #'string<)
                        default (nth 4 args))
                  "b/B.csproj")))
       ;; last pick no longer exists, so no default
       (let ((my/dotnet--project-history '("gone/X.csproj")))
         (should (equal (my/dotnet--pick-project "Run: ")
                        (expand-file-name "b/B.csproj" root)))
         (should (equal offered '("a/A.csproj" "b/B.csproj")))
         (should-not default))
       (let ((my/dotnet--project-history '("a/A.csproj")))
         (my/dotnet--pick-project "Run: ")
         (should (equal default "a/A.csproj")))))))

(ert-deftest my/dotnet-build-project-uses-picked-project ()
  (let ((csproj (expand-file-name "src/App/App.csproj" temporary-file-directory))
        (dir nil)
        (buffer nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (_) (setq dir default-directory
                                 buffer (funcall compilation-buffer-name-function nil)))))
      (my/dotnet-build-project csproj))
    (should (equal dir (file-name-directory csproj)))
    (should (equal buffer "*Dotnet Build App*"))))

(ert-deftest my/dotnet-msbuild-p ()
  (should (my/dotnet--msbuild-p "dotnet build"))
  (should (my/dotnet--msbuild-p "dotnet run --project x.csproj"))
  (should-not (my/dotnet--msbuild-p "dotnet add x.csproj reference y.csproj"))
  (should-not (my/dotnet--msbuild-p "dotnet new console -o App"))
  (should-not (my/dotnet--msbuild-p "dotnet builder")))

(ert-deftest my/dotnet-add-reference-command ()
  (let ((api (expand-file-name "Api/Api.csproj" temporary-file-directory))
        (core (expand-file-name "Core/Core.csproj" temporary-file-directory))
        (data (expand-file-name "Data/Data.csproj" temporary-file-directory)))
    ;; menu args and the color flag would make dotnet add fail
    (should (equal (my/test--dotnet-command
                    '("--configuration=Release" "--no-restore")
                    (my/dotnet-add-reference api (list core data)))
                   (format "dotnet add %s reference %s %s"
                           (shell-quote-argument api)
                           (shell-quote-argument core)
                           (shell-quote-argument data))))))

(ert-deftest my/dotnet-pick-several-projects-excludes-target ()
  (my/test--with-temp-dir
   root
   (let ((default-directory root)
         (offered nil))
     (call-process "git" nil nil nil "init" "-q")
     (dolist (f '("Api/Api.csproj" "Core/Core.csproj" "Data/Data.csproj"))
       (make-directory (file-name-directory (expand-file-name f root)) t)
       (write-region "" nil (expand-file-name f root)))
     (let ((api (expand-file-name "Api/Api.csproj" root)))
       (cl-letf (((symbol-function 'completing-read-multiple)
                  (lambda (_prompt names &rest _)
                    (setq offered (sort (copy-sequence names) #'string<))
                    '("Core/Core.csproj" "Data/Data.csproj"))))
         (should (equal (my/dotnet--pick-project "Refs: " t api)
                        (list (expand-file-name "Core/Core.csproj" root)
                              (expand-file-name "Data/Data.csproj" root))))
         (should (equal offered '("Core/Core.csproj" "Data/Data.csproj"))))
       (cl-letf (((symbol-function 'completing-read-multiple) (lambda (&rest _) nil)))
         (should-error (my/dotnet--pick-project "Refs: " t api) :type 'user-error))))))

(ert-deftest my/dotnet-references-reads-project-references-only ()
  (my/test--with-temp-dir
   root
   (let ((csproj (expand-file-name "Api.csproj" root)))
     (write-region "<Project Sdk=\"Microsoft.NET.Sdk\">
  <ItemGroup>
    <PackageReference Include=\"Newtonsoft.Json\" Version=\"13.0.3\" />
    <ProjectReference Include=\"..\\Core\\Core.csproj\" />
    <ProjectReference Condition=\"'$(X)' == 'y'\"
                      Include=\"..\\Data\\Data.csproj\">
      <Private>false</Private>
    </ProjectReference>
  </ItemGroup>
</Project>
" nil csproj)
     (should (equal (my/dotnet--references csproj)
                    '("..\\Core\\Core.csproj" "..\\Data\\Data.csproj"))))))

(ert-deftest my/dotnet-remove-reference-command ()
  (let ((api (expand-file-name "Api/Api.csproj" temporary-file-directory))
        (dir nil)
        (ran nil))
    (cl-letf (((symbol-function 'compile)
               (lambda (command) (setq ran command dir default-directory))))
      (my/dotnet-remove-reference api '("..\\Core\\Core.csproj")))
    (should (equal ran (format "dotnet remove %s reference %s"
                               (shell-quote-argument api)
                               (shell-quote-argument "..\\Core\\Core.csproj"))))
    ;; the stored paths are relative to the project, so it must run from there
    (should (equal dir (file-name-directory api)))))

(ert-deftest my/dotnet-remove-reference-without-references ()
  (my/test--with-temp-dir
   root
   (let ((csproj (expand-file-name "Api.csproj" root)))
     (write-region "<Project Sdk=\"Microsoft.NET.Sdk\" />\n" nil csproj)
     (cl-letf (((symbol-function 'my/dotnet--pick-project) (lambda (&rest _) csproj)))
       (should-error (call-interactively 'my/dotnet-remove-reference)
                     :type 'user-error)))))

(ert-deftest my/dotnet-prerelease-only-reaches-add-package ()
  (let ((menu '("--configuration=Release" "--no-restore" "--prerelease")))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet add a.csproj package Serilog" "*p*"))
                   "dotnet add a.csproj package Serilog --prerelease"))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet build" "*b*"))
                   "dotnet build --configuration=Release --no-restore -clp:ForceConsoleColor"))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet clean" "*c*"))
                   "dotnet clean --configuration=Release -clp:ForceConsoleColor"))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet add a.csproj reference b.csproj" "*r*"))
                   "dotnet add a.csproj reference b.csproj"))))

(defconst my/test--search-json
  "{\"version\": 2, \"problems\": [], \"searchResult\": [
     {\"sourceName\": \"offline\", \"packages\": [
       {\"id\": \"Serilog\", \"latestVersion\": \"2.0.0\", \"totalDownloads\": 10}]},
     {\"sourceName\": \"nuget.org\", \"packages\": [
       {\"id\": \"Serilog\", \"latestVersion\": \"4.4.0\", \"totalDownloads\": 3324144730},
       {\"id\": \"Serilog.Sinks.File\", \"latestVersion\": \"7.0.0\", \"totalDownloads\": 1239526774}]}]}")

(ert-deftest my/dotnet-search-packages-merges-sources ()
  (let (args)
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _dest _display &rest rest)
                 (setq args rest)
                 (insert my/test--search-json)
                 0)))
      ;; first source wins for a package listed twice
      (should (equal (my/dotnet--search-packages "serilog" nil)
                     '(("Serilog" . "2.0.0  10 downloads")
                       ("Serilog.Sinks.File" . "7.0.0  1.2G downloads"))))
      (should-not (member "--prerelease" args))
      (my/dotnet--search-packages "serilog" t)
      (should (member "--prerelease" args)))))

(ert-deftest my/dotnet-search-packages-failure ()
  (cl-letf (((symbol-function 'call-process)
             (lambda (&rest _) (insert "error: no network") 1)))
    (should-error (my/dotnet--search-packages "serilog" nil) :type 'user-error)))

(ert-deftest my/dotnet-add-package-rejects-empty-search ()
  (cl-letf (((symbol-function 'my/dotnet--pick-project) (lambda (&rest _) "a.csproj"))
            ((symbol-function 'read-string) (lambda (&rest _) "")))
    (should-error (call-interactively 'my/dotnet-add-package) :type 'user-error)))

(ert-deftest my/dotnet-no-build-and-warning-level-per-command ()
  (let ((menu '("--no-build" "--property:WarningLevel=0")))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet run --project a.csproj" "*r*"))
                   "dotnet run --project a.csproj --no-build --property:WarningLevel=0 -clp:ForceConsoleColor"))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet test" "*t*"))
                   "dotnet test --no-build --property:WarningLevel=0 -clp:ForceConsoleColor"))
    ;; dotnet build and clean reject --no-build
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet build" "*b*"))
                   "dotnet build --property:WarningLevel=0 -clp:ForceConsoleColor"))
    (should (equal (my/test--dotnet-command menu (my/dotnet--compile "dotnet clean" "*c*"))
                   "dotnet clean -clp:ForceConsoleColor"))))

(ert-deftest my/todo-starts-new-file-and-keeps-existing ()
  (require 'notes-functions)
  (my/test--with-temp-dir
   dir
   (let ((user-emacs-directory dir))
     (my/open-todo)
     (unwind-protect
         (progn
           ;; the folder exists, the file doesn't yet
           (should (file-equal-p (file-name-directory buffer-file-name) dir))
           (should (equal (file-name-nondirectory buffer-file-name) "todo.org"))
           (should (equal (buffer-string) (concat "#+title: todo, " (system-name) "\n\n* TODO ")))
           (should (eobp))
           ;; nothing is written until saved
           (should-not (file-exists-p buffer-file-name)))
       (set-buffer-modified-p nil)
       (kill-buffer))
     (write-region "* TODO mine\n" nil (expand-file-name "todo.org" dir))
     (my/open-todo)
     (unwind-protect (should (equal (buffer-string) "* TODO mine\n"))
       (kill-buffer)))))

(ert-deftest my/cheatsheet-opens-read-only ()
  (require 'notes-functions)
  (my/open-notes-cheatsheet)
  (unwind-protect
      (progn
        (should (string-suffix-p "emacs-cheatsheet.org" buffer-file-name))
        (should buffer-read-only))
    (kill-buffer)))

;------------------------------------------------------------ folding

(require 'csharp-functions)

(defconst my/test--regions "class A
{
    #region Outer
    int a;
    #region Inner
    int b;
    #endregion
    int c;
    #endregion
    int d;
}
")

(defun my/test--visible ()
  "buffer text as displayed, with folded regions replaced"
  (let ((text (buffer-string)))
    (dolist (ov (sort (overlays-in (point-min) (point-max))
                      (lambda (a b) (> (overlay-start a) (overlay-start b)))))
      (when (overlay-get ov 'my/region)
        (setq text (concat (substring text 0 (1- (overlay-start ov)))
                           (overlay-get ov 'display)
                           (substring text (1- (overlay-end ov)))))))
    text))

(ert-deftest my/fold-region-skips-nested-and-unfolds ()
  (with-temp-buffer
    (insert my/test--regions)
    (goto-char (point-min))
    (search-forward "#region Outer")
    (my/toggle-fold)
    (should (equal (my/test--visible) "class A
{
    #region Outer...
    int d;
}
"))
    (my/toggle-fold)
    (should (equal (my/test--visible) my/test--regions))))

(ert-deftest my/fold-inner-region-only ()
  (with-temp-buffer
    (insert my/test--regions)
    (goto-char (point-min))
    (search-forward "#region Inner")
    (beginning-of-line)
    (my/toggle-fold)
    (should (string-match-p "#region Inner\\.\\.\\.\n    int c;" (my/test--visible)))))

(ert-deftest my/fold-region-without-end ()
  (with-temp-buffer
    (insert "#region Lonely\nint a;\n")
    (goto-char (point-min))
    (should-error (my/toggle-fold) :type 'user-error)))

(ert-deftest my/fold-elsewhere-uses-hideshow ()
  (with-temp-buffer
    (insert my/test--regions)
    (goto-char (point-min))
    (search-forward "int a;")
    (let ((called nil))
      (cl-letf (((symbol-function 'hs-toggle-hiding) (lambda () (setq called t))))
        (my/toggle-fold))
      (should called))))

;------------------------------------------------------------ home

(require 'home-functions)

(defmacro my/test--with-home-files (projects recent &rest body)
  "run BODY with PROJECTS and RECENT written to save files in a temp dir"
  `(my/test--with-temp-dir
    dir
    (let ((user-emacs-directory dir))
      (with-temp-file project-list-file
        (prin1 (mapcar #'list ,projects) (current-buffer)))
      (with-temp-file (expand-file-name "recentf" dir)
        (insert ";;; generated\n\n(setq recentf-list\n      '(\n")
        (dolist (f ,recent) (insert (format "        %S\n" f)))
        (insert "        ))\n\n(setq recentf-filter-changer-current 'nil)\n"))
      ,@body)))

(defun my/test--home-buttons ()
  (let (labels)
    (save-excursion
      (goto-char (point-min))
      (while (forward-button 1 nil nil t)
        (push (button-label (button-at (point))) labels)))
    (nreverse labels)))

(ert-deftest my/home-lists-projects-and-recent-files ()
  (my/test--with-home-files
   '("c:/Repos/Api/" "c:/Repos/Web/") '("c:/Repos/Api/Program.cs" "c:/notes.org")
   (with-current-buffer (my/home)
     (unwind-protect
         (progn
           ;; todo first, always
           (should (equal (my/test--home-buttons)
                          (list (abbreviate-file-name (expand-file-name "todo.org" dir))
                                "c:/Repos/Api/" "c:/Repos/Web/" "c:/Repos/Api/Program.cs" "c:/notes.org")))
           (should buffer-read-only)
           (should-not (string-match-p "\\`emacs" (buffer-string)))
           (let (opened switched todo)
             (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f)))
                       ((symbol-function 'dired) (lambda (d) (setq switched d)))
                       ((symbol-function 'my/open-todo) (lambda () (setq todo t))))
               ;; point starts on the todo button
               (push-button)
               (my/home--jump "Projects")
               (push-button)
               (my/home--jump "Recent files")
               (push-button))
             (should todo)
             (should (equal switched "c:/Repos/Api/"))
             (should (equal opened "c:/Repos/Api/Program.cs"))))
       (kill-buffer)))))

(ert-deftest my/home-startup-keeps-a-file-from-the-command-line ()
  (my/test--with-home-files
   nil nil
   (let ((file-buffer (find-file-noselect (expand-file-name "recentf" dir))))
     (unwind-protect (should (eq (my/home-startup) file-buffer))
       (kill-buffer file-buffer)))
   (let ((home (my/home-startup)))
     (unwind-protect
         (progn
           (should (equal (buffer-name home) my/home-buffer-name))
           (should-not (buffer-local-value 'display-line-numbers-mode home)))
       (kill-buffer home)))))

(ert-deftest my/home-on-new-tabs ()
  (should (eq tab-bar-new-tab-choice #'my/home))
  (let ((buffer (funcall tab-bar-new-tab-choice)))
    (unwind-protect (should (equal (buffer-name buffer) my/home-buffer-name))
      (kill-buffer buffer))))

(ert-deftest my/home-shows-at-most-ten-recent-files ()
  (my/test--with-home-files
   nil (mapcar (lambda (n) (format "c:/f%d.cs" n)) (number-sequence 1 15))
   (with-current-buffer (my/home)
     (unwind-protect (should (= (length (my/test--home-buttons)) (1+ my/home-recent-count)))
       (kill-buffer)))))

(ert-deftest my/home-empty-and-broken-files ()
  (my/test--with-home-files
   nil nil
   (write-region "(setq recentf-list '(\"unterminated" nil (expand-file-name "recentf" dir))
   (should-not (my/home--recent-files))
   (with-current-buffer (my/home)
     (unwind-protect
         (progn
           ;; only the todo entry, which shows even without a file
           (should (= (length (my/test--home-buttons)) 1))
           (should (= (how-many "none yet" (point-min) (point-max)) 2)))
       (kill-buffer)))))

(provide 'my-tests)
