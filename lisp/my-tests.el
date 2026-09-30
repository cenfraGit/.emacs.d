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

(provide 'my-tests)
