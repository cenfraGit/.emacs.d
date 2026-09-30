;--------------------------------------------------------------------------------
; my-tests.el
; tests for lisp/, run with:
; emacs --batch --eval "(setq load-prefer-newer t)" -L lisp -l lisp/my-tests.el -f ert-run-tests-batch-and-exit
;--------------------------------------------------------------------------------

(require 'ert)
(require 'cl-lib)
;; loaded up front so it can't replace the mocked `compile' mid-test
(require 'compile)
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
                 "dotnet build --configuration=Release --no-restore")))

(ert-deftest my/dotnet-clean-drops-no-restore ()
  (should (equal (my/test--dotnet-command
                  '("--configuration=Debug" "--no-restore")
                  (my/dotnet--compile "dotnet clean" "*c*"))
                 "dotnet clean --configuration=Debug")))

(ert-deftest my/dotnet-outside-menu-adds-nothing ()
  (should (equal (my/test--dotnet-command
                  nil
                  (my/dotnet--compile "dotnet test" "*t*"))
                 "dotnet test")))

(provide 'my-tests)
