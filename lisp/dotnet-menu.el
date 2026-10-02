;--------------------------------------------------------------------------------
; dotnet-menu.el
; magit-style menu for dotnet-functions.el, opened with C-c d
;--------------------------------------------------------------------------------

(require 'transient)
(require 'dotnet-functions)

(transient-define-prefix my/dotnet-menu ()
  "run dotnet commands on the solution root or a picked project"
  ["Arguments"
   ("-c" "configuration" "--configuration=" :choices ("Release" "Debug"))
   ("-n" "no restore" "--no-restore")
   ("-b" "no build (run, test)" "--no-build")
   ("-w" "hide warnings" "--property:WarningLevel=0")
   ("-p" "include pre-release packages" "--prerelease")]
  [["Build"
    ("b r" "root" my/dotnet-build-root)
    ("b p" "project" my/dotnet-build-project)]
   ["Clean"
    ("c r" "root" my/dotnet-clean-root)
    ("c p" "project" my/dotnet-clean-project)]
   ["Test"
    ("t r" "root" my/dotnet-test-root)
    ("t p" "project" my/dotnet-test-project)]
   ["Run"
    ("r p" "project" my/dotnet-run-project)]]
  [["New"
    ("n" "project or solution" my/dotnet-new-project)]
   ["References"
    ("a" "add to project" my/dotnet-add-reference)
    ("d" "remove from project" my/dotnet-remove-reference)]
   ["Packages"
    ("p" "search and add" my/dotnet-add-package)]])

(provide 'dotnet-menu)
