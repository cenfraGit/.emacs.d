;--------------------------------------------------------------------------------
; dotnet-menu.el
; magit-style menu for dotnet-functions.el, opened with C-c d
;--------------------------------------------------------------------------------

(require 'transient)
(require 'dotnet-functions)

(transient-define-prefix my/dotnet-menu ()
  "run dotnet commands on the solution root or the nearest project"
  ["Arguments"
   ("-c" "configuration" "--configuration=" :choices ("Release" "Debug"))
   ("-n" "no restore" "--no-restore")]
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
    ("r" "project" my/dotnet-run-project)]])

(provide 'dotnet-menu)
