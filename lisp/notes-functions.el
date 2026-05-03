;--------------------------------------------------------------------------------
; notes-functions.el
; commands to open notes
;--------------------------------------------------------------------------------

(defun my/open-notes-elisp ()
  (interactive)
  (find-file (expand-file-name "notes/elisp-notes.org" user-emacs-directory))
)

(global-set-key (kbd "C-c h e") #'my/open-notes-elisp)

(provide 'notes-functions)