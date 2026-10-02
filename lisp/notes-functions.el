;--------------------------------------------------------------------------------
; notes-functions.el
; commands to open notes
;--------------------------------------------------------------------------------

(defun my/open-notes-elisp ()
  (interactive)
  (find-file (expand-file-name "notes/elisp-notes.org" user-emacs-directory))
)

(defun my/open-notes-cheatsheet ()
  (interactive)
  (find-file (expand-file-name "notes/emacs-cheatsheet.org" user-emacs-directory))
)

(global-set-key (kbd "C-c h e") #'my/open-notes-elisp)
(global-set-key (kbd "C-c h c") #'my/open-notes-cheatsheet)

(provide 'notes-functions)