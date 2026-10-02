;--------------------------------------------------------------------------------
; notes-functions.el
; commands to open notes
;--------------------------------------------------------------------------------

(defun my/open-notes-elisp ()
  (interactive)
  (find-file (expand-file-name "notes/elisp-notes.org" user-emacs-directory))
)

(defun my/open-notes-cheatsheet ()
  "open the cheatsheet read-only, C-x C-q to edit it"
  (interactive)
  (find-file-read-only (expand-file-name "notes/emacs-cheatsheet.org" user-emacs-directory))
)

(defun my/open-todo ()
  "open this machine's todo list, gitignored like everything not allow-listed"
  (interactive)
  (find-file (expand-file-name "todo.org" user-emacs-directory))
  (when (= (buffer-size) 0)
    (insert "#+title: todo, " (system-name) "\n\n* TODO "))
)

(global-set-key (kbd "C-c h e") #'my/open-notes-elisp)
(global-set-key (kbd "C-c h t") #'my/open-todo)
(global-set-key (kbd "C-c h c") #'my/open-notes-cheatsheet)

(provide 'notes-functions)