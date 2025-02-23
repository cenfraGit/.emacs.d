(defun insert-comment (prefix suffix word total-length)
  (interactive "sPrefix: \nsSuffix: \nsWord: \nnTotal Length: ")
  (let* ((word-length (length word))
         (dashes-length (- total-length
                           (length prefix)
                           (length suffix)
                           word-length))
         (dashes-half (/ dashes-length 2))
         (dashes (make-string dashes-half ?-))
         (comment (concat prefix dashes " " word " " dashes suffix)))
    (insert comment)))

(defun insert-python-comment (word)
  (interactive "sWord: ")
  (insert-comment "# " " #" word 42))

(defun insert-c-comment (word)
  (interactive "sWord: ")
  (insert-comment "/* " " */" word 42))

(defun insert-latex-comment (word)
  (interactive "sWord: ")
  (insert-comment "% " " %" word 42))

(defun insert-elisp-comment (word)
  (interactive "sWord: ")
  (insert-comment ";; " " ;;" word 42))

(defun insert-comment-based-on-mode (word)
  (interactive "sWord: ")
  (cond
   ((derived-mode-p 'python-mode) (insert-python-comment word))
   ((derived-mode-p 'c-mode) (insert-c-comment word))
   ((derived-mode-p 'c++-mode) (insert-c-comment word))
   ((derived-mode-p 'latex-mode) (insert-latex-comment word))
   ((derived-mode-p 'emacs-lisp-mode) (insert-elisp-comment word))
   (t (insert-c-comment word))))

(defun insert-text-comment (word)
  (interactive "sWord: ")
  (let* ((total-length 60)
         (line (make-string total-length ?-))
         (prefix (cond ((derived-mode-p 'python-mode) "# ")
                       ((or (derived-mode-p 'c-mode) (derived-mode-p 'c++-mode)) "// ")
                       ((derived-mode-p 'emacs-lisp-mode) ";; ")
                       ((derived-mode-p 'latex-mode) "% ")
                       (t ""))))
    (insert (concat prefix line "\n"
                    prefix word "\n"
                    prefix line
		    ))))
