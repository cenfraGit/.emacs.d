
;; --------------------------------------------------------------------------------
;; comment functions
;; --------------------------------------------------------------------------------

(defvar my/inline-comment-length 52)
(defvar my/block-comment-length 80)

(defun my/insert-inline-comment (prefix suffix word)
  (let* ((word-length (length word))
         (dashes-length (- my/inline-comment-length
                           (length prefix)
                           (length suffix)
                           word-length
                           2)) ;; spaces around word
         (dashes-half (/ dashes-length 2))
         (dashes (make-string dashes-half ?-))
         (comment (concat prefix dashes " " word " " dashes suffix)))
    (insert comment)))

(defun my/insert-inline-comment-based-on-mode (word)
  (interactive "sWord: ")
  (cond
   ((derived-mode-p 'python-mode) (my/insert-inline-comment "# " " #" word))
   ((derived-mode-p 'c-mode) (my/insert-inline-comment "/* " " */" word))
   ((derived-mode-p 'c++-mode) (my/insert-inline-comment "/* " " */" word))
   ((derived-mode-p 'csharp-mode) (my/insert-inline-comment "// " " //" word))
   ((derived-mode-p 'emacs-lisp-mode) (my/insert-inline-comment ";; " " ;;" word))
   ((derived-mode-p 'latex-mode) (my/insert-inline-comment "% " " %" word))
   (t (my/insert-inline-comment "// " " //" word))))

(defun my/insert-block-comment (word)
  (interactive "sWord: ")
  (let* ((line (make-string my/block-comment-length ?-))
         (prefix (cond
                  ((derived-mode-p 'python-mode) "# ")
                  ((derived-mode-p 'c-mode) "// ")
                  ((derived-mode-p 'c++-mode) "// ")
                  ((derived-mode-p 'csharp-mode) "// ")
                  ((derived-mode-p 'emacs-lisp-mode) ";; ")
                  ((derived-mode-p 'latex-mode) "% ")
                  (t "// "))))
    (insert (concat prefix line "\n"
                    prefix word "\n"
                    prefix line))))

;; --------------------------------------------------------------------------------
;; latex functions
;; --------------------------------------------------------------------------------

(defun latex-codeblock ()
  (interactive)
  (let ((language (read-string "Language: "))
        (number (read-string "Line number: ")))
    (insert (format "\\begin{codeblock}{%s}{%s}\n\n\\end{codeblock}" language number))
    (forward-line -1)))

(defun latex-enumerate ()
  (interactive)
  (insert "\\begin{enumerate}\n\\item \n\\end{enumerate}")
  (forward-line -1)
  (end-of-line))

(defun latex-itemize ()
  (interactive)
  (insert "\\begin{itemize}\n\\item \n\\end{itemize}")
  (forward-line -1)
  (end-of-line))
