; --------------------------------------------------------------------------------
; comment-functions.el
; --------------------------------------------------------------------------------

; ------------------------------------------------------------ variables

(defvar my/header-comment-length 80)
(defvar my/block-comment-length 80)
(defvar my/line-comment-length 60)
(defvar my/inline-comment-length 52)

; ------------------------------------------------------------ internals

(defun my/internal-get-comment-prefix ()
  "returns current mode comment prefix"
  (or comment-start "//")
)

(defun my/internal-get-comment-line (length)
  "returns a line with the comment prefix"
  (concat (my/internal-get-comment-prefix) (make-string length ?-))
)

; ------------------------------------------------------------ header

(defun my/insert-header-comment ()
  (interactive)
  (let ((line-comment (my/internal-get-comment-line my/header-comment-length)))
    (goto-char (point-min))
    (insert line-comment)
    (insert "\n")
    (insert (concat (my/internal-get-comment-prefix) (file-name-nondirectory (buffer-file-name))))
    (insert "\n")
    (insert line-comment)
  )
)

; ------------------------------------------------------------ block

(defun my/insert-block-comment (word)
  (interactive "sWord: ")
  (let ((line-comment (my/internal-get-comment-line my/header-comment-length)))
    (insert line-comment)
    (insert "\n")
    (insert (concat (my/internal-get-comment-prefix) word))
    (insert "\n")
    (insert line-comment)
  )
)

; ------------------------------------------------------------ line

(defun my/insert-line-comment (word)
  (interactive "sWord: ")
  (let ((line-comment (my/internal-get-comment-line my/line-comment-length)))
    (insert (concat line-comment " " word))
  )
)

; ------------------------------------------------------------ inline

(defun my/insert-inline-comment (word)
  (interactive "sWord: ")
  (let* ( (word-length (length word))
          (comment-symbol (my/internal-get-comment-prefix))
          (comment-symbol-length (length comment-symbol))
          (dashes-length (- my/inline-comment-length
                            4 ; spaces around word and after/before dashes
                            (* comment-symbol-length 2) ; at start and end
                            word-length))
          (dashes-half (/ dashes-length 2))
          (dashes (make-string dashes-half ?─))
          (comment (concat comment-symbol " " dashes " " word " " dashes " " comment-symbol)))
    (insert comment)
  )
)

;------------------------------------------------------------ keybindings

(global-set-key (kbd "C-c c l") #'my/insert-line-comment)
(global-set-key (kbd "C-c c r") #'my/insert-block-comment)
(global-set-key (kbd "C-c c h") #'my/insert-header-comment)
(global-set-key (kbd "C-c c i") #'my/insert-inline-comment)

(provide 'comment-functions)