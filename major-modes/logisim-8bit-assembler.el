(defvar logisim-8bit-assembler-mode-hook nil)

(defvar logisim-8bit-assembler-mode-keywords
  '("ADD" "SHR" "SHL" "NOT" "AND" "OR" "XOR" "LOAD" "STR" "DATA" "JMPA" "JMPIF" "JMPR" "CLF" "STOP"))

(defvar logisim-8bit-assembler-mode-regex-keywords
  (regexp-opt logisim-8bit-assembler-mode-keywords 'words))

(defvar logisim-8bit-assembler-mode-font-lock-keywords
  `((,logisim-8bit-assembler-mode-regex-keywords . font-lock-keyword-face)
    ("R[0-3]" . font-lock-variable-name-face)
    ("\\b[01]+\\b" . font-lock-constant-face)))

;; (defun logisim-8bit-assembler-align-instructions ()
;;   "Align instruction keywords to 4 character spaces."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward (regexp-opt logisim-8bit-assembler-mode-keywords 'words) nil t)
;;       (let ((keyword (match-string 0)))
;;         (replace-match (concat keyword (make-string (- 4 (length keyword)) ?\s)))))))

(define-derived-mode logisim-8bit-assembler-mode fundamental-mode "logisim-8bit-assembler"
  (setq font-lock-defaults '((logisim-8bit-assembler-mode-font-lock-keywords)))
  ;; (add-hook 'before-save-hook 'logisim-8bit-assembler-align-instructions nil t)
  )

(add-to-list 'auto-mode-alist '("\\.asm8\\'" . logisim-8bit-assembler-mode))

(provide 'logisim-8bit-assembler-mode)
