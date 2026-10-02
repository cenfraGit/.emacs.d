;--------------------------------------------------------------------------------
; csharp-functions.el
; file template creation for c# related projects
;--------------------------------------------------------------------------------

;------------------------------------------------------------ internal

(defun my/wpf--project-root ()
  "return current project root"
  ;; project-current first: it autoloads project.el, which defines project-root
  (let ((project (project-current t)))
    (project-root project))
)

(defun my/wpf--default-namespace ()
  "guess namespace from project root and current directory"
  (let* ((root (my/wpf--project-root))
         (base (file-name-base (directory-file-name root)))
         (relative (file-relative-name default-directory root))
         (relative (directory-file-name relative)))
    (if (string= relative ".")
        base
      (concat base "."
              (replace-regexp-in-string
               "/" "."
               (replace-regexp-in-string "\\\\" "." relative)))))
)

(defun my/wpf--xaml-template (kind namespace name)
  "return xaml template for KIND, NAMESPACE, and NAME"
  (pcase kind
    ("Window"
     (format
      "<Window x:Class=\"%s.%s\"\n\
        xmlns=\"http://schemas.microsoft.com/winfx/2006/xaml/presentation\"\n\
        xmlns:x=\"http://schemas.microsoft.com/winfx/2006/xaml\"\n\
        Title=\"%s\" Height=\"450\" Width=\"800\">\n\
    <DockPanel>\n\
        \n\
    </DockPanel>\n\
</Window>\n"
      namespace name name))
    ("UserControl"
     (format
      "<UserControl x:Class=\"%s.%s\"\n\
             xmlns=\"http://schemas.microsoft.com/winfx/2006/xaml/presentation\"\n\
             xmlns:x=\"http://schemas.microsoft.com/winfx/2006/xaml\">\n\
    <DockPanel>\n\
        \n\
    </DockPanel>\n\
</UserControl>\n"
      namespace name)))
)

(defun my/wpf--code-behind-template (kind namespace name)
  "return c# code behind template for KIND, NAMESPACE, and NAME"
  (format
   "namespace %s;\n\n\
public partial class %s : %s\n\
{\n\
    public %s()\n\
    {\n\
        InitializeComponent();\n\
    }\n\
}\n"
   namespace name kind name)
)

;------------------------------------------------------------ wpf

(defun my/wpf-create-xaml-file (kind name namespace)
  "create WPF KIND files for NAME using NAMESPACE
   KIND should be either Window or UserControl"
  (interactive
   (let* ((kind (completing-read "Kind: " '("Window" "UserControl") nil t))
          (name (read-string (format "%s name: " kind)))
          (namespace (read-string "Namespace: " (my/wpf--default-namespace))))
     (list kind name namespace)))
  (let ((xaml-file (concat name ".xaml"))
        (code-file (concat name ".xaml.cs")))
    (when (or (file-exists-p xaml-file)
              (file-exists-p code-file))
      (user-error "One or both files already exist"))
    (find-file xaml-file)
    (insert (my/wpf--xaml-template kind namespace name))
    (save-buffer)
    (find-file code-file)
    (insert (my/wpf--code-behind-template kind namespace name))
    (save-buffer)
    (message "Created %s and %s" xaml-file code-file))
)

;------------------------------------------------------------ folding

(defun my/region-end ()
  "return the end of the #endregion line matching the #region on this line"
  (save-excursion
    (let ((depth 1))
      (end-of-line)
      (while (and (> depth 0)
                  (re-search-forward "^[ \t]*#\\(end\\)?region\\_>" nil t))
        (setq depth (if (match-beginning 1) (1- depth) (1+ depth))))
      (if (= depth 0)
          (line-end-position)
        (user-error "No matching #endregion"))))
)

(defun my/toggle-fold ()
  "fold the #region on this line, otherwise the block at point like hs-toggle-hiding"
  (interactive)
  (if (not (save-excursion (beginning-of-line) (looking-at-p "[ \t]*#region\\_>")))
      (hs-toggle-hiding)
    (let* ((start (line-end-position))
           (folded (seq-find (lambda (ov) (overlay-get ov 'my/region))
                             (overlays-at start))))
      (if folded
          (delete-overlay folded)
        (let ((ov (make-overlay start (my/region-end))))
          (overlay-put ov 'my/region t)
          (overlay-put ov 'display "...")
          ;; removed if the folded text gets deleted
          (overlay-put ov 'evaporate t)))))
)

(provide 'csharp-functions)
