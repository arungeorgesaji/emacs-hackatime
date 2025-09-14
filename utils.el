;;; utils.el --- Utility functions for HackaTime Extension -*- lexical-binding: t; -*-

(defun hackatime-get-lines ()
  (if (buffer-live-p (current-buffer))
      (count-lines (point-min) (point-max))
    0))

(defun hackatime-get-cursor-pos ()
  (if (buffer-live-p (current-buffer))
      (point)
    1))

(defun hackatime-get-machine-id ()
  (cond
   ((file-exists-p "/etc/machine-id")
    (string-trim (with-temp-buffer
                   (insert-file-contents "/etc/machine-id")
                   (buffer-string))))
   ((eq system-type 'darwin)
    (string-trim (shell-command-to-string "ioreg -d2 -c IOPlatformExpertDevice | awk -F\\\" '/IOPlatformUUID/{print $(NF-1)}'")))
   ((eq system-type 'windows-nt)
    (string-trim (shell-command-to-string "wmic csproduct get UUID /format:value | findstr UUID | cut -d= -f2")))
   (t "unknown")))

(defun hackatime-get-time ()
  (string-to-number (format-time-string "%s")))

(defun hackatime-get-category ()
  "coding") 

(defun hackatime-get-entity ()
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (buffer-name)))

(defconst hackatime-lang-map
  '(("python-mode" . "python")
    ("js-mode" . "javascript")
    ("html-mode" . "html")
    ("css-mode" . "css")
    ("java-mode" . "java")
    ("c++-mode" . "cpp")
    ("c-mode" . "c")
    ("go-mode" . "go")
    ("ruby-mode" . "ruby")
    ("php-mode" . "php")
    ("swift-mode" . "swift")
    ("typescript-mode" . "typescript")
    ("markdown-mode" . "markdown")
    ("sh-mode" . "bash")
    ("json-mode" . "json")
    ("yaml-mode" . "yaml")
    ("toml-mode" . "toml")
    ("ini-mode" . "ini")
    ("text-mode" . "text")
    ("fundamental-mode" . "text")
    ("xml-mode" . "xml")
    ("csv-mode" . "csv")
    ("tsv-mode" . "tsv")
    ("log-mode" . "text")
    ("mdx-mode" . "markdown")))

(defun hackatime-get-language ()
  (let ((mode-name (symbol-name major-mode)))
    (or (cdr (assoc mode-name hackatime-lang-map))
        "unknown")))

(defun hackatime-get-type ()
  (hackatime-get-category))

(defun hackatime-get-user-agent ()
  "emacs-hackatime/1.0")

(defun hackatime-get-editor ()
  "Emacs")

(defun hackatime-get-project ()
  (if-let ((root (vc-root-dir)))
      (file-name-nondirectory (directory-file-name root))
    "unknown"))

(defun hackatime-get-project-root-count ()
  (if-let ((root (vc-root-dir)))
      (let* ((submodules (split-string
                          (shell-command-to-string
                           (format "git -C %s submodule status --quiet | wc -l" root))
                          "\n" t)))
        (+ 1 (string-to-number (car submodules))))
    1))

(defun hackatime-get-is-write ()
  (if (and (buffer-live-p (current-buffer)) (buffer-modified-p))
      t
    nil))

(defun hackatime-get-operating-system ()
  (pcase system-type
    ('gnu/linux "linux")
    ('darwin "darwin")
    ('windows-nt "windows")
    ('ms-dos "ms-dos")
    ('cygwin "cygwin")
    (_ "unknown")))

(defun hackatime-get-lineno ()
  (if (buffer-live-p (current-buffer))
      (line-number-at-pos)
    1))

(defvar hackatime-default-branch "main") 

(defun hackatime-get-branch ()
  (if-let ((root (vc-root-dir)))
      (let ((branch (string-trim
                     (shell-command-to-string
                      (format "git -C %s rev-parse --abbrev-ref HEAD" root)))))
        (if (or (null branch) (string-empty-p branch))
            hackatime-default-branch
          branch))
    hackatime-default-branch))

(provide 'utils)
