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
    (if-let ((file (buffer-file-name)))
        (file-name-nondirectory (directory-file-name (file-name-directory file)))
      "unknown")))

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

(defun hackatime-save-current-state ()
  "Save current buffer state and return line changes compared to previous version."
  (when (and (buffer-file-name) (buffer-live-p (current-buffer)))
    (let* ((hackatime-dir (expand-file-name "emacs-hackatime-cache" "~"))
           (filename (file-name-nondirectory (buffer-file-name))) 
           (timestamp (format-time-string "%Y%m%d-%H%M%S"))
           (save-filename (format "%s-%s" timestamp filename)) 
           (save-path (expand-file-name save-filename hackatime-dir)))
      
      (unless (file-exists-p hackatime-dir)
        (make-directory hackatime-dir t)
        (hackatime-message "Created cache directory: %s" hackatime-dir))
      
      (let ((line-changes (hackatime-get-line-changes filename hackatime-dir)))
        
        (condition-case err
            (progn
              (write-region (point-min) (point-max) save-path nil 'quiet)
              (hackatime-message "Saved state to: %s" save-path))
          (error (hackatime-message "Failed to save state: %s" err)))
        
        (hackatime-clean-old-files hackatime-dir filename)
        
        line-changes))))

(defun hackatime-clean-old-files (&optional hackatime-dir current-filename)
  "Clean up old cache files, keeping only the most recent version of each file."
  (let* ((dir (or hackatime-dir (expand-file-name "emacs-hackatime-cache" "~")))
         (filename (or current-filename 
                      (when (buffer-file-name)
                        (file-name-nondirectory (buffer-file-name)))))
         (current-time (float-time))
         (24-hours-ago (- current-time 86400))
         (all-files (when (file-exists-p dir)
                      (directory-files dir t "^[^.]"))))
    
    (when all-files
      (when filename
        (let* ((current-file-files (cl-remove-if-not 
                                    (lambda (file) 
                                      (string-match-p (concat ".*-" (regexp-quote filename) "$") file))
                                    all-files))
               (sorted-current-files (sort current-file-files #'file-newer-than-file-p)))
          (when (> (length sorted-current-files) 2)
            (dolist (file (nthcdr 2 sorted-current-files))
              (delete-file file)
              (hackatime-message "Deleted old version: %s" (file-name-nondirectory file))))))

      (dolist (file all-files)
        (when (and (file-regular-p file)
                   (file-exists-p file)  
                   (> (- current-time (float-time (nth 5 (file-attributes file)))) 24-hours-ago))
          (delete-file file)
          (hackatime-message "Deleted old file: %s" (file-name-nondirectory file)))))))

(defun hackatime-get-line-changes (&optional filename hackatime-dir)
  "Calculate line additions and deletions by comparing with previous version."
  (let* ((current-filename (or filename 
                              (when (buffer-file-name)
                                (file-name-nondirectory (buffer-file-name)))))
         (dir (or hackatime-dir (expand-file-name "emacs-hackatime-cache" "~")))
         (files (when (and current-filename (file-exists-p dir))
                  (cl-remove-if-not 
                   (lambda (file) 
                     (string-match-p (concat ".*-" (regexp-quote current-filename) "$") file))
                   (directory-files dir t "^[^.]"))))
         (sorted-files (sort files #'file-newer-than-file-p))
         (additions 0)
         (deletions 0))
    
    (hackatime-message "Found %d cache files for %s" (length sorted-files) current-filename)
    
    (when (and files (>= (length sorted-files) 1))
      (let* ((previous-file (car sorted-files))
             (temp-current-file (make-temp-file "hackatime-current-" nil 
                                               (concat "-" current-filename))))
        
        (unwind-protect
            (progn
              (write-region (point-min) (point-max) temp-current-file nil 'quiet)
              
              (let ((diff-output (shell-command-to-string
                                 (format "diff -u %s %s 2>/dev/null || true"
                                         (shell-quote-argument previous-file)
                                         (shell-quote-argument temp-current-file)))))
                
                (hackatime-message "Diff output length: %d chars" (length diff-output))
                
                (dolist (line (split-string diff-output "\n"))
                  (cond
                   ((string-match-p "^\\+[^+]" line)
                    (setq additions (1+ additions)))
                   ((string-match-p "^-[^-]" line)
                    (setq deletions (1+ deletions))))))
              
              (hackatime-message "Calculated changes: +%d -%d" additions deletions))
          
          (when (file-exists-p temp-current-file)
            (delete-file temp-current-file)))))
    
    (cons additions deletions)))

(provide 'utils)
