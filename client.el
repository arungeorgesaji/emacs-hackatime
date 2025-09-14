;;; client.el --- HackaTime Emacs API client -*- lexical-binding: t; -*-

;; Author: Arun George Saji
;; Package-Requires: ((emacs "30.2"))
;; Keywords: hackatime, tracking
;; URL: https://github.com/arungeorgesaji/emacs-hackatime

(require 'json)
(require 'url)
(require 'utils)

(defgroup hackatime nil
  "Customization group for HackaTime tracking in Emacs.")

(defcustom hackatime-debug-mode nil
  "Display debug messages for HackaTime tracking."
  :type 'boolean
  :group 'hackatime)

(defcustom hackatime-heartbeat-interval 5
  "Interval in seconds between heartbeats."
  :type 'integer
  :group 'hackatime)

(defcustom hackatime-api-url "https://hackatime.hackclub.com/api/hackatime/v1/users/current/heartbeats.bulk"
  "API URL for HackaTime heartbeat submissions."
  :type 'string
  :group 'hackatime)

(defcustom hackatime-api-key ""
  "API key for HackaTime authentication."
  :type 'string
  :group 'hackatime)

(defcustom hackatime-flush-interval 30
  "Time interval in seconds between automatic heartbeat flushes."
  :type 'integer
  :group 'hackatime)

(defconst hackatime-heartbeats-file
  (expand-file-name "heartbeats.json" user-emacs-directory))

(defvar hackatime-heartbeats '())

(defvar hackatime-last-flush-time (float-time))

(defvar hackatime-heartbeat-timer nil)

(defvar hackatime-flush-timer nil)

(defun hackatime-message (format-string &rest args)
  (when hackatime-debug-mode
    (apply 'message (concat "[HackaTime] " format-string) args)))

(defun hackatime-load-heartbeats ()
  (when (file-exists-p hackatime-heartbeats-file)
    (with-temp-buffer
      (insert-file-contents hackatime-heartbeats-file)
      (let ((data (ignore-errors (json-read-from-string (buffer-string)))))
        (setq hackatime-heartbeats
              (cond
               ((vectorp data) (append data nil))  
               ((listp data) data)
               (t '())))
        (hackatime-message "Loaded %d pending heartbeats from file" 
                           (length hackatime-heartbeats))))))

(defun hackatime-save-heartbeats ()
  (when hackatime-heartbeats
    (with-temp-file hackatime-heartbeats-file
      (insert (json-encode hackatime-heartbeats)))
    (hackatime-message "Saved %d pending heartbeats" 
                       (length hackatime-heartbeats))))

(defun hackatime-create-heartbeat ()
  (let ((lines (hackatime-get-lines))
        (cursor (hackatime-get-cursor-pos))
        (machine-id (hackatime-get-machine-id))
        (timestamp (hackatime-get-time))
        (category (hackatime-get-category))
        (entity (hackatime-get-entity))
        (language (hackatime-get-language))
        (type (hackatime-get-type))
        (is-write (hackatime-get-is-write))
        (project (hackatime-get-project))
        (project-root-count (hackatime-get-project-root-count))
        (os (hackatime-get-operating-system))
        (lineno (hackatime-get-lineno))
        (branch (hackatime-get-branch))
        (editor (hackatime-get-editor))
        (user-agent (hackatime-get-user-agent)))
    
    (hackatime-message "Heartbeat data: lines=%s cursor=%s machine=%s time=%s category=%s entity=%s lang=%s type=%s write=%s project=%s roots=%s os=%s line=%s branch=%s editor=%s agent=%s"
                       lines cursor machine-id timestamp category entity language type 
                       is-write project project-root-count 
                       os lineno branch editor user-agent)
    
    `(("lines" . ,lines)
      ("cursorpos" . ,cursor)
      ("machine_name_id" . ,machine-id)
      ("time" . ,timestamp)
      ("category" . ,category)
      ("entity" . ,entity)
      ("language" . ,language)
      ("type" . ,type)
      ("is_write" . ,is-write)
      ("project" . ,project)
      ("project_root_count" . ,project-root-count)
      ("operating_system" . ,os)
      ("lineno" . ,lineno)
      ("branch" . ,branch)
      ("editor" . ,editor)
      ("user_agent" . ,user-agent))))

(defun hackatime-add-heartbeat ()
  (let ((hb (hackatime-create-heartbeat)))
    (push hb hackatime-heartbeats)
    (hackatime-save-heartbeats)))

(defun hackatime-flush ()
  (when hackatime-heartbeats
    (let ((payload (json-encode (reverse hackatime-heartbeats)))
          (heartbeat-count (length hackatime-heartbeats)))
      (condition-case err
          (let ((url-request-method "POST")
                (url-request-extra-headers `(("Authorization" . ,(concat "Basic " (base64-encode-string hackatime-api-key t)))
                                             ("Content-Type" . "application/json")
                                             ("User-Agent" . "emacs-hackatime/1.0")))
                (url-request-data payload))
            (url-retrieve
             hackatime-api-url
             (lambda (status)
               (condition-case callback-err
                   (if (plist-get status :error)
                       (hackatime-message "Failed to send heartbeat: %s" (plist-get status :error))
                     (hackatime-message "Sent %d heartbeats" heartbeat-count))
                 (error (hackatime-message "Error in callback: %s" callback-err)))
               (setq hackatime-heartbeats '())
               (hackatime-save-heartbeats)
               (setq hackatime-last-flush-time (float-time)))))
        (error (hackatime-message "Exception sending heartbeats: %s" err))))))

(defun hackatime-start-tracking ()
  (when hackatime-api-key
    (setq hackatime-heartbeat-timer
          (run-with-timer 0 hackatime-heartbeat-interval #'hackatime-add-heartbeat))
    
    (setq hackatime-flush-timer
          (run-with-timer 0 hackatime-flush-interval #'hackatime-flush))
    
    (hackatime-message "Tracking started - heartbeats every %ds, flush every %ds" 
                       hackatime-heartbeat-interval hackatime-flush-interval)))

(defun hackatime-stop-tracking ()
  (when hackatime-heartbeat-timer
    (cancel-timer hackatime-heartbeat-timer)
    (setq hackatime-heartbeat-timer nil))
  
  (when hackatime-flush-timer
    (cancel-timer hackatime-flush-timer)
    (setq hackatime-flush-timer nil))
  
  (hackatime-flush)
  (hackatime-message "Tracking stopped"))

(if (and hackatime-api-key (not (string-empty-p hackatime-api-key)))
    (progn
      (hackatime-message "API key detected, auto-starting tracking...")
      (hackatime-load-heartbeats)
      (hackatime-start-tracking))
  (message "API key missing! Please set `hackatime-api-key` to enable tracking."))

(provide 'client)
