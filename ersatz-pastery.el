(require 'web-server)
(require 'cl)
(require 'json)

(defvar valid-keys '("key1" "key2"))

(defvar ersatz-storage '())

;;; Paste definition
(cl-defstruct (paste (:constructor new-paste))
  (duration 43200 :read-only t)
  (title "" :read-only t)
  (language nil :read-only t)
  (max_views 0 :read-only t)
  (body "" :read-only t))

(defun ersatz-paste-to-table (id paste)
  (let ((table (make-hash-table)))
    (puthash "id" id table)
    (puthash "title" (paste-title paste) table)
    (puthash "url" (format "https://www.pastery.net/%s/" id) table) ; TODO/FIXME mock part
    (puthash "language" (paste-language paste) table)
    (puthash "duration" (paste-duration paste) table)
    table))

(defun ersatz-create-pastes-list (id-list)
  (--map (ersatz-paste-to-table (car it) (cdr it))
         (--filter (cdr it)
                   (--map (cons it (alist-get it ersatz-storage))
                          id-list))))

(defun ersatz-pastes-to-json (id-list)
  "Returns a JSON representation of the valid pastes in the storage with the specified ID

Invalid ID are silently discarded"
  (let ((table (make-hash-table))
        (pastes (or (ersatz-create-pastes-list id-list) [])))
    (puthash "pastes" pastes table)
    (json-encode table)))

(defun ersatz-storage-to-json ()
  "Returns a JSON representation of the whole storage"
  (ersatz-pastes-to-json (--map (car it) ersatz-storage)))

;;; Generic utility functions
(defun ersatz-path-surrounded-with-/? (path)
  (let ((characters (string-to-list path)))
   (and (= ?/ (car characters))
        (= ?/ (car (last characters))))))

(defun ersatz-remove-first-last-characters (path)
  (substring path 1 (1- (length path))))

(defun ersatz-get-path-components (path)
  "Returns a list with the path components or nil if the path is not valid"
  (when (ersatz-path-surrounded-with-/? path)
    (let* ((path-without-trailing-/ (ersatz-remove-first-last-characters path))
           (path-components (split-string path-without-trailing-/ "/"))
           (interesting-components (-take 3 path-components)))
      (when (equal '("api" "paste") (-take 2 interesting-components))
        interesting-components))))

(defun ersatz-get-paste-id (path)
  "Returns nil if the path is invalid, or string with the ID (empty string meaning \"no ID present\")"
  (when-let ((path-components (ersatz-get-path-components path)))
    (or (elt path-components 2) "")))

;;; POST
(defun ersatz-create-paste-id ()
  "Take the last 6 characters of the integer UNIX time"
  (let ((timestamp-as-string (number-to-string (truncate (time-to-seconds (current-time))))))
    (substring (reverse timestamp-as-string)
               0 6)))

(defun ersatz-handle-post (path headers) "POST")

;;; GET
(defun ersatz-handle-get-paste (id)
  (ersatz-pastes-to-json (list id)))

(defun ersatz-handle-get-list ()
  (ersatz-storage-to-json))

(defun ersatz-handle-get (path headers)
  (if-let (id (ersatz-get-paste-id path))
      (if (string-empty-p id)
          (ersatz-handle-get-list)
        (ersatz-handle-get-paste id))
      ;;; TODO/FIXME handle this 301 no content
    (error "INVALID SOMETHING. Handle this")))

;;; DELETE
(defun ersatz-delete-paste (id)
  (let ((paste (cdr (assoc id ersatz-storage))))
    (if (not paste)
        "{\"result\": \"error\", \"error_msg\": \"That paste does not belong to you.\"}"
      (setq ersatz-storage (assoc-delete-all id ersatz-storage))
      "{\"result\": \"success\"}")))

(defun ersatz-handle-delete (path)
  ;;; TODO/FIXME handle the 301 case
  (ersatz-delete-paste (ersatz-get-paste-id path)))

(defun read-sample (name)
  (with-temp-buffer
    (insert-file-contents (format "test-data/%s" name))
    (forward-line)
    (buffer-substring-no-properties (point) (point-max))))

(defun ersatz-get-api-key-error (headers)
  (let ((key (assoc "api_key" headers)))
    (if (not key)
        (read-sample "missing_api_key.txt")
        (if (find key valid-keys :test 'equal )
            (read-sample "invalid_api_key.txt")))))

(defun ersatz-handle-request (process headers)
  (or (ersatz-get-api-key-error headers)
      (let ((get-path (alist-get ':GET headers)))
        (and get-path (ersatz-handle-get get-path headers)))
      (let ((delete-path (alist-get ':DELETE headers)))
        (and delete-path (ersatz-handle-delete delete-path)))
      (let ((post-path (alist-post ':POST headers)))
        (and post-path (ersatz-handle-post post-path headers)))))

(defun ersatz-pastery-handler (request)
  (with-slots (process headers) request
    (let ((code-content (ersatz-handle-request process headers)))
      (ws-response-header process 200
                          '("Content Type" . "application/json")
                          (cons "Content-Length" (number-to-string (length code-content))))
      (process-send-string process code-content))))

(defun stop-ersatz-server ()
  (interactive)
  (ws-stop-all))

(defun start-ersatz-server (&optional try-kill)
  (interactive)
  (when try-kill
    (stop-ersatz-server))
  (setq ersatz-storage '())
  (ws-start #'ersatz-pastery-handler 8080))

(defmacro with-debug-server (&rest forms)
  (let ((result (make-symbol "result")))
    `(progn
       (start-ersatz-server t)
       (let ((,result (progn ,@forms)))
         (stop-ersatz-server)
         ,result))))

(provide 'esatz-pastery)
