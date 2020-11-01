(require 'web-server)
(require 'cl)

(defvar valid-keys '("key1" "key2"))

(defvar ersatz-storage '())

(cl-defstruct paste duration title language max_views content)

;;; POST
(defun ersatz-create-paste-id ()
  "Take the last 6 characters of the integer UNIX time"
  (let ((timestamp-as-string (number-to-string (truncate (time-to-seconds (current-time))))))
    (substring (reverse timestamp-as-string)
               0 6)))

(defun ersatz-handle-post (path headers) "POST")

;;; GET
(defun ersatz-handle-get (path headers) "GET")


;;; DELETE
(defun ersatz-delete-paste (id)
  (let ((paste (cdr (assoc id ersatz-storage))))
    (if (not paste)
        "{\"result\": \"error\", \"error_msg\": \"That paste does not belong to you.\"}"
      (assoc-delete-all id ersatz-storage)
      "{\"result\": \"success\"}")))

(defun ersatz-ensure-valid-path (path-legs)
  (unless (equal '("api" "paste") (-take 2 path-legs))
    ;;; TODO/FIXME check how pastery handles this
    (error "Invalid URL specification")))

(defun ersatz-get-paste-id (path)
  (unless (= ?/ (car (last (string-to-list path))))
    (error "Invalid path")) ;;; TODO/FIXME check how pastery handles this
  (let ((legs (-take 3 (cdr (split-string (substring path 0 (1- (length path))) "/")))))
    (ersatz-ensure-valid-path legs)
    (elt legs 2)))

(defun ersatz-handle-delete (path)
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
