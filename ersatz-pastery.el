(require 'web-server)
(require 'cl)
(require 'json)
(require 'subr-x) ; (eval-when-compile (require 'subr-x)) could work too
(require 'ersatz-constants)
(require 'ersatz-paste)
(require 'ersatz-utils)

;;; TODO/FIXME duration and max_views not accounted at all
;;; TODO/FIXME wrong permissions! The pastes should be stored by key!!!

(defvar ersatz-valid-keys nil
  "Keys accepted by the current server")

(defvar ersatz-storage '())

(cl-defstruct (ersatz-answer (:constructor new-server-answer))
  (keep-alive nil :read-only t)
  (HTTP-code HTTP-ok :read-only t)
  (message "" :read-only t))

(defun ersatz-send-answer (answer process)
  (if (ersatz-answer-keep-alive answer)
      (prog1 :keep-alive
        (ws-response-header process 100))
    (let ((message (ersatz-answer-message answer)))
      (ws-response-header process (ersatz-answer-HTTP-code answer)
                          (cons "Content Type" "application/json")
                          (cons "Content-Length" (number-to-string (length message))))
      (process-send-string process message))))

;;;;;;;;
;;; POST
(defun ersatz-create-paste-id ()
  "Take the last 6 characters of the integer UNIX time"
  (let ((timestamp-as-string (number-to-string (truncate (time-to-seconds (current-time))))))
    (substring (reverse timestamp-as-string)
               0 6)))

(defun ersatz-add-max-views-or-string (headers arguments)
  "Return the max-views specified by the user as an integer, or a string if the value is not a valid non-negative integer"
  (if-let ((max-views (cdr (assoc "max_views" headers))))
      (let ((converted (ersatz-to-integer max-views)))
        (if (not converted)
            "\"max_views\" should be a non-negative integer number of views before the paste is deleted."
          (append (list :max_views converted) arguments)))))

(defun ersatz-add-duration-or-string (headers arguments)
  "Return the duration specified by the user as an integer, or a string if the value is not a valid non-negative integer"
  (if-let ((duration (cdr (assoc "duration" headers))))
      (let ((converted (ersatz-to-integer duration)))
        (if (not converted)
            "\"duration\" should be a positive integer number of minutes before the paste is deleted."
          (append (list :duration converted) arguments)))))

(defun ersatz-validate-language (user-specified-language)
  "Return the language specified by the user if valid, or \"text\""
  (or (find user-specified-language valid-languages :test 'equal)
      "text"))

(defun ersatz-add-language (headers arguments)
  "Return the arguments with the language specified by the user, or text if none"
  (let ((user-specified-language (cdr (assoc "language" headers))))
    (append (list :language (ersatz-validate-language user-specified-language))
            arguments)))

(defun ersatz-add-title (headers arguments)
  "Return a new set of arguements with the title specified by the user"
  (if-let ((title (cdr (assoc "title" headers))))
      (append (list :title title) arguments)
    arguments))

(defun ersatz-create-paste-arguments (headers)
  "Extract the arguments from the headers, returns an alist of header values or a string with an error"
  (let ((partial-headers-or-error (ersatz-add-duration-or-string
                                   headers
                                   (ersatz-add-language
                                    headers
                                    (ersatz-add-title headers nil)))))
    (if (stringp partial-headers-or-error)
        partial-headers-or-error
      (ersatz-add-max-views-or-string headers partial-headers-or-error))))

(defun ersatz-paste-from-arguments (arguments)
  (let* ((paste (apply #'new-paste arguments))
         (id (ersatz-create-paste-id))
         (alist-element (cons id paste)))
    (setq ersatz-storage (cons alist-element ersatz-storage))
    id))

(defun ersatz-paste-json-from-storage (id)
  (ersatz-paste-to-json id (cdr (assoc id ersatz-storage))))

(defun ersatz-100-continue? (headers)
  (string= "100-continue"
           (downcase
            (or (cdr (assoc :EXPECT headers)) ""))))

(defun ersatz-continue-callback (something process string))

(defun ersatz-handle-post (process path headers) 
  (let ((continue-requested (ersatz-100-continue? headers))
        (arguments-or-error (ersatz-create-paste-arguments headers)))
    (if (stringp arguments-or-error)
        (new-server-answer :HTTP-code HTTP-unprocessable-entity
                           :message (ersatz-create-json-error arguments-or-error))
      (if-let ((body (and (not (ersatz-100-continue? headers))
                          (concat (caar (last headers)) "\n"))))
          (progn
            (new-id (ersatz-paste-from-arguments (append (list :body body) arguments-or-error)))
            (new-server-answer :message (ersatz-paste-json-from-storage new-id)))
        ;; TODO/FIXME not handled
        ;; <-- add callback registration with currying here
        :keep-alive))))

;;;;;;;
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

;;;;;;;;;;
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

;;; TODO/FIXME duplicated code with the path splitting function
(defun ersatz-get-path-error (path)
  "Returns nil if the path 'looks ok' or (301 . '') if it does not"
  (let ((components (split-string path "/")))
    (unless
        (and
         (string= "" (car components))
         (string= "api" (elt components 1))
         (string= "paste" (elt components 2))
         (string= "" (car (last components)))
         (<= (length components) 5))
      (new-server-answer :HTTP-code HTTP-moved-permanently))))

(defun ersatz-get-api-key-error (headers)
  (let ((key (assoc "api_key" headers)))
    (if (not key)
        (new-server-answer :HTTP-code HTTP-unprocessable-entity
                           :message (ersatz-create-json-error "Missing keys: 'api_key'"))
      (unless (find (cdr key) ersatz-valid-keys :test 'equal )
        (new-server-answer :HTTP-code HTTP-unprocessable-entity
                           :message (ersatz-create-json-error "\"api_key\" must be a valid API key."))))))

(defun ersatz-create-response (headers)
  (or       
   (let ((get-path (alist-get ':GET headers)))
     (and get-path
          (or (ersatz-get-api-key-error headers)
              (ersatz-get-path-error get-path)
              (ersatz-validate-key-names headers '("api_key"))
              (new-server-answer :message (ersatz-handle-get get-path headers)))))
   (let ((delete-path (alist-get ':DELETE headers)))
     (and delete-path
          (or (ersatz-get-api-key-error headers)
              (ersatz-get-path-error delete-path)
              (ersatz-validate-key-names headers '("api_key"))
              (new-server-answer :message (ersatz-handle-delete delete-path)))))
   (let ((post-path (alist-get ':POST headers)))
     (and post-path
          (or (ersatz-get-api-key-error headers)
              (ersatz-get-path-error post-path)
              (ersatz-validate-key-names headers '("api_key" "title" "language" "duration" "max_views"))
              (ersatz-handle-post post-path headers))))
   (new-server-answer :HTTP-code HTTP-moved-permanently)))

(defun ersatz-handle-request (headers send-response-f)
  (funcall send-response-f (ersatz-create-response headers)))

(defun ersatz-pastery-handler (request)
  (with-slots (process headers) request
    (ersatz-handle-request headers
                           (lambda (answer)
                             (ersatz-send-answer answer process)))))

(defun stop-ersatz-server ()
  (interactive)
  (ws-stop-all))

(defun start-ersatz-server (valid-keys &optional try-kill)
  (interactive)
  (setq ersatz-valid-keys valid-keys)
  (when try-kill
    (stop-ersatz-server))
  (setq ersatz-storage '())
  (ws-start #'ersatz-pastery-handler ersatz-pastery-server-port))

(provide 'ersatz-pastery)
